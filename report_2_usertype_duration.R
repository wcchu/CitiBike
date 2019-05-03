suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(tfestimators))
suppressPackageStartupMessages(library(tree))
suppressPackageStartupMessages(library(pre))

raw <- read.csv("citibike_2014-07.csv", stringsAsFactors = F)

## Task: predict the usertype (Subscriber/customer classification) and
## the trip duration (regression) for a given starting station at a given time.

## preprocess data
d <-
  raw %>%
  ## change time string to POSIX time
  mutate(time = as.POSIXct(starttime, tz = "EST"),
         trip_dur = tripduration/60) %>%
  mutate(wday = as.integer(format(time, "%w")),
         hour = as.integer(format(time, "%H"))) %>%
  select(lat = start.station.latitude,
         lon = start.station.longitude,
         wday,
         hour,
         usertype,
         trip_dur) %>%
  mutate(usertype = as.factor(usertype))
## Now this dataset has 5 predictor features: lat, lon, day, and hour.
## The responses usertype, b_year, and gender will be predicted independently from
## the 5 features.
feat_names <- c("lat", "lon", "wday", "hour")

## for tensorflow
feat_cols <- feature_columns(
  column_numeric("lat", "lon", "wday", "hour"))

## function to split training and test set
splitter <- function(d, t = 0.2) {
  # d is the input data
  # t is the proportion of test (0 < t < 1)
  if (t <= 0 | t >= 1) {
    stop("0 < t < 1 not satisfied")
  }
  n <- nrow(d)
  s <- sample(x = n, size = as.integer(0.2 * n), replace = F)
  list(
    train = d[-s, ],
    test = d[s, ]
  )
}

## function to sample max of N rows from data frame
sampler <- function(d, m = 10000) {
  n <- nrow(d)
  if (m >= n) {
    warning(sprintf("data size %d smaller than desired size, return original", n))
    return(d)
  }
  s <- sample(x = n, size = m, replace = F)
  d[s, ]
}


## 1. predict the usertype (classification)

## sample subscribers to the same amount of customers to balance the data
d_customers <- d[d$usertype == "Customer", ]
d_subscribers <- d[d$usertype == "Subscriber", ]
u <-
  rbind(
    ## reduce either d_customers or d_subscribers so their sizes are the same
    d_customers %>% sampler(m = nrow(d_subscribers)),
    d_subscribers %>% sampler(m = nrow(d_customers))
  ) %>%
  mutate(usertype_int = ifelse(usertype == "Subscriber", 1, 0)) %>%
  ## reduce data size for development
  sampler(m = 100000) %>%
  ## split training and test sets with 80%-20%
  splitter(t = 0.2)

## (1) random forest
rf <-
  randomForest(
    x = u$train[, feat_names],
    y = u$train[, "usertype"],
    xtest = u$test[, feat_names],
    ytest = as.factor(u$test[, "usertype"])
  )
print(rf) ## error rate ~ 32%


## (2) tensorflow linear classifier

input <- function(d) {
  input_fn(usertype_int ~ lat + lon + wday + hour,
           data = d,
           batch_size = 100,
           epochs = 3)
}

lin_cl <- linear_classifier(feature_columns = feat_cols)

train(lin_cl, input_fn = input(u$train))

lin_cl_eval <- evaluate(lin_cl, input_fn = input(u$test))
print(lin_cl_eval) ## average loss ~ 69%


## (3) tensorflow dnn classifier

dnn_cl <- dnn_classifier(
  hidden_units = c(5, 5, 5),
  feature_columns = feat_cols)

train(dnn_cl, input_fn = input(u$train))

dnn_cl_eval <- evaluate(dnn_cl, input_fn = input(u$test))
print(dnn_cl_eval) ## average loss ~ 69%


## (4) pre (rulefit) classifier

pre_cl <- pre(usertype ~ lat + lon + wday + hour,
              data = u$train,
              family = "binomial",
              ntrees = 5)

pre_cl_pred <- predict(object = pre_cl,
                       newdata = u$test,
                       type = "class")
pre_cl_err <-
  u$test %>%
  mutate(prediction = pre_cl_pred) %>%
  mutate(error = ifelse(usertype == prediction, 0, 1))
print(mean(pre_cl_err$error)) ## average loss ~ 37%

## 2. predict the trip duration (regression)

## (1) tree regression

tree_rg <- tree(
  formula = trip_dur ~ lat + lon + wday + hour,
  data = u$train)

tree_rg_pred <- predict(
  object = tree_rg,
  newdata = u$test,
  type = "vector")

tree_rg_loss <- mean(abs(u$test$trip_dur - tree_rg_pred))
print(tree_rg_loss) ## loss ~ 11.2

## (2) linear regression

lm_rg <- lm(
  formula = trip_dur ~ lat + lon + wday + hour,
  data = u$train)

lm_rg_pred <- predict(
  object = lm_rg,
  newdata = u$test)

lm_rg_loss <- mean(abs(u$test$trip_dur - lm_rg_pred))
print(lm_rg_loss) ## loss ~ 11.3


## (3) tensorflow linear regression

input <- function(d) {
  input_fn(trip_dur ~ lat + lon + wday + hour,
           data = d,
           batch_size = 100,
           epochs = 3)
}

lin_rg <- linear_regressor(feature_columns = feat_cols)

train(lin_rg, input_fn = input(u$train))

lin_rg_eval <- evaluate(lin_rg, input_fn = input(u$test))
print(lin_rg_eval)


## (4) tensorflow dnn regressor

dnn_rg <- dnn_regressor(
  hidden_units = c(5, 5, 5),
  feature_columns = feat_cols)

train(dnn_rg, input_fn = input(u$train))

dnn_rg_eval <- evaluate(dnn_rg, input_fn = input(u$test))
print(dnn_rg_eval)
