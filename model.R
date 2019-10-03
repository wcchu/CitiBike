suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(tree))
suppressPackageStartupMessages(library(pre))
suppressPackageStartupMessages(library(neuralnet))

raw <- read.csv(unz("citibike_2014-07.csv.zip", "citibike_2014-07.csv"),
                 header = T, stringsAsFactors = F)

## Task: predict the usertype (Subscriber/customer classification) and
## the trip duration (regression) for a given starting location and time.

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
## Now this dataset has 4 predictor features: lat, lon, day, and hour.
## The responses usertype and trip_dur will be predicted independently from
## the 4 features.
feat_names <- c("lat", "lon", "wday", "hour")

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


## (2) pre (rulefit) classifier
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

# scale tar based on the mean and scale of ref
mean_sd_scale <- function(ref, tar) {
  centers <- apply(ref, 2, mean)
  scales <- apply(ref, 2, sd)
  scale(tar, center = centers, scale = scales)
}

v <- d %>% sampler(m = 100000) %>% splitter(t = 0.2)

# take train dataset feature cols before scaling as scaler ref
ref_feat <- v$train[feat_names]
# scale train and test
train <-
  cbind(
    mean_sd_scale(ref = ref_feat, tar = ref_feat),
    v$train['trip_dur']
  )
test <-
  cbind(
    mean_sd_scale(ref = ref_feat, tar = v$test[feat_names]),
    v$test['trip_dur']
  )

## (1) tree regression

tree_rg <- tree(
  formula = trip_dur ~ lat + lon + wday + hour,
  data = train)

tree_rg_pred <- predict(
  object = tree_rg,
  newdata = test,
  type = "vector")

tree_rg_loss <- mean(abs(test$trip_dur - tree_rg_pred))
print(tree_rg_loss) ## loss ~ 8

## (2) linear regression

lm_rg <- lm(
  formula = trip_dur ~ lat + lon + wday + hour,
  data = train)

lm_rg_pred <- predict(
  object = lm_rg,
  newdata = test)

lm_rg_loss <- mean(abs(test$trip_dur - lm_rg_pred))
print(lm_rg_loss) ## loss ~ 8

## (3) neuralnet

nn_rg <- neuralnet(
  formula = trip_dur ~ lat + lon + wday + hour,
  data = train,
  hidden = c(5, 3)
)
plot(nn_rg, rep = 'best')

nn_rg_pred <- predict(nn_rg, newdata = test[feat_names])

nn_rg_loss <- mean(abs(test$trip_dur - nn_rg_pred))
print(nn_rg_loss)
