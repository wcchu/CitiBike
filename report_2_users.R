suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(tfestimators))

raw <- read.csv("citibike_2014-07.csv", stringsAsFactors = F)

## Task: predict the user (Subscriber/customer, birth year, gender) and the trip
## (duration, top time, end station) for a given starting station at a given time.

## preprocess data
d <-
  raw %>%
  ## change time string to POSIX time
  mutate(time = as.POSIXct(starttime, tz = "EST")) %>%
  mutate(month = as.integer(format(time, "%m")),
         wday = as.integer(format(time, "%w")),
         hour = as.integer(format(time, "%H"))) %>%
  select(lat = start.station.latitude,
         lon = start.station.longitude,
         month,
         wday,
         hour,
         usertype,
         byear = birth.year,
         gender) %>%
  mutate(usertype = as.factor(usertype),
         gender = as.factor(gender))
## Now this dataset has 5 predictor features: lat, lon, month, day, and hour.
## The responses usertype, b_year, and gender will be predicted independently from
## the 5 features.
feat_names <- c("lat", "lon", "month", "wday", "hour")

## for tensorflow
feat_cols <- feature_columns(
  column_numeric("lat", "lon", "month", "wday", "hour"))

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
d_usertype <-
  rbind(
    d_customers,
    d_subscribers[sample(x = nrow(d_subscribers),
                         size = nrow(d_customers),
                         replace = F), ]
  ) %>%
  mutate(usertype = ifelse(usertype == "Subscriber", 1, 0)) %>%
  sampler(m = 50000) ## reduce data size for development

## split training and test sets with 80%-20%
d_usertype_s <- splitter(d_usertype)

## (1) random forest
rf_usertype <-
  randomForest(
    x = d_usertype_s$train[, feat_names],
    y = as.factor(d_usertype_s$train[, "usertype"]),
    xtest = d_usertype_s$test[, feat_names],
    ytest = as.factor(d_usertype_s$test[, "usertype"])
  )
print(rf_usertype) ## error rate ~ 31%


## (2) tensorflow linear classifier

usertype_input_fn <- function(d) {
  input_fn(usertype ~ lat + lon + month + wday + hour,
           data = d,
           batch_size = 100,
           epochs = 3)
}

usertype_classifier <- linear_classifier(feature_columns = feat_cols)

train(usertype_classifier,
      input_fn = usertype_input_fn(d_usertype_s$train))

predictions <- predict(usertype_classifier,
                       input_fn = usertype_input_fn(d_usertype_s$test))
evaluation <- evaluate(usertype_classifier,
                       input_fn = usertype_input_fn(d_usertype_s$test))

## 2. predict the gender

## filter out users with missing gender
#d_gender <- d %>% filter(gender %in% c(1, 2))


## 3. predict the birth year

## filterout users with missing birth year
#d_byear <- d %>% filter(byear != "\\N") %>% mutate(byear = as.integer(byear))
