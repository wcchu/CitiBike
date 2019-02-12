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
colnames_feat <- c("lat", "lon", "month", "wday", "hour")

## create function to split training and test set
split_train_test <- function(d, t = 0.2) {
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
  )

## split training and test sets with 80%-20%
d_usertype_s <- split_train_test(d_usertype)

## (1) random forest
rf_usertype <-
  randomForest(
    x = d_usertype_s$train[, colnames_feat],
    y = as.factor(d_usertype_s$train[, "usertype"]),
    xtest = d_usertype_s$test[, colnames_feat],
    ytest = as.factor(d_usertype_s$test[, "usertype"])
  )
print(rf_usertype) ## error rate ~ 31%


## (2) tensorflow

usertype_classifier <-
  linear_classifier(
    feature_columns = feature_columns(
      column_numeric("lat", "lon", "month", "wday", "hour"))
  )

train_and_evaluate(
  usertype_classifier,
  input_fn(usertype ~ lat + lon + month + wday + hour,
           data = d_usertype_s$train,
           batch_size = 32,
           epochs = 3))

predictions <-
  predict(
    usertype_classifier,
    input_fn = input_fn(usertype ~ lat + lon + month + wday + hour,
                        data = d_usertype_s$test,
                        batch_size = 32,
                        epochs = 1),
    predict_keys = "logistic")


## 2. predict the gender

## filter out users with missing gender
#d_gender <- d %>% filter(gender %in% c(1, 2))


## 3. predict the birth year

## filterout users with missing birth year
#d_byear <- d %>% filter(byear != "\\N") %>% mutate(byear = as.integer(byear))
