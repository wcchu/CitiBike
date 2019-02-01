suppressPackageStartupMessages(library("tidyverse"))

d <- read.csv("citibike_2014-07.csv", stringsAsFactors = F)

## Task: predict the user (Subscriber/customer, birth year, gender) and the trip
## (duration, top time, end station) for a given starting station at a given time.

## 1. predict the user

## preprocess data
d1 <-
  d %>%
  ## filter out users with missing gender or birth year
  filter(gender %in% c(1, 2), birth.year != "\\N") %>%
  ## round birth year to decade (not trunc)
  mutate(b_year = round((as.numeric(birth.year)-1900.0)/10.0)) %>%
  select(lat = start.station.latitude,
         lon = start.station.longitude,
         time = starttime,
         usertype,
         b_year,
         gender)
