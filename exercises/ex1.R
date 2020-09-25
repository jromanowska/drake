# EXERCISE 1
#
# TASK: 
# Below is a "standard" messy analysis.
# Please, clean it and rewrite so that every part of the analysis is done by a function.
# Keep this values as function parameters:
# input_file <- 'data/Metro_Interstate_Traffic_Volume.csv'
# gam_k <- -1

library(dplyr)
library(purrr)

# Read Traffic Volume dataset ---------------------------------------------
input_file <- 'data/Metro_Interstate_Traffic_Volume.csv'
read_d <- function(input_file){
  data <- read.csv(input_file)
  return(data)
}

# Preprocess data ---------------------------------------------------------
preproc_d <- function(d){
  data <- d %>%
    mutate(date = as.Date(date_time),
           hour = lubridate::hour(date_time))
  
  # data %>%
    # filter(holiday != 'None')
  
  data <- data %>%
    group_by(date) %>%
    mutate(day_type = case_when(any(holiday != 'None') ~ 'Holiday',
                                lubridate::wday(date, week_start = 1) >= 6 ~ 'Weekend',
                                T  ~ 'Weekday')) %>%
    ungroup()
  
  data <- data %>%
    select(day_type, temp, hour, traffic_volume)
  
  data$day_type <- as.factor(data$day_type)
  
  # summary(data)
  
  data <- data %>% filter(temp > 0)
  return(data)
}
# Fit model ---------------------------------------------------------------
fit_and_test <- function(data, gam_k){
  library(mgcv)
  
  model <- gam(traffic_volume ~ s(hour, k = gam_k) + day_type + temp, data = data)
  
  # Predict test set --------------------------------------------------------
  test_predictions <- data %>%
    mutate(prediction = predict(model, .))
  return(list(model = model, test = test_predictions))
}

data <- read_d(input_file)
data_preproc <- preproc_d(data)
out <- fit_and_test(data_preproc, -1)
