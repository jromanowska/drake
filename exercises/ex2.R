# EXERCISE 2
#
# TASK: Transform results of Exercise 1 into drake_plan() and execute the plan.

library(drake)

library(dplyr)
library(purrr)
library(mgcv)

# Functions ---------------------------------------------------------------

preprocessData <- function(data) {
  data %>%
    filter(temp > 0) %>%
    mutate(date = as.Date(date_time),
           hour = lubridate::hour(date_time)) %>%
    group_by(date) %>%
    mutate(day_type = case_when(any(holiday != 'None') ~ 'Holiday',
                                lubridate::wday(date, week_start = 1) >= 6 ~ 'Weekend',
                                T  ~ 'Weekday')) %>%
    ungroup() %>%
    mutate(day_type = as.factor(day_type)) %>%
    select(day_type, temp, hour, traffic_volume)
}

fitModel <- function(data, gam_k) {
  gam(traffic_volume ~ s(hour, k = gam_k) + day_type + temp, data = data)
}

predictNewData <- function(model, newdata) {
  newdata %>%
    mutate(prediction = predict(model, .))
}


# Analysis ----------------------------------------------------------------
data_in <- readr::read_csv('data/Metro_Interstate_Traffic_Volume.csv')
my_plan <- drake_plan(
  # Data
  data = preprocessData(data_in),
  
  # Model
  model = fitModel(data, gam_k = -1),
  
  #Performance
  test_predictions = predictNewData(model, data)
)

make(my_plan)
readd(test_predictions)

clean()
make(my_plan)

vis_drake_graph(my_plan)
clean(model)
vis_drake_graph(my_plan)

make(my_plan)
vis_drake_graph(my_plan)

fitModel <- function(data, gam_k) {
  gam(traffic_volume ~ s(hour, k = gam_k) + day_type, data = data)
}
vis_drake_graph(my_plan)
outdated(my_plan)
make(my_plan)
vis_drake_graph(my_plan)

# nothing happens if I just edit the file, I need to read it in after editing
