# EXERCISE 4
#
# TASK: Use plan from Exercise 2 and:
# - Add input file tracking to the plan
# - Add to the plan rendering of `report.Rmd` to `report.html` (Hint: Use `rmarkdown::render()`, `kintr_in()` and `file_out()`)
# - Test what happens to the plan after modification to input file or removal of rendered report (html file)

library(dplyr)
library(purrr)
library(mgcv)

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

library(drake)

readd(test_predictions)

my_plan <- drake_plan(
  # Data
  data_in = readr::read_csv(file_in('data/Metro_Interstate_Traffic_Volume.csv')),
  data = preprocessData(data_in),
  
  # Model
  model = fitModel(data, gam_k = -1),
  
  #Performance
  test_predictions = predictNewData(model, data),
  
  rmarkdown::render(knitr_in("report.Rmd"), output_file = file_out("report.html"))
)

make(my_plan)
vis_drake_graph(my_plan)

unlink("report.html")
outdated(my_plan)
vis_drake_graph(my_plan)

make(my_plan)
outdated(my_plan)
