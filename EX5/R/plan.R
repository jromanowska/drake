my_plan <- drake_plan(
  # Data
  data_in = readr::read_csv(file_in(!!input_file)),
  data = preprocessData(data_in),
  
  # Model
  model = target(fitModel(data, gam_k = gam_k_vect),
                 transform = map(gam_k_vect = !!gam_k_vect_in)),
  
  #Performance
  test_predictions = target(predictNewData(model, data),transform = map(model))
  #                           ,
  # 
  # rmarkdown::render(knitr_in("report.Rmd"), output_file = file_out("report.html"))
)
