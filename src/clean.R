# Source of script: https://github.com/christophM/interpretable-ml-book/blob/master/R/get-cervical-cancer-dataset.R
# Dataset source: http://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29
# Paper: http://www.inescporto.pt/~jsc/publications/conferences/2017KelwinIBPRIA.pdf

library("dplyr")

get.cervical.data = function(data_dir) {
  cervical = read.csv(
    sprintf('%s/risk_factors_cervical_cancer.csv', data_dir), 
    na.strings = c('?'), 
    stringsAsFactors = FALSE)
  
  cervical = select(cervical, -Citology, -Schiller, -Hinselmann)
  
  cervical$Biopsy = factor(cervical$Biopsy, levels = c(0, 1), labels=c('Healthy', 'Cancer'))
  
  ## subset variables to the ones that should be used in the book
  cervical = dplyr::select(cervical, 
                           Age, 
                           Number.of.sexual.partners, 
                           First.sexual.intercourse,
                           Num.of.pregnancies, 
                           Smokes, 
                           Smokes..years., 
                           Hormonal.Contraceptives, 
                           Hormonal.Contraceptives..years.,
                           IUD, 
                           IUD..years., 
                           STDs, 
                           STDs..number., 
                           STDs..Number.of.diagnosis, 
                           STDs..Time.since.first.diagnosis,
                           STDs..Time.since.last.diagnosis, 
                           Biopsy)
  
  # NA imputation
  imputer = mlr::imputeMode()
  
  
  cervical_impute = mlr::impute(cervical, classes = list(numeric = imputeMode()))
  cervical = cervical_impute$data
  
  
  # Factor variables: Smokes, IUD, STDs, Hormonal.Contraceptives
  cervical$Smokes = factor(cervical$Smokes, levels = c(0, 1), labels=c("Smoker", "Non Smoker"))
  cervical$STDs = factor(cervical$STDs, levels=c(0, 1), labels=c("Has STDs", "No STDs"))
  cervical$IUD = factor(cervical$IUD, levels = c(0, 1), labels=c("Has IUD", "No IUD"))
  cervical$Hormonal.Contraceptives = factor(cervical$Hormonal.Contraceptives, levels = c(0, 1), labels=c("Contraceptives", "No Contraceptives"))
  cervical <- setNames(
    cervical, 
    c("age", "num_sexual_partners", "first_sexual_intercourse", "num_pregnancies",
      "smokes", "smokes_yrs", "hormonal_contraceptives", "hormonal_contraceptives_yrs",
      "iud", "iud_yrs", "stds", "num_stds", "num_stds_diag", "first_time_std_diag",
      "last_time_std_diag", "biopsy"))
  cervical
}

get.cervical.task = function(data_dir){
  cervical = get.cervical.data(data_dir)
  mlr::makeClassifTask(id='cervical', data = cervical, target = 'Biopsy')
}