source("mtcars_dane.r")
source("machine_dane.r")
source("imports-85_dane.r")

lm_regression_mtCars <- function(){
  cat("\n\n\n LM MTCARS \n")
  cars_train = wczytaj_mtcars()$train
  cars_test = wczytaj_mtcars()$test
  cars_columns = wczytaj_mtcars()$columns
  
  model = lm(qsec~., data=cars_train[,cars_columns])
  predicted = predict(model, cars_test)
  cat("\nRMSE dla LM = ", rmse(cars_test$qsec,predicted))
  
}

lm_regression_imports <- function(){
  cat("\n\n\n LM IMPORTS-85 \n")
  
  imports_train = wczytaj_imports_85()$train
  imports_test = wczytaj_imports_85()$test
  imports_columns = wczytaj_imports_85()$columns
  
  model = lm(symboling~., data=imports_train[,imports_columns])
  predicted = predict(model, imports_test)
  cat("\nRMSE dla LM = ", rmse(imports_test$symboling,predicted))
  
}

lm_regression_machine <- function(){
  cat("\n\n\n LM MACHINE \n")
  
  machine_train = wczytaj_machine()$train
  machine_test = wczytaj_machine()$test
  machine_columns = wczytaj_machine()$columns
  
  model = lm(PRP~., data=machine_train[,machine_columns])
  predicted = predict(model, machine_test)
  cat("\nRMSE dla LM = ", rmse(machine_test$PRP,predicted))
  
}
