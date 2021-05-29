source("mtcars_dane.r")
source("machine_naprawa.r")
source("imports-85_naprawa.r")

library(e1071) #biblioteka z SVM

svm_regression_mtCars <- function(){ 
  cars_train = wczytaj_mtcars()$train
  cars_test = wczytaj_mtcars()$test
  cars_columns = wczytaj_mtcars()$columns
  
  cars_svm_radial = svm_mtcars(cars_columns, cars_test)
  przewidziane = predict(cars_svm_radial,cars_test)
  cat("\nRMSE dla kernel 'radial' = ", rmse(cars_test$qsec,przewidziane))#~0.60968
  
  cars_svm_polynomial = svm_mtcars(cars_columns, cars_test, kernel = "polynomial")
  przewidziane2 = predict(cars_svm_polynomial,cars_test)
  cat("\nRMSE dla kernel 'polynomial' = ", rmse(cars_test$qsec,przewidziane2))#~0.9385
  
  cars_svm_sigmoid = svm_mtcars(cars_columns, cars_test, kernel = "sigmoid")
  przewidziane3 = predict(cars_svm_sigmoid,cars_test)
  cat("\nRMSE dla kernel 'sigmoid' = ", rmse(cars_test$qsec,przewidziane3))#~0.86997
  
  cars_svm_linear = svm_mtcars(cars_columns, cars_test, kernel = "linear")
  przewidziane4 = predict(cars_svm_linear,cars_test)
  cat("\nRMSE dla kernel 'linear' = ", rmse(cars_test$qsec,przewidziane4))#~0.3739
}

# machine = wczytaj_machine()
# imports = wczytaj_imports_85()