source("mtcars_dane.r")
source("machine_dane.r")
source("imports-85_dane.r")
source("ENB2012_dane.r")

library(e1071) #biblioteka z SVM

svm_regression_mtCars <- function(){
  cat("\n\n\n SVM MTCARS \n")
  cars_train = wczytaj_mtcars()$train
  cars_test = wczytaj_mtcars()$test
  cars_columns = wczytaj_mtcars()$columns

  cars_svm_radial = svm_mtcars(cars_columns, cars_train)
  przewidziane = predict(cars_svm_radial,cars_test)
  cat("\nRMSE dla kernel 'radial' = ", rmse(cars_test$qsec,przewidziane))#~0.60968

  cars_svm_polynomial = svm_mtcars(cars_columns, cars_train, kernel = "polynomial")
  przewidziane2 = predict(cars_svm_polynomial,cars_test)
  cat("\nRMSE dla kernel 'polynomial' = ", rmse(cars_test$qsec,przewidziane2))#~0.9385

  cars_svm_sigmoid = svm_mtcars(cars_columns, cars_train, kernel = "sigmoid")
  przewidziane3 = predict(cars_svm_sigmoid,cars_test)
  cat("\nRMSE dla kernel 'sigmoid' = ", rmse(cars_test$qsec,przewidziane3))#~0.86997

  cars_svm_linear = svm_mtcars(cars_columns, cars_train, kernel = "linear")
  przewidziane4 = predict(cars_svm_linear,cars_test)
  cat("\nRMSE dla kernel 'linear' = ", rmse(cars_test$qsec,przewidziane4))#~0.3739
  
  ###MSE###
  
  cars_svm_radial = svm_mtcars(cars_columns, cars_train)
  przewidziane = predict(cars_svm_radial,cars_test)
  cat("\nMSE dla kernel 'radial' = ", mse(cars_test$qsec,przewidziane))#1.671088
  
  cars_svm_polynomial = svm_mtcars(cars_columns, cars_train, kernel = "polynomial")
  przewidziane2 = predict(cars_svm_polynomial,cars_test)
  cat("\nMSE dla kernel 'polynomial' = ", mse(cars_test$qsec,przewidziane2))#3.363071
  
  cars_svm_sigmoid = svm_mtcars(cars_columns, cars_train, kernel = "sigmoid")
  przewidziane3 = predict(cars_svm_sigmoid,cars_test)
  cat("\nMSE dla kernel 'sigmoid' = ", mse(cars_test$qsec,przewidziane3))#1.066011
  
  cars_svm_linear = svm_mtcars(cars_columns, cars_train, kernel = "linear")
  przewidziane4 = predict(cars_svm_linear,cars_test)
  cat("\nMSE dla kernel 'linear' = ", mse(cars_test$qsec,przewidziane4))# 0.6467646
  
}

svm_regression_imports <- function(){
  cat("\n\n\n SVM IMPORTS-85 \n")

  imports_train = wczytaj_imports_85()$train
  imports_test = wczytaj_imports_85()$test
  imports_columns = wczytaj_imports_85()$columns

  imports_svm_radial = svm_imports(imports_columns, imports_train)
  przewidziane = predict(imports_svm_radial,imports_test)
  cat("\nRMSE dla kernel 'radial' = ", rmse(imports_test$symboling,przewidziane))

  imports_svm_polynomial = svm_imports(imports_columns, imports_train, kernel = "polynomial")
  przewidziane2 = predict(imports_svm_polynomial,imports_test)
  cat("\nRMSE dla kernel 'polynomial' = ", rmse(imports_test$symboling,przewidziane2))

  imports_svm_sigmoid = svm_imports(imports_columns, imports_train, kernel = "sigmoid")
  przewidziane3 = predict(imports_svm_sigmoid,imports_test)
  cat("\nRMSE dla kernel 'sigmoid' = ", rmse(imports_test$symboling,przewidziane3))

  imports_svm_linear = svm_imports(imports_columns, imports_train, kernel = "linear")
  przewidziane4 = predict(imports_svm_linear,imports_test)
  cat("\nRMSE dla kernel 'linear' = ", rmse(imports_test$symboling,przewidziane4))
  
  ###MSE###
  
  cat("\nMSE dla kernel 'radial' = ", mse(imports_test$symboling,przewidziane))
  cat("\nMSE dla kernel 'polynomial' = ", mse(imports_test$symboling,przewidziane2))
  cat("\nMSE dla kernel 'sigmoid' = ", mse(imports_test$symboling,przewidziane3))
  cat("\nMSE dla kernel 'linear' = ", mse(imports_test$symboling,przewidziane4))
}

svm_regression_machine <- function(){
  cat("\n\n\n SVM MACHINE \n")

  machine_train = wczytaj_machine()$train
  machine_test = wczytaj_machine()$test
  machine_columns = wczytaj_machine()$columns

  machine_svm_radial = svm_machine(machine_columns, machine_train)
  przewidziane = predict(machine_svm_radial,machine_test)
  cat("\nRMSE dla kernel 'radial' = ", rmse(machine_test$PRP,przewidziane))

  machine_svm_polynomial = svm_machine(machine_columns, machine_train, kernel = "polynomial")
  przewidziane2 = predict(machine_svm_polynomial,machine_test)
  cat("\nRMSE dla kernel 'polynomial' = ", rmse(machine_test$PRP,przewidziane2))

  machine_svm_sigmoid = svm_machine(machine_columns, machine_train, kernel = "sigmoid")
  przewidziane3 = predict(machine_svm_sigmoid,machine_test)
  cat("\nRMSE dla kernel 'sigmoid' = ", rmse(machine_test$PRP,przewidziane3))

  machine_svm_linear = svm_machine(machine_columns, machine_train, kernel = "linear")
  przewidziane4 = predict(machine_svm_linear,machine_test)
  cat("\nRMSE dla kernel 'linear' = ", rmse(machine_test$PRP,przewidziane4))
  
  ###MSE###
  
  cat("\nMSE dla kernel 'radial' = ", mse(machine_test$PRP,przewidziane))
  cat("\nMSE dla kernel 'polynomial' = ", mse(machine_test$PRP,przewidziane2))
  cat("\nMSE dla kernel 'sigmoid' = ", mse(machine_test$PRP,przewidziane3))
  cat("\nMSE dla kernel 'linear' = ", mse(machine_test$PRP,przewidziane4))
  
}

svm_regression_energy <- function(){ 
  cat("\n\n\n SVM ENERGY \n")
  
  energy_train = wczytaj_energy_efficiency()$train
  energy_test = wczytaj_energy_efficiency()$test
  energy_columns = wczytaj_energy_efficiency()$columns
  
  energy_svm_radial = svm_energy(energy_columns, energy_train)
  przewidziane = predict(energy_svm_radial,energy_test)
  cat("\nRMSE dla kernel 'radial' = ", rmse(energy_test$HeatingLoad,przewidziane))
  
  energy_svm_polynomial = svm_energy(energy_columns, energy_train, kernel = "polynomial")
  przewidziane2 = predict(energy_svm_polynomial,energy_test)
  cat("\nRMSE dla kernel 'polynomial' = ", rmse(energy_test$HeatingLoad,przewidziane2))
  
  energy_svm_sigmoid = svm_energy(energy_columns, energy_train, kernel = "sigmoid")
  przewidziane3 = predict(energy_svm_sigmoid,energy_test)
  cat("\nRMSE dla kernel 'sigmoid' = ", rmse(energy_test$HeatingLoad,przewidziane3))
  
  energy_svm_linear = svm_energy(energy_columns, energy_train, kernel = "linear")
  przewidziane4 = predict(energy_svm_linear,energy_test)
  cat("\nRMSE dla kernel 'linear' = ", rmse(energy_test$HeatingLoad,przewidziane4))
  
  
  ###MSE###
  cat("\nMSE dla kernel 'radial' = ", mse(energy_test$HeatingLoad,przewidziane))
  cat("\nMSE dla kernel 'polynomial' = ", mse(energy_test$HeatingLoad,przewidziane2))
  cat("\nMSE dla kernel 'sigmoid' = ", mse(energy_test$HeatingLoad,przewidziane3))
  cat("\nMSE dla kernel 'linear' = ", mse(energy_test$HeatingLoad,przewidziane4))
}