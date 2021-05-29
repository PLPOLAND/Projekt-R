source("mtcars_dane.r")
source("machine_dane.r")
source("imports-85_dane.r")

library(e1071) #biblioteka z SVM

svm_regression_mtCars <- function(){
  cat("\n\n\n SVM MTCARS \n")
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

svm_regression_imports <- function(){
  cat("\n\n\n SVM IMPORTS-85 \n")

  imports_train = wczytaj_imports_85()$train
  imports_test = wczytaj_imports_85()$test
  imports_columns = wczytaj_imports_85()$columns

  idx = which(imports_columns == "engine.location")
  imports_columns = imports_columns[ -idx] #usuwam dodatkowo engine,location ponieważ svm "krzyczy" że mu nie pasuje - brak danych o wartości innej niż 1

  imports_svm_radial = svm_imports(imports_columns, imports_test)
  przewidziane = predict(imports_svm_radial,imports_test)
  cat("\nRMSE dla kernel 'radial' = ", rmse(imports_test$horsepower,przewidziane))

  imports_svm_polynomial = svm_imports(imports_columns, imports_test, kernel = "polynomial")
  przewidziane2 = predict(imports_svm_polynomial,imports_test)
  cat("\nRMSE dla kernel 'polynomial' = ", rmse(imports_test$horsepower,przewidziane2))

  imports_svm_sigmoid = svm_imports(imports_columns, imports_test, kernel = "sigmoid")
  przewidziane3 = predict(imports_svm_sigmoid,imports_test)
  cat("\nRMSE dla kernel 'sigmoid' = ", rmse(imports_test$horsepower,przewidziane3))

  imports_svm_linear = svm_imports(imports_columns, imports_test, kernel = "linear")
  przewidziane4 = predict(imports_svm_linear,imports_test)
  cat("\nRMSE dla kernel 'linear' = ", rmse(imports_test$horsepower,przewidziane4))
}

svm_regression_machine <- function(){ 
  cat("\n\n\n SVM MACHINE \n")
  
  machine_train = wczytaj_machine()$train
  machine_test = wczytaj_machine()$test
  machine_columns = wczytaj_machine()$columns
  
  imports_svm_radial = svm_machine(machine_columns, machine_test)
  przewidziane = predict(imports_svm_radial,machine_test)
  cat("\nRMSE dla kernel 'radial' = ", rmse(machine_test$PRP,przewidziane))
  
  imports_svm_polynomial = svm_machine(machine_columns, machine_test, kernel = "polynomial")
  przewidziane2 = predict(imports_svm_polynomial,machine_test)
  cat("\nRMSE dla kernel 'polynomial' = ", rmse(machine_test$PRP,przewidziane2))
  
  imports_svm_sigmoid = svm_machine(machine_columns, machine_test, kernel = "sigmoid")
  przewidziane3 = predict(imports_svm_sigmoid,machine_test)
  cat("\nRMSE dla kernel 'sigmoid' = ", rmse(machine_test$PRP,przewidziane3))
  
  imports_svm_linear = svm_machine(machine_columns, machine_test, kernel = "linear")
  przewidziane4 = predict(imports_svm_linear,machine_test)
  cat("\nRMSE dla kernel 'linear' = ", rmse(machine_test$PRP,przewidziane4))
}
# machine = wczytaj_machine()