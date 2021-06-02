source("mtcars_dane.r")
source("machine_dane.r")
source("imports-85_dane.r")

glm_regression_mtCars <- function(){
  cat("\n\n\n GLM MTCARS \n")
  cars_train = wczytaj_mtcars()$train
  cars_test = wczytaj_mtcars()$test
  cars_columns = wczytaj_mtcars()$columns
  
  options(warn=-1) #turn off warnings - predict
  
  glm_quasipoisson = glm(qsec~., data=cars_train[,cars_columns],
                        family = quasipoisson(link = "log"))
  predicted_quasipoisson = predict(glm_quasipoisson, cars_test)
  cat("\nRMSE dla GLM 'quasipoisson' = ", rmse(cars_test$qsec,predicted_quasipoisson))
  cat("\nMSE dla GLM 'quasipoisson' = ", mse(cars_test$qsec,predicted_quasipoisson))
  
  
  glm_gamma = glm(qsec~., data=cars_train[,cars_columns],
                  family = Gamma(link = "inverse"))
  predicted_gamma = predict(glm_gamma, cars_test)
  cat("\nRMSE dla GLM 'gamma' = ", rmse(cars_test$qsec,predicted_gamma))
  cat("\nMSE dla GLM 'gamma' = ", mse(cars_test$qsec,predicted_gamma))
  
  
  glm_gaussian = glm(qsec~., data=cars_train[,cars_columns],
                    family = gaussian(link = "identity"))
  predicted_gaussian = predict(glm_gaussian, cars_test)
  cat("\nRMSE dla GLM 'gaussian' = ", rmse(cars_test$qsec,predicted_gaussian))
  cat("\nMSE dla GLM 'gaussian' = ", mse(cars_test$qsec,predicted_gaussian))
  
  
  glm_quasi = glm(qsec~., data=cars_train[,cars_columns],
                  family = quasi(link = "identity", variance = "constant"))
  predicted_quasi = predict(glm_quasi, cars_test)
  cat("\nRMSE dla GLM 'quasi' = ", rmse(cars_test$qsec,predicted_quasi))
  cat("\nMSE dla GLM 'quasi' = ", mse(cars_test$qsec,predicted_quasi))
  
  
  glm_poisson = glm(qsec~., data=cars_train[,cars_columns],
                    family = poisson(link = "log"))
  predicted_poisson = predict(glm_poisson, cars_test)
  cat("\nRMSE dla GLM 'poisson' = ", rmse(cars_test$qsec,predicted_poisson))
  cat("\nMSE dla GLM 'poisson' = ", mse(cars_test$qsec,predicted_poisson))
  
  options(warn=1) #turn on warnings - predict
}

glm_regression_imports <- function(){
  cat("\n\n\n GLM IMPORTS-85 \n")
  
  imports_train = wczytaj_imports_85()$train
  imports_test = wczytaj_imports_85()$test
  imports_columns = wczytaj_imports_85()$columns
  
  options(warn=-1) #turn off warnings - predict
  
  glm_quasipoisson = glm(symboling~., data=imports_train[,imports_columns],
                         family = quasipoisson(link = "log"))
  predicted_quasipoisson = predict(glm_quasipoisson, imports_test)
  cat("\nRMSE dla GLM 'quasipoisson' = ", rmse(imports_test$symboling,predicted_quasipoisson))
  cat("\nMSE dla GLM 'quasipoisson' = ", mse(imports_test$symboling,predicted_quasipoisson))
  
  glm_gamma = glm(symboling~., data=imports_train[,imports_columns],
                  family = Gamma(link = "inverse"))
  predicted_gamma = predict(glm_gamma, imports_test)
  cat("\nRMSE dla GLM 'gamma' = ", rmse(imports_test$symboling,predicted_gamma))
  cat("\nMSE dla GLM 'gamma' = ", mse(imports_test$symboling,predicted_gamma))
  
  
  glm_gaussian = glm(symboling~., data=imports_train[,imports_columns],
                     family = gaussian(link = "identity"))
  predicted_gaussian = predict(glm_gaussian, imports_test)
  cat("\nRMSE dla GLM 'gaussian' = ", rmse(imports_test$symboling,predicted_gaussian))
  cat("\nMSE dla GLM 'gaussian' = ", mse(imports_test$symboling,predicted_gaussian))
  
  
  glm_quasi = glm(symboling~., data=imports_train[,imports_columns],
                  family = quasi(link = "identity", variance = "constant"))
  predicted_quasi = predict(glm_quasi, imports_test)
  cat("\nRMSE dla GLM 'quasi' = ", rmse(imports_test$symboling,predicted_quasi))
  cat("\nMSE dla GLM 'quasi' = ", mse(imports_test$symboling,predicted_quasi))
  
  
  glm_poisson = glm(symboling~., data=imports_train[,imports_columns],
                    family = poisson(link = "log"))
  predicted_poisson = predict(glm_poisson, imports_test)
  cat("\nRMSE dla GLM 'poisson' = ", rmse(imports_test$symboling,predicted_poisson))
  cat("\nMSE dla GLM 'poisson' = ", mse(imports_test$symboling,predicted_poisson))
  
  options(warn=1) #turn on warnings - predict
}

glm_regression_machine <- function(){
  cat("\n\n\n GLM MACHINE \n")
  
  machine_train = wczytaj_machine()$train
  machine_test = wczytaj_machine()$test
  machine_columns = wczytaj_machine()$columns
  
  options(warn=-1) #turn off warnings - predict
  
  glm_quasipoisson = glm(PRP~., data=machine_train[,machine_columns],
                         family = quasipoisson(link = "log"))
  predicted_quasipoisson = predict(glm_quasipoisson, machine_test)
  cat("\nRMSE dla GLM 'quasipoisson' = ", rmse(machine_test$PRP,predicted_quasipoisson))
  cat("\nMSE dla GLM 'quasipoisson' = ", mse(machine_test$PRP,predicted_quasipoisson))
  
  
  glm_gamma = glm(PRP~., data=machine_train[,machine_columns],
                  family = Gamma(link = "inverse"))
  predicted_gamma = predict(glm_gamma, machine_test)
  cat("\nRMSE dla GLM 'gamma' = ", rmse(machine_test$PRP,predicted_gamma))
  cat("\nMSE dla GLM 'gamma' = ", mse(machine_test$PRP,predicted_gamma))
  
  
  glm_gaussian = glm(PRP~., data=machine_train[,machine_columns],
                     family = gaussian(link = "identity"))
  predicted_gaussian = predict(glm_gaussian, machine_test)
  cat("\nRMSE dla GLM 'gaussian' = ", rmse(machine_test$PRP,predicted_gaussian))
  cat("\nMSE dla GLM 'gaussian' = ", mse(machine_test$PRP,predicted_gaussian))
  
  
  glm_quasi = glm(PRP~., data=machine_train[,machine_columns],
                  family = quasi(link = "identity", variance = "constant"))
  predicted_quasi = predict(glm_quasi, machine_test)
  cat("\nRMSE dla GLM 'quasi' = ", rmse(machine_test$PRP,predicted_quasi))
  cat("\nMSE dla GLM 'quasi' = ", mse(machine_test$PRP,predicted_quasi))
  
  
  glm_poisson = glm(PRP~., data=machine_train[,machine_columns],
                    family = poisson(link = "log"))
  predicted_poisson = predict(glm_poisson, machine_test)
  cat("\nRMSE dla GLM 'poisson' = ", rmse(machine_test$PRP,predicted_poisson))
  cat("\nMSE dla GLM 'poisson' = ", mse(machine_test$PRP,predicted_poisson))
  
  options(warn=1) #turn on warnings - predict
}

glm_regression_energy <- function(){
  cat("\n\n\n GLM ENERGY \n")
  
  energy_train = wczytaj_energy_efficiency()$train
  energy_test = wczytaj_energy_efficiency()$test
  energy_columns = wczytaj_energy_efficiency()$columns
  
  options(warn=-1) #turn off warnings - predict
  
  glm_quasipoisson = glm(HeatingLoad~., data=energy_train[,energy_columns],
                         family = quasipoisson(link = "log"))
  predicted_quasipoisson = predict(glm_quasipoisson, energy_test)
  cat("\nRMSE dla GLM 'quasipoisson' = ", rmse(energy_test$HeatingLoad,predicted_quasipoisson))
  cat("\nMSE dla GLM 'quasipoisson' = ", mse(energy_test$HeatingLoad,predicted_quasipoisson))
  
  
  glm_gamma = glm(HeatingLoad~., data=energy_train[,energy_columns],
                  family = Gamma(link = "inverse"))
  predicted_gamma = predict(glm_gamma, energy_test)
  cat("\nRMSE dla GLM 'gamma' = ", rmse(energy_test$HeatingLoad,predicted_gamma))
  cat("\nMSE dla GLM 'gamma' = ", mse(energy_test$HeatingLoad,predicted_gamma))
  
  
  glm_gaussian = glm(HeatingLoad~., data=energy_train[,energy_columns],
                     family = gaussian(link = "identity"))
  predicted_gaussian = predict(glm_gaussian, energy_test)
  cat("\nRMSE dla GLM 'gaussian' = ", rmse(energy_test$HeatingLoad,predicted_gaussian))
  cat("\nMSE dla GLM 'gaussian' = ", mse(energy_test$HeatingLoad,predicted_gaussian))
  
  
  glm_quasi = glm(HeatingLoad~., data=energy_train[,energy_columns],
                  family = quasi(link = "identity", variance = "constant"))
  predicted_quasi = predict(glm_quasi, energy_test)
  cat("\nRMSE dla GLM 'quasi' = ", rmse(energy_test$HeatingLoad,predicted_quasi))
  cat("\nMSE dla GLM 'quasi' = ", mse(energy_test$HeatingLoad,predicted_quasi))
  
  
  glm_poisson = glm(HeatingLoad~., data=energy_train[,energy_columns],
                    family = poisson(link = "log"))
  predicted_poisson = predict(glm_poisson, energy_test)
  cat("\nRMSE dla GLM 'poisson' = ", rmse(energy_test$HeatingLoad,predicted_poisson))
  cat("\nMSE dla GLM 'poisson' = ", mse(energy_test$HeatingLoad,predicted_poisson))
  
  options(warn=1) #turn on warnings - predict
}
