library(readxl)
rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

wczytaj_energy_efficiency <- function(){
  
  energy <- read_excel("ENB2012_data.xlsx")
  
  names(energy)[1] <- "RelativeCompactness"
  names(energy)[2] <- "SurfaceArea"
  names(energy)[3] <- "WallArea"
  names(energy)[4] <- "RoofArea"
  names(energy)[5] <- "OverallHeight"
  names(energy)[6] <- "Orientation"
  names(energy)[7] <- "GlazingArea"
  names(energy)[8] <- "GlazingAreaDistribution"
  names(energy)[9] <- "HeatingLoad"
  names(energy)[10] <- "CoolingLoad"
  
  # summary(energy)
  # pacman::p_load(corrplot)
  # energy.cor <- sapply(energy,function(x) as.numeric(x))
  # cor_matrix=cor(energy.cor)
  # corrplot.mixed(cor_matrix)
  
  
  set.seed(123)
  
  energy = as.data.frame(sapply(energy, as.numeric))#zmiana na numeryczne i "ramke"
  
  indexy <- sample(nrow(energy), size = trunc(0.25* nrow(energy)) )
  train <- energy[indexy, ]
  test <- energy[-indexy, ]
  
  #wybieranie kolumn "ważnych"
  tmp <- lm(HeatingLoad~1 , data = energy)
  forward <- step(tmp, direction = "forward", scope = list(upper=.~.+RelativeCompactness+SurfaceArea+WallArea+RoofArea+OverallHeight+Orientation+GlazingArea+GlazingAreaDistribution), trace=0)
  summary(forward)$r.squared#0.9161955
  
  tmp <- lm(HeatingLoad~RelativeCompactness+SurfaceArea+WallArea+RoofArea+OverallHeight+Orientation+GlazingArea+GlazingAreaDistribution , data = energy)
  backward <- step(tmp, direction = "backward", scope = list(upper=.~.+RelativeCompactness+SurfaceArea+WallArea+RoofArea+OverallHeight+Orientation+GlazingArea+GlazingAreaDistribution), trace=0)
  summary(backward)$r.squared#0.9161955
  
  tmp <- lm(HeatingLoad~. , data = energy)
  both <- step(tmp, direction = "both", scope = list(upper=.~.+RelativeCompactness+SurfaceArea+WallArea+RoofArea+OverallHeight+Orientation+GlazingArea+GlazingAreaDistribution), trace=0)
  summary(both)$r.squared #0.9690846
  
  
  nazwyKolumn <- variable.names(both) #najlepszy forward - najwyższy r.squared
  nazwyKolumn[1] <- "HeatingLoad" #poprawka nazwy 
  
  return(list("columns" = nazwyKolumn, "train" = train, "test"=test))

}

svm_energy <- function(kolumny, dane, kernel = "radial"){
  model = svm(HeatingLoad~., data=dane[,kolumny], kernel = kernel, type="nu-regression")
  return (model)
}