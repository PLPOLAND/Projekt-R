pacman::p_load(foreign)

#https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars
#opis pod powy?szym linkiem

# #sprawdzenie corelacji
# pacman::p_load(corrplot)
# cars.cor <- sapply(cars,function(x) as.numeric(x))
# cor_matrix=cor(cars.cor)
# corrplot(cor_matrix)



# wczytaj_mtcars <- function(){
#   cars = mtcars
#   # summary(cars)
#   
#   
#   cars$vs = factor(cars$vs ,labels = c("V", "S"))
#   cars$am = factor(cars$am , labels = c("automatic", "manual") )
#   cars$gear = as.factor(cars$gear)
#   
#   cat("Wczytano mtcars i poprawiono dane")
#   return(cars)
# }

rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

wczytaj_mtcars <- function(){
  cars = mtcars
  cars = as.data.frame(sapply(cars, as.numeric))#zmiana na numeryczne i "ramke"
  
  granica <- floor((nrow(cars)/4)*3)
  train <- cars[1:granica, ]
  test <- cars[(granica+1):nrow(cars), ]
  
  #wybieranie kolumn "ważnych"
  tmp <- lm(qsec~1 , data = cars)
  forward <- step(tmp, direction = "forward", scope = list(upper=.~.+mpg+cyl+hp+drat+wt+vs+am+gear+carb), trace=0)
  summary(forward)$r.squared#0.8523863
  
  tmp <- lm(qsec~mpg+cyl+hp+drat+wt+vs+am+gear+carb , data = cars)
  backward <- step(tmp, direction = "backward", scope = list(upper=.~.+mpg+cyl+hp+drat+wt+vs+am+gear+carb), trace=0)
  summary(backward)$r.squared#0.8453189
  
  tmp <- lm(qsec~. , data = cars)
  both <- step(tmp, direction = "both", scope = list(upper=.~.+mpg+cyl+hp+drat+wt+vs+am+gear+carb), trace=0)
  summary(both)$r.squared #0.8642928
  
  nazwyKolumn <- variable.names(both) #najlepszy both
  nazwyKolumn[1] <- "qsec" #poprawka nazwy 
  
  return (list("columns" = nazwyKolumn, "train" = train, "test"=test))
}

svm_mtcars <- function(kolumny, dane, kernel = "radial"){
  model = svm(qsec~., data=dane[,kolumny], kernel = kernel, type="nu-regression")
  return (model)
}