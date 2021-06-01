
pacman::p_load(foreign)


rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

wczytaj_imports_85 <- function(){
  imports = read.csv("imports-85.csv")
  #summary(imports)
  
  imports$symboling = as.factor(imports$symboling)
  imports$make = as.factor(imports$make)
  imports$fuel.type = as.factor(imports$fuel.type)
  imports$aspiration = as.factor(imports$aspiration)
  imports$num.of.doors = as.factor(imports$num.of.doors)
  imports$body.style = as.factor(imports$body.style)
  imports$drive.wheels = as.factor(imports$drive.wheels)
  imports$engine.location = as.factor(imports$engine.location)
  imports$engine.type = as.factor(imports$engine.type)
  imports$fuel.system = as.factor(imports$fuel.system)
  #summary(imports)
  
  #hist(imports$normalized.losses,breaks = 50) #wybieram mediane bo nie jest rozk?ad normalny
  imports$normalized.losses[which(is.na(imports$normalized.losses))] = median(imports$normalized.losses, na.rm = T) 
  
  #summary(imports)
  #table(imports$body.style[which(is.na(imports$num.of.doors))])
  imports$num.of.doors[which(is.na(imports$num.of.doors))] = as.factor("four") #wybieram 4 poniewa? wi?kszo?? aut tego typu jest 4-drzwiowe
  
  #hist(imports$bore,breaks = 50)#wybieram mediane bo nie jest rozk?ad normalny
  imports$bore[which(is.na(imports$bore))] = median(imports$bore, na.rm = T)
  
  #summary(imports)
  #hist(imports$stroke,breaks = 50)#wybieram mediane bo nie jest rozk?ad normalny
  imports$stroke[which(is.na(imports$stroke))] = median(imports$stroke, na.rm = T)
  
  #hist(imports$horsepower,breaks = 50)#wybieram mediane bo nie jest rozk?ad normalny
  imports$horsepower[which(is.na(imports$horsepower))] = median(imports$horsepower, na.rm = T)
  
  #summary(imports)
  #hist(imports$peak.rpm,breaks = 50)#wybieram mediane bo nie jest rozk?ad normalny
  imports$peak.rpm[which(is.na(imports$peak.rpm))] = median(imports$peak.rpm, na.rm = T)
  
  #hist(imports$price, breaks = 50)#wybieram mediane bo nie jest rozk?ad normalny
  imports$price[which(is.na(imports$price))] = median(imports$price, na.rm = T)
  set.seed(123)
  
  imports = as.data.frame(sapply(imports, as.numeric))#zmiana na numeryczne i "ramke"
  
  indexy <- sample(nrow(imports), size = trunc(0.25* nrow(imports)) )
  train <- imports[indexy, ]
  test <- imports[-indexy, ]
  
  #eliminacja zmiennych
  tmp <- lm(symboling~1, data = train)
  forward = step(tmp, direction = "forward", scope = list(upper=.~.+ horsepower + `normalized.losses` + make + `fuel.type` + aspiration + `num.of.doors` + `body.style` + `drive.wheels` + `engine.location` + `wheel.base` + length + width + height + `curb.weight` + `engine.type` + `num.of.cylinders` + `engine.size` + `fuel.system` + bore + stroke + `compression.ratio` + price + `peak.rpm` + `city.mpg` + `highway.mpg`), trace=0)
  summary(forward)$r.squared#0.7512137
  
  tmp <- lm(symboling~horsepower+`normalized.losses`+make+`fuel.type`+aspiration+`num.of.doors`+`body.style`+`drive.wheels`+`engine.location`+`wheel.base`+length+width+height+`curb.weight`+`engine.type`+`num.of.cylinders`+`engine.size`+`fuel.system`+bore+stroke+`compression.ratio`+price+`peak.rpm`+`city.mpg`+`highway.mpg`, data = train)
  backward = step(tmp, direction = "backward", scope = list(upper=.~.+ horsepower + `normalized.losses` + make + `fuel.type` + aspiration + `num.of.doors` + `body.style` + `drive.wheels` + `engine.location` + `wheel.base` + length + width + height + `curb.weight` + `engine.type` + `num.of.cylinders` + `engine.size` + `fuel.system` + bore + stroke + `compression.ratio` + price + `peak.rpm` + `city.mpg` + `highway.mpg`), trace=0)
  summary(backward)$r.squared#0.8202633
  
  tmp <- lm(symboling~. , data = imports)
  both <- step(tmp, direction = "both", scope = list(upper=.~.+ horsepower + `normalized.losses` + make + `fuel.type` + aspiration + `num.of.doors` + `body.style` + `drive.wheels` + `engine.location` + `wheel.base` + length + width + height + `curb.weight` + `engine.type` + `num.of.cylinders` + `engine.size` + `fuel.system` + bore + stroke + `compression.ratio` + price + `peak.rpm` + `city.mpg` + `highway.mpg`), trace=0)
  summary(both)$r.squared#0.7014353
  
  nazwyKolumn <- variable.names(backward) #najlepszy forward - najwyższy r.squared
  nazwyKolumn[1] <- "symboling" #poprawka nazwy 
  
  return(list("columns" = nazwyKolumn, "train" = train, "test"=test))
}

svm_imports <- function(kolumny, dane, kernel = "radial"){
  model = svm(symboling~., data=dane[,kolumny], kernel = kernel, type="nu-regression")
  return (model)
}

# pacman::p_load(corrplot)
# imports.cor <- sapply(imports,function(x) as.numeric(x))
# cor_matrix=cor(imports.cor)
# corrplot.mixed(cor_matrix)
