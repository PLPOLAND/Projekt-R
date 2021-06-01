
pacman::p_load(foreign)
# 
# machine = read.table("machine.data", sep=",")
# names(machine)[1] <- "vendor name"
# names(machine)[2] <- "Model Name"
# names(machine)[3] <- "MYCT"
# names(machine)[4] <- "MMIN"
# names(machine)[5] <- "MMAX"
# names(machine)[6] <- "CACH"
# names(machine)[7] <- "CHMIN"
# names(machine)[8] <- "CHMAX"
# names(machine)[9] <- "PRP"
# names(machine)[10] <- "ERP"
# machine$`vendor name` = as.factor(machine$`vendor name`)
# 
# idx= which(colnames(machine) == "Model Name") 
# machine = machine[ ,-idx] #usuni?cie nie potrzebnej danej
# 
# summary(machine)
# 
# pacman::p_load(corrplot)
# machine.cor <- sapply(machine,function(x) as.numeric(x))
# cor_matrix=cor(machine.cor)
# corrplot.mixed(cor_matrix)

rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}
wczytaj_machine <- function(){
  machine = read.table("machine.data", sep=",")
  names(machine)[1] <- "vendor name"
  names(machine)[2] <- "Model Name"
  names(machine)[3] <- "MYCT"
  names(machine)[4] <- "MMIN"
  names(machine)[5] <- "MMAX"
  names(machine)[6] <- "CACH"
  names(machine)[7] <- "CHMIN"
  names(machine)[8] <- "CHMAX"
  names(machine)[9] <- "PRP"
  names(machine)[10] <- "ERP"
  machine$`vendor name` = as.factor(machine$`vendor name`)
  
  idx= which(colnames(machine) == "Model Name") 
  machine = machine[ ,-idx] #usuni?cie nie potrzebnej danej
  idx= which(colnames(machine) == "ERP") 
  machine = machine[ ,-idx] #usuni?cie nie potrzebnej danej
  
  
  
  machine = as.data.frame(sapply(machine, as.numeric))#zmiana na numeryczne i "ramke"
  
  indexy <- sample(nrow(machine), size = trunc(0.25* nrow(machine)) )
  train <- machine[indexy, ]
  test <- machine[-indexy, ]
  
  tmp <- lm(PRP~1 , data = machine)
  forward <- step(tmp, direction = "forward", scope = list(upper=.~.+`vendor name`+MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX), trace=0)
  summary(forward)$r.squared#0.8648239
  
  tmp <- lm(PRP~+`vendor name`+MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX , data = machine)
  backward <- step(tmp, direction = "backward", scope = list(upper=.~.+`vendor name`+MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX), trace=0)
  summary(backward)$r.squared#0.8648239
  
  tmp <- lm(PRP~. , data = machine)
  both <- step(tmp, direction = "both", scope = list(upper=.~.+`vendor name`+MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX), trace=0)
  summary(both)$r.squared #0.8648239
  
  nazwyKolumn <- variable.names(both) #wszystkie wybrały to samo
  nazwyKolumn[1] <- "PRP" #poprawka nazwy 
  
  return (list("columns" = nazwyKolumn, "train" = train, "test"=test))
}

svm_machine <- function(kolumny, dane, kernel = "radial"){
  model = svm(PRP~., data=dane[,kolumny], kernel = kernel, type="nu-regression")
  return (model)
}
