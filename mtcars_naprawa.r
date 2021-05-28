pacman::p_load(foreign)

#https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars
#opis pod powy?szym linkiem
# 
# cars = mtcars
# summary(cars)
# 
# 
# cars$vs = factor(cars$vs ,labels = c("V", "S"))
# cars$am = factor(cars$am , labels = c("automatic", "manual") )
# cars$gear = as.factor(cars$gear)
# 
# #sprawdzenie corelacji
# pacman::p_load(corrplot)
# cars.cor <- sapply(cars,function(x) as.numeric(x))
# cor_matrix=cor(cars.cor)
# corrplot(cor_matrix)



wczytaj_mtcars <- function(){
  cars = mtcars
  summary(cars)
  
  
  cars$vs = factor(cars$vs ,labels = c("V", "S"))
  cars$am = factor(cars$am , labels = c("automatic", "manual") )
  cars$gear = as.factor(cars$gear)
  
  print("Wczytano mtcars i poprawiono dane")
  return(cars)
}