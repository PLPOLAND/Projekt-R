
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
# corrplot(cor_matrix)

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
  
  print("Wczytano machine i poprawiono dane")
  return(machine)
}