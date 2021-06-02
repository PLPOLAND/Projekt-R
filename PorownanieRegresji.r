source("svmRegresja.r")
source("glmRegresja.r")
source("lmRegresja.r")

svm_regression_mtCars() #pokaż RMSE dla regresji SVM na mtcars
svm_regression_imports() #pokaż RMSE dla regresji SVM na imports
svm_regression_machine() #pokaż RMSE dla regresji SVM na machines
svm_regression_energy() #pokaż RMSE dla regresji SVM na Energy efficiency

glm_regression_mtCars() #pokaż RMSE dla regresji GLM na mtcars
glm_regression_imports() #pokaż RMSE dla regresji GLM na imports
glm_regression_machine() #pokaż RMSE dla regresji GLM na machines 
glm_regression_energy() #pokaż RMSE dla regresji GLM na Energy efficiency

lm_regression_mtCars() #pokaż RMSE dla regresji LM na mtcars
lm_regression_imports() #pokaż RMSE dla regresji LM na imports
lm_regression_machine() #pokaż RMSE dla regresji LM na machines 
lm_regression_energy() #pokaż RMSE dla regresji LM na Energy efficiency
