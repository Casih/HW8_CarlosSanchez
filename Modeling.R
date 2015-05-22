#########################-Librerias-########################
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("xts")
#install.packages("forecast")


library(dplyr)
library(tidyr)
library(ggplot2)
library(xts)
library(forecast)
#########################-Librerias-########################


####################-Input: Entrenamiento-##################
DataSet <- read.csv("~/Desktop/training_set.csv",header=T)
####################-Input: Entrenamiento-##################

########################-Input: Pruebas-####################
DataSetTest <- read.csv("~/Desktop/test_set.csv",header=T)
########################-Input: Pruebas-####################

####################-Data Wrangling-########################
xreg <- cbind(DataSet$cod_calendario,DataSet$conteo_restaurantes,DataSet$temp_max,DataSet$temp_min,DataSet$precipitacion,DataSet$eventos)
colnames(xreg) <- c("cod_calendario","conteo_restaurantes","temp_max","temp_min","precipitacion","eventos")
xregt <- cbind(DataSetTest$cod_calendario,DataSetTest$conteo_restaurantes,DataSetTest$temp_max,DataSetTest$temp_min,DataSetTest$precipitacion,DataSetTest$eventos)
colnames(xregt) <- c("cod_calendario","conteo_restaurantes","temp_max","temp_min","precipitacion","eventos")
xtrain = as.POSIXct(DataSet$fecha)
xtest = as.POSIXct(DataSetTest$fecha)
####################-Data Wrangling-########################

######################Arima(4,0,0)########################
y = arima(DataSet$conteo_ordenes,order=c(4,0,0),season=c(4,0,3),xreg=xreg, method="ML",transform.pars = FALSE, include.mean = TRUE)
######################-Arima(4,0,0)-########################

####################-Data Wrangling-########################
result = as.data.frame(forecast(y,xreg=xregt))
colnames(result) <- c("A","B","C","D","E")
fecha <- as.factor(xtest)
conteo_ordenes <- result$A
df = data.frame(fecha, conteo_ordenes)
####################-Data Wrangling-########################

##############-Output: predicciones en test-################
write.csv(df, "~/Desktop/result.csv", row.names=FALSE)
##############-Output: predicciones en test-################
