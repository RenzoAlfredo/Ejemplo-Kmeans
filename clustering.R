#-------------- 0. Cargando los paquetes --------------#

install.packages(c(
  "caret",
  "DataExplorer",
  "VIM",
  "missForest",
  "ggplot2",
  "dummies",
  "lattice",
  "colorspace",
  "data.table",
  "randomForest",
  "foreach",
  "itertools",
  "MASS",
  "pROC",
  "foreign",
  "gmodels",
  "InformationValue",
  "caTools",
  "MLmeTrics",
  "dplyr",
  "iterators",
  "MLmeTrics",
  "tidyverse",
  "kableExtra",
  "scales",
  "Boruta",
  "BRugs",
  "R2OpenBUGS",
  "factoextra",
  "mvoutlier",
  "outliers",
  "cluster",
  "fpc",
  "mclust",
  "dbscan",
  "readxl",
  "psych",
  "corrplot",
  "mclust",
  "gclus",
  "rrcov",
  "tourr",
  "aplpack",
  "TeachingDemos",
  "rgl",
  "ape",
  "DMwR",
  "GGally",
  "Hmisc",
  "PerformanceAnalytics",
  "e1071",
  "class",
  "sqldf",
  "unbalanced",
  "h2o",
  "ROCR",
  "tm",
  "SnowballC",
  "wordcloud",
  "C50",
  "e1071",
  "ggpubr",
  "gridExtra",
  "gdata",
  "ggbiplot",
  "GPArotation",
  "nFactors",
  "gplots",
  "RColorBrewer",
  "semPlot",
  "tidyverse",
  "RSQLite",
  "nycflights13",
  "RODBC"
), dependencies = T)


#-------------- 1. Cargamos las librerìas --------------#

library(ggplot2)
library(caret)
library(DataExplorer)
library(VIM)
library(missForest) 
library(lattice)
library(colorspace)
library(data.table)
library(randomForest)
library(foreach)
library(itertools)
library(MASS)
library(pROC)
library(foreign)
library(gmodels) 
library(caTools) 
library(dplyr)  
library(scales)
library(Boruta) 
library(R2OpenBUGS)
library(factoextra)
library(mvoutlier)
library(outliers)
library(cluster)
library(fpc)
library(mclust)
library(dbscan)
library(readxl)
library(psych)
library(corrplot)
library(mclust)
library(gclus)
library(rrcov)
library(tourr)
library(aplpack)
library(TeachingDemos)
library(rgl)
library(ape) 
library(GGally)
library(Hmisc)
library(PerformanceAnalytics)
library(e1071)
library(class)
library(sqldf)
library(tidyverse)
library(RSQLite)
library(nycflights13)
library(RODBC)
library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
library(DataExplorer)


#-------------- 2. Leemos la conexión a la BD --------------#

con <- dbConnect(odbc::odbc(),
                 driver = 'SQL Server',
                 server = "SVRBI",
                 database = "BDANALYTICS", 
                 uid = "rhuamanyau_DCP",
                 pwd= "Sct$082023" 
)


#-------------- 3. Cargamos nuestro DS de análisis --------------# 

ds <- dbGetQuery(con,'select* from BanMed_clientes_for_clustering')
 

#-------------- 4. Browsing de la informaciòn--------------#

str(ds) #estructura
head(ds,20) #primeras k filas
summary(ds) #resumen
names(ds) #nombre de los campos
dim(ds) #dimensiones
dplyr::glimpse(ds) #estructura


#-------------- 5. Transformaciones de tipo de datos de variables--------------#

ds$sexoCat <- as.factor(ds$sexoCat) 
ds$estado_civilCat <- as.factor(ds$estado_civilCat)
ds$categoria_preferencia_uno <- as.factor(ds$categoria_preferencia_uno)
ds$categoria_preferencia_dos <- as.factor(ds$categoria_preferencia_dos)


#-------------- 6. Distribución de variables-----------------------------------#

#############################################
#### DISTRIBUCION DE VARIABLES CONTINUAS ####
#############################################
par(mfrow = c(2, 3))
hist(ds$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(ds$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(ds$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(ds$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(ds$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(ds$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1))

###############################################
#### DISTRIBUCION DE VARIABLES CATEGÓRICAS ####
###############################################
par(mfrow = c(2, 2))
plot(ds$sexoCat, main = "Sexo", col = "seashell" )
plot(ds$estado_civilCat, main = "Estado Civil", col = "seashell" )
plot(ds$categoria_preferencia_uno, main = "categoría preferencia uno", col = "seashell" )
plot(ds$categoria_preferencia_dos, main = "categoría preferencia dos", col = "seashell" ) 
par(mfrow = c(1, 1))

#-------------- 7. Transformación de las variables----------------------------# 
#logaritmo natural scalar
c_value <- 1 
ds_d = data.frame(ds[,6:11] + c_value)
head(ds_d, 20)
 
log_scale_ds = log(as.data.frame(ds_d))

head(log_scale_ds, 20)

par(mfrow = c(2, 3))
hist(log_scale_ds$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(log_scale_ds$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(log_scale_ds$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(log_scale_ds$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(log_scale_ds$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(log_scale_ds$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1))

#min-max scalar
norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}

head(norm_minmax)

ds.minmax <- as.data.frame(lapply(ds[,6:11], norm_minmax))

head(ds.minmax)

par(mfrow = c(2, 3))
hist(ds.minmax$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(ds.minmax$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(ds.minmax$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(ds.minmax$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(ds.minmax$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(ds.minmax$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1))



#normal scalar
ds.scale <- as.data.frame(scale(ds[,6:11]))

head(ds.scale, 20)

par(mfrow = c(2, 3))
hist(ds.scale$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(ds.scale$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(ds.scale$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(ds.scale$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(ds.scale$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(ds.scale$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1))

#robust scalar
robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}

head(robust_scalar)

ds.robustscalar <- as.data.frame(lapply(ds[,6:11], robust_scalar))

head(ds.robustscalar)

par(mfrow = c(2, 3))
hist(ds.robustscalar$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(ds.robustscalar$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(ds.robustscalar$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(ds.robustscalar$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(ds.robustscalar$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(ds.robustscalar$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1))

#potencia al cuadrado

ds.potencia <- as.data.frame(ds[,6:11]**2)
head(ds.potencia)

par(mfrow = c(2, 3))
hist(ds.potencia$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(ds.potencia$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(ds.potencia$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(ds.potencia$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(ds.potencia$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(ds.potencia$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1))


#-------------- 8. Determinamos el número de clusters óptimos------------------# 

sumbt <- kmeans(log_scale_ds, centers = 1)$betweenss
for(i in 2:15) sumbt[i] <- kmeans(log_scale_ds, centers = i)$betweenss 
plot(1:15, sumbt, type = "b", xlab = "numero de clusters", ylab = "suma de cuadrados intergrupos")


#-------------- 9. Creación de los clusters------------------# 

set.seed(80) # fijamos semillas
log_scale_ds.km <- kmeans(log_scale_ds, centers = 4) # k means 
names(log_scale_ds.km)
head(log_scale_ds.km)

log_scale_ds.km$cluster #asignación observaciones a clusters
log_scale_ds.km$totss #inercia total 210995
log_scale_ds.km$betweenss #inercia inter grupos, nos interesa que sea el más alto posible 165813.3 
log_scale_ds.km$withinss #inercia intra grupos 4621.807  4911.110  6922.736 28726.052
log_scale_ds.km$tot.withinss #inercia intra gurpos (total), nos interesa que sea el menor posible 45181.7

str(log_scale_ds.km)

#-------------- 10. Visualización de los clusters------------------# 
aggregate(ds[,6:11], by = list(log_scale_ds.km$cluster), mean) #agregado

 
fviz_cluster(log_scale_ds.km, data = log_scale_ds)


#-------------- 11. Unificación------------------# 

tabla = data.frame(ds, log_scale_ds.km$cluster)

tabla1 = filter(tabla, log_scale_ds.km.cluster == 1)
tabla2 = filter(tabla, log_scale_ds.km.cluster == 2)
tabla3 = filter(tabla, log_scale_ds.km.cluster == 3)
tabla4 = filter(tabla, log_scale_ds.km.cluster == 4)

head(tabla1)
str(tabla1)

par(mfrow = c(2, 3))
hist(tabla1$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(tabla1$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(tabla1$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(tabla1$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(tabla1$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(tabla1$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1)) 

par(mfrow = c(2, 2)) 
plot(tabla1$sexoCat, main = "Sexo", col = "seashell" )
plot(tabla1$estado_civilCat, main = "Estado Civil", col = "seashell" )
plot(tabla1$categoria_preferencia_uno, main = "categoría preferencia uno", col = "seashell" )
plot(tabla1$categoria_preferencia_dos, main = "categoría preferencia dos", col = "seashell" ) 
par(mfrow = c(1, 1))


par(mfrow = c(2, 3))
hist(tabla2$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(tabla2$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(tabla2$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(tabla2$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(tabla2$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(tabla2$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1)) 

par(mfrow = c(2, 2)) 
plot(tabla2$sexoCat, main = "Sexo", col = "seashell" )
plot(tabla2$estado_civilCat, main = "Estado Civil", col = "seashell" )
plot(tabla2$categoria_preferencia_uno, main = "categoría preferencia uno", col = "seashell" )
plot(tabla2$categoria_preferencia_dos, main = "categoría preferencia dos", col = "seashell" ) 
par(mfrow = c(1, 1))

 

par(mfrow = c(2, 3))
hist(tabla3$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(tabla3$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(tabla3$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(tabla3$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(tabla3$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(tabla3$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1)) 

par(mfrow = c(2, 2)) 
plot(tabla3$sexoCat, main = "Sexo", col = "seashell" )
plot(tabla3$estado_civilCat, main = "Estado Civil", col = "seashell" )
plot(tabla3$categoria_preferencia_uno, main = "categoría preferencia uno", col = "seashell" )
plot(tabla3$categoria_preferencia_dos, main = "categoría preferencia dos", col = "seashell" ) 
par(mfrow = c(1, 1))




par(mfrow = c(2, 3))
hist(tabla4$edadCat, main = "Edad del cliente", col = "orange", xlab = "Edad")
hist(tabla4$partic_preferencia_uno, main = "partic_preferencia_uno", col = "orange")
hist(tabla4$monto_prefrencia_uno, main = "monto_prefrencia_uno", col = "orange")
hist(tabla4$partic_preferencia_dos, main = "partic_preferencia_dos", col = "orange")
hist(tabla4$monto_prefrencia_dos, main = "monto_prefrencia_dos", col = "orange")
hist(tabla4$montoglobal, main = "montoglobal", col = "orange")
par(mfrow = c(1, 1)) 

par(mfrow = c(2, 2)) 
plot(tabla4$sexoCat, main = "Sexo", col = "seashell" )
plot(tabla4$estado_civilCat, main = "Estado Civil", col = "seashell" )
plot(tabla4$categoria_preferencia_uno, main = "categoría preferencia uno", col = "seashell" )
plot(tabla4$categoria_preferencia_dos, main = "categoría preferencia dos", col = "seashell" ) 
par(mfrow = c(1, 1))















 
