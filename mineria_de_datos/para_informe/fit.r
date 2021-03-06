# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# SETUP
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Clean memmory
rm(list = ls())

#Define working dirs
data_dir <- 'C:/Users/Marcos/OneDrive - Tienda Nube/MiM/Mineria de Datos/tp/data'
script_dir <- 'C:/Users/Marcos/OneDrive - Tienda Nube/MiM/code/MiM/mineria_de_datos/best_result_2'

# script_dir <- '/mnt/c/Users/Marcos/OneDrive - Tienda Nube/MiM/code/MiM/mineria_de_datos/best_result_2'
# data_dir <- '/mnt/c/Users/Marcos/OneDrive - Tienda Nube/MiM/Mineria de Datos/tp/data'
#Load required libraries
source(paste(script_dir,'/utils.R', sep = '')) #My custom made functions
library('caret')
library('dplyr')


memory.limit(50000) #Incremento la memoria para que no de error
#Parece que es mas lento.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA LOAD (build working data_set)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('DATA LOAD')

#data_set <- readRDS(paste(data_dir,'/data_set_full.rds', sep = ''))
data_set <- readRDS(paste(data_dir,'/data_set_0.1.rds', sep = ''))
str(data_set)

prop.table(table(data_set$Label,data_set$creative_size))

library(ggplot2)
ggplot(data_set[data_set$train_sample!="evaluation",], aes(x=auction_bidfloor, fill=Label)) +
  geom_density(alpha=0.5) + xlim(0,5) + ylim(0,1)


ggplot(data_set[data_set$train_sample!="evaluation",], aes(x=auction_categorical_7, fill=Label)) +
  geom_density(alpha=0.5)

ggplot(data_set, aes(x=creative_height,y=creative_width)) +
  geom_point() + geom_smooth(method=lm)

plot(data_set$Label)

hist(data_set[data_set$train_sample!="evaluation",]$auction_categorical_7)

hist(table(data_set$Label,data_set$creative_size))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# MODEL FIT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Creo los indices de validacion y entrenamiento
# indexIn <- list()
# indexIn[["tr1"]] <- which(data_set$train_sample=="training")  # Indico con qué entrenar
# indexOut <- list()
# indexOut[["tr1"]] <- which(data_set$train_sample=="validation")  # Indico con qué validar
#
#CREO EL MODELO
fitControl <- trainControl(method="LGOCV",  # Esto da igual
                           # number = 5,
                           # repeats = 3,
                           # index=indexIn, #Voy a entrenar con estas filas
                           # indexOut=indexOut, #voy a validar con estas filas
                           verboseIter=TRUE,
                           returnData=FALSE, # No le pido que me devuelve el dataset OJO! dataset muy grandes
                           summaryFunction=twoClassSummary, #Te devuelve el area bajo la curva de ROC
                           classProbs=TRUE)

#Armo todas las combinaciones posibles de hiperparametros -> Grilla.
xgbGrid <- expand.grid(nrounds = c(100),
                       max_depth = c(10),
                       eta = c(0.2),
                       gamma = c(1),
                       colsample_bytree = c(0.75),
                       min_child_weight = c(10),
                       subsample = c(0.95))

# Entreno el modelo con una seleccion de 15 combinaciones de la grilla
xgbFit <- train(Label ~ ., data=select(data_set[data_set$train_sample!="evaluation",],-id,-train_sample),
                method = "xgbTree",
                trControl = fitControl,
                tuneGrid = xgbGrid, #Sampleo 15 puntos de la grilla de hiperparametros
                metric = "ROC",
                na.action = na.pass,
                allowParallel=TRUE)

print('BEST FITTING RESULT')
print(xgbFit$results[which.max(xgbFit$results$ROC),]) #Imprimo el ROC optimo del modelo.
write.table(varImp(xgbFit)$importance,paste(data_dir,'/var_importance_model_',format(Sys.time(), "%Y%m%d%H%M%S"),'.csv', sep = ''),
           sep=',', quote=FALSE)
write.table(xgbFit$results[which.max(xgbFit$results$ROC),],paste(data_dir,'/best_auc_model_',format(Sys.time(), "%Y%m%d%H%M%S"),'.csv', sep = ''),
           sep=',', quote=FALSE, row.names=FALSE)
saveRDS(xgbFit, file = paste(data_dir,'/model_',format(Sys.time(), "%Y%m%d%H%M%S"),'.rds', sep = ''))
# png(paste(data_dir,'/model_',format(Sys.time(), "%Y%m%d%H%M%S"),'.png', sep = ''))
# plot(varImp(xgbFit)) #Veo la importancia de las variables
# dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# PREDICT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('PREDICT')
# Hago la prediccion con los datos de evaluacion
preds_eval <- predict(xgbFit,
                      newdata = select(data_set[data_set$train_sample=="evaluation",],-id,-train_sample),
                      type="prob", #Devolveme las probabilidades
                      na.action = na.pass)

# Lo paso a un archivo de texto
options(scipen=10) #para sacar la notacion cientifica
#Creo el data set para hacer submit en kaggle (id, prob)
submit <- data.frame(id=data_set[data_set$train_sample=="evaluation","id"],
                    pred=preds_eval$click)
#Exporto a csv el dataset para subir a kaggle
write.table(submit,
            paste(data_dir,'/preds_submit_',format(Sys.time(), "%Y%m%d%H%M%S"),'.csv', sep = ''),
            sep=",", quote=FALSE, row.names=FALSE)

print('DONE')
