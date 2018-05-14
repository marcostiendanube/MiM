# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# SETUP
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('SETUP')
#Clean memmory
rm(list = ls())

#Define working dirs
data_dir <- 'C:/Users/Marcos/OneDrive - Tienda Nube/MiM/Mineria de Datos/tp/data'
script_dir <- 'C:/Users/Marcos/OneDrive - Tienda Nube/MiM/Mineria de Datos/tp/code'

print('load libraries')
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
data_set <- readRDS(paste(data_dir,'/data_set_0.1_transformed.rds', sep = ''))
str(data_set)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# MODEL FIT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('MODEL FIT')

# Creo los indices de validacion y entrenamiento
indexIn <- list()
indexIn[["tr1"]] <- which(data_set$train_sample=="training")  # Indico con qué entrenar
indexOut <- list()
indexOut[["tr1"]] <- which(data_set$train_sample=="validation")  # Indico con qué validar

#CREO EL MODELO
fitControl <- trainControl(method="cv",  # Esto da igual
                           index=indexIn, #Voy a entrenar con estas filas
                           indexOut=indexOut, #voy a validar con estas filas
                           verboseIter=TRUE,
                           returnData=FALSE, # No le pido que me devuelve el dataset OJO! dataset muy grandes
                           summaryFunction=twoClassSummary, #Te devuelve el area bajo la curva de ROC
                           classProbs=TRUE)

models_compare <- data.frame()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# KNN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# print('KNN')
#
# gridK <- expand.grid(k = c(1, 21, 51, 101, 151, 201, 251))
#
# knnFit <- train(Label ~ ., data=select(data_set[data_set$train_sample!="evaluation",],-id,-train_sample),  # En esta caso va a tirar un warning de que la variable train_sample tiene cero varianza, es cierto, pero lo vamos a ignorar.
#                method="knn",
#                trControl = fitControl,
#                preProcess = c("center","scale"), #COmo pre procesa los datos
#                tuneGrid = gridK, #Valores de K que quiero probar
#                metric = "ROC", #Con que voy a validar
#                na.action = na.pass) #Que hacer con los missings
#
# print('BEST FITTING RESULT')
# print(knnFit$results[which.max(knnFit$results$ROC),]) #Busco cual es el mejor K en todos los resultados
# models_compare <- data.frame(model = 'knn', roc = knnFit$results[which.max(knnFit$results$ROC),'ROC'])
# png(paste(data_dir,'/model_knn.png', sep = ''))
# plot(knnFit)
# dev.off()
# saveRDS(knnFit, file = paste(data_dir,'/model_knn.rds', sep = ''))
# gc()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# TREES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# print('Tree')
#
# gridCp <- expand.grid(cp = seq(0, 0.2, 0.005))
#
# rpartFit <- train(Label ~ ., data=select(data_set[data_set$train_sample!="evaluation",],-id,-train_sample),
#                   method = "rpart",
#                   trControl = fitControl,
#                   tuneGrid = gridCp, # Complexity factor (penaliza la profundidad del arbol)
#                   metric = "ROC",
#                   na.action = na.pass)
#
# print(rpartFit$results[which.max(rpartFit$results$ROC),])
# models_compare <- rbind(models_compare,data.frame(model = 'tree', roc = rpartFit$results[which.max(rpartFit$results$ROC),'ROC']))
# png(paste(data_dir,'/model_tree.png', sep = ''))
# plot(rpartFit)
# dev.off()
# saveRDS(rpartFit, file = paste(data_dir,'/model_tree.rds', sep = ''))
# gc()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# BAGGIN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# print('bagging')
#
# # Veo cómo predice bagging
# bagFit <- train(Label ~ ., data=select(data_set[data_set$train_sample!="evaluation",],-id,-train_sample),
#                 method = "treebag", # Aca le digo que haga bagging
#                 trControl = fitControl,
#                 metric = "ROC",
#                 na.action = na.pass)
#
# print(bagFit$results[which.max(bagFit$results$ROC),])
# models_compare <- rbind(models_compare,data.frame(model = 'bagging', roc = bagFit$results[which.max(bagFit$results$ROC),'ROC']))
# saveRDS(bagFit, file = paste(data_dir,'/model_bagging.rds', sep = ''))
# gc()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# RANDOM FOREST
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('random forest')

# Veo cómo predice random forest
rfFit <- train(Label ~ ., data=select(data_set[data_set$train_sample!="evaluation",],-id,-train_sample),
               method = "rf",
               trControl = fitControl,
               tuneLength=3,
               metric = "ROC",
               na.action = na.pass)

print(rfFit$results[which.max(rfFit$results$ROC),])
models_compare <- rbind(models_compare,data.frame(model = 'rf', roc = rfFit$results[which.max(rfFit$results$ROC),'ROC']))
png(paste(data_dir,'/model_rf.png', sep = ''))
plot(rfFit)
dev.off()
saveRDS(rfFit, file = paste(data_dir,'/model_rf.rds', sep = ''))

#plot(varImp(rfFit))  # Importante para entender los datos

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# XGBOOST
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('XGBOOST')
#Armo todas las combinaciones posibles de hiperparametros -> Grilla.
xgbGrid <- expand.grid(nrounds = c(100, 200, 300),
                      max_depth = c(5, 10, 15),
                      eta = c(0.2, 0.3, 0.4),
                      gamma = c(0, 1, 3),
                      colsample_bytree = seq(0.7, 1, by = 0.05),
                      min_child_weight = c(1, 2, 5, 10),
                      subsample = seq(0.5, 1, by = 0.05))

# Entreno el modelo con una seleccion de 15 combinaciones de la grilla
xgbFit <- train(Label ~ ., data=select(data_set[data_set$train_sample!="evaluation",],-id,-train_sample),
                method = "xgbTree",
                trControl = fitControl,
                tuneGrid = xgbGrid[sample(c(1:nrow(xgbGrid)), 5),], #Sampleo 15 puntos de la grilla de hiperparametros
                metric = "ROC",
                na.action = na.pass,
                allowParallel=TRUE)

print('BEST FITTING RESULT')
print(xgbFit$results[which.max(xgbFit$results$ROC),]) #Imprimo el ROC optimo del modelo.
models_compare <- rbind(models_compare,data.frame(model = 'xgb', roc = xgbFit$results[which.max(xgbFit$results$ROC),'ROC']))
png(paste(data_dir,'/model_xgboost.png', sep = ''))
plot(varImp(xgbFit)) #Veo la importancia de las variables
dev.off()
saveRDS(xgbFit, file = paste(data_dir,'/model_xgboost.rds', sep = ''))

print('save models comparison')
saveRDS(models_compare, file = paste(data_dir,'/models_compare.rds', sep = ''))
