#USO
#Ejecutar desde la consola main.r

rm(list = ls())

data_dir <- '/Users/loli/Documents/MiM/2018-Q1/M??todos Estad??sticos Aplicados a Negocios/TP2/MiM/MEAN'
setwd(data_dir)

library(dplyr)
source('predictor.r')
source('fixture.r')


#Load working files
df_grupos <- read.table("grupos_2018.csv", sep = ",", header = TRUE)
df_grupos <- data.frame(lapply(df_grupos, as.character), stringsAsFactors=FALSE)
df_equipos <- read.csv("datos_equipos_2018.csv", sep = ";", header = TRUE)
#df_entrenamiento <- read.csv('tmp_train_2014.csv')

df_entrenamiento <- read.csv("datos_mundiales.csv", sep = ";", header = TRUE)
val_index <- sample(c(1:nrow(df_entrenamiento)), nrow(df_entrenamiento)*.1)
df_entrenamiento$puntaje_fifa_porcent <- df_entrenamiento$puntaje_fifa_obj/ df_entrenamiento$puntaje_fifa_riv -1
df_entrenamiento$train_sample <- "training"
df_entrenamiento[val_index, "train_sample"] <- "validation"
training_set <- df_entrenamiento[df_entrenamiento$train_sample =="training",]


#Train model
modelo <- entrenar_modelo_de_prediccion(training_set)

#Predecir fase de grupos
grupos <- fase_de_grupo(df_grupos, df_equipos, modelo)

resultados <- fase_eliminatoria(grupos, df_equipos, modelo)
resultados
