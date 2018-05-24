#USO
#Ejecutar desde la consola main.r


rm(list = ls())

library(dplyr)
source('predictor.r')
source('fixture.r')

data_dir <- 'c:/Users/Marcos/OneDrive - Tienda Nube/MiM/Metodos Estadisticos Aplicados a Negocios/tp2'
# data_dir <- '/Users/loli/Documents/MiM/2018-Q1/M??todos Estad??sticos Aplicados a Negocios/TP2'
setwd(data_dir)

#Load working files
df_grupos <- read.table("grupos_2018.csv", sep = ",", header = TRUE)
df_grupos <- data.frame(lapply(df_grupos, as.character), stringsAsFactors=FALSE)
df_entrenamiento <- read.csv('tmp_train_2014.csv')
df_equipos <- read.csv('tmp_eval_2018.csv')

#Train model
modelo <- entrenar_modelo_de_prediccion(df_entrenamiento)

#Predecir fase de grupos
df_grupos <- fase_de_grupo(df_grupos,df_equipos,modelo)

df_grupos
