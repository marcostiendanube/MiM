
rm(list = ls())

library(dplyr)
source('predictor.r')

setwd('c:/Users/Marcos/OneDrive - Tienda Nube/MiM/Metodos Estadisticos Aplicados a Negocios/tp2')

df_entrenamiento <- read.csv('tmp_train_2014.csv')
# str(df_entrenamiento)
modelo <- entrenar_modelo_de_prediccion(df_entrenamiento)

df_equipos <- read.csv('tmp_eval_2018.csv')
# str(df_equipos)
tmp <- predecir_partido('Argentina','Argentina',df_equipos,modelo)

# Veo quien gano comparando los goles
ifelse(tmp[2] == tmp [3],'empataron',
  ifelse(tmp[2] > tmp [3],'Gano Argentina','Gano Alemania'))

# En caso de igualdad de goles veo quien gano para la fase de eliminacion
ifelse(tmp[1] == 1,'Gano Argentina','Gano Alemania')
