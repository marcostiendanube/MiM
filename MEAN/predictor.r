# equipo_1: nombre del equipo 1
# equipo_2: nombre del equipo 2
# df_equipos: data frame con todos los equipos del mundial 2018 y sus Xs
# nombre: nombre del equipo
# modelo de prediccion ya entrenado.

predecir_partido <- function (equipo_1,equipo_2,df_equipos, modelo){
  x_equipo_1 <- filter(df_equipos,pais == equipo_1)
  x_equipo_2 <- filter(df_equipos,pais == equipo_2)
  goles_equipo_1 <- predict(modelo,
                      data.frame(X1 = x_equipo_1$X1,X2 = x_equipo_2$X2,
                        X3 = x_equipo_1$X3,X4 = x_equipo_2$X4,X5 = x_equipo_1$X5,
                        X6 = x_equipo_1$X6,X7 = x_equipo_1$X7,X8 = x_equipo_1$X8,
                        X9 = x_equipo_2$X9,X10 = x_equipo_1$X10,X11 = x_equipo_2$X11))
  goles_equipo_2 <- predict(modelo,
                      data.frame(X1 = x_equipo_2$X1,X2 = x_equipo_1$X2,
                        X3 = x_equipo_2$X3,X4 = x_equipo_1$X4,X5 = x_equipo_2$X5,
                        X6 = x_equipo_2$X6,X7 = x_equipo_2$X7,X8 = x_equipo_2$X8,
                        X9 = x_equipo_1$X9,X10 = x_equipo_2$X10,X11 = x_equipo_1$X11))
  # ganador {1,2} -> 1: gana Equipo1, 2: gana Equipo2
  ganador <- ifelse(goles_equipo_1 > goles_equipo_2,1,2)
  # goles_equipo_n -> goles redondeado sin decimales
  print(goles_equipo_1)
  print(goles_equipo_2)
  goles_equipo_1 <- round(goles_equipo_1)
  goles_equipo_2 <- round(goles_equipo_2)
  print(goles_equipo_1)
  print(goles_equipo_2)
  resultado <- c(ganador,goles_equipo_1,goles_equipo_2)
  return(resultado)
}

entrenar_modelo_de_prediccion <- function(df_entrenamiento){
  attach(df_entrenamiento)
  modelo <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11)
  # saveRDS(modelo,'modelo.rds')
  summary(modelo)
  return(modelo)
}

#Ejemplo funcionando
library(dplyr)
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
