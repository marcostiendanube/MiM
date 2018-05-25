# equipo_1: nombre del equipo 1
# equipo_2: nombre del equipo 2
# df_equipos: data frame con todos los equipos del mundial 2018 y sus Xs
# nombre: nombre del equipo
# modelo de prediccion ya entrenado.

predecir_partido2 <- function (equipo_1,equipo_2,df_equipos, modelo){
  x_equipo_1 <- filter(df_equipos,pais == equipo_1)
  x_equipo_2 <- filter(df_equipos,pais == equipo_2)
  goles_equipo_1 <- predict(modelo,
                      data.frame(X1 = x_equipo_1$X1,X2 = x_equipo_2$X2,
                        X3 = x_equipo_1$X3,X4 = x_equipo_2$X4,X5 = x_equipo_1$X5,
                        X6 = x_equipo_1$X6,X7 = x_equipo_1$X7,X8 = x_equipo_1$X8,
                        X9 = x_equipo_2$X8,X10 = x_equipo_1$X10,X11 = x_equipo_2$X10))
  goles_equipo_2 <- predict(modelo,
                      data.frame(X1 = x_equipo_2$X1,X2 = x_equipo_1$X2,
                        X3 = x_equipo_2$X3,X4 = x_equipo_1$X4,X5 = x_equipo_2$X5,
                        X6 = x_equipo_2$X6,X7 = x_equipo_2$X7,X8 = x_equipo_2$X8,
                        X9 = x_equipo_1$X8,X10 = x_equipo_2$X10,X11 = x_equipo_1$X10))
  #Aplicamos el factor suerte
  set.seed(2018)
  goles_equipo_1 <- goles_equipo_1 + rnorm(1,0,0.5)
  goles_equipo_2 <- goles_equipo_2 + rnorm(1,0,0.5)

  # ganador {1,2} -> 1: gana Equipo1, 2: gana Equipo2
  ganador <- ifelse(goles_equipo_1 > goles_equipo_2,1,2)
  # goles_equipo_n -> goles redondeado sin decimales
  goles_equipo_1 <- round(goles_equipo_1)
  goles_equipo_2 <- round(goles_equipo_2)
  resultado <- c(ganador,goles_equipo_1,goles_equipo_2)
  return(resultado)
}

entrenar_modelo_de_prediccion <- function(df_entrenamiento){
  #attach(df_entrenamiento)
  modelo <- lm(Y~pot_ataque_obj+pot_defensa_riv+goles_favor_obj+goles_contra_riv+mundial_anterior+mejores_cuatro+cabeza_serie_obj+cabeza_serie_riv+puntaje_fifa_porcent, data = df_entrenamiento)
  # saveRDS(modelo,'modelo.rds')
  summary(modelo)
  return(modelo)
}
