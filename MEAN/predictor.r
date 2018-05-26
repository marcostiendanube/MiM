# equipo_1: nombre del equipo 1
# equipo_2: nombre del equipo 2
# df_equipos: data frame con todos los equipos del mundial 2018 y sus Xs
# nombre: nombre del equipo
# modelo de prediccion ya entrenado.s

predecir_partido <- function (equipo_1,equipo_2,df_equipos, modelo){
  x_equipo_obj <- filter(df_equipos,pais == equipo_1)
  x_equipo_riv <- filter(df_equipos,pais == equipo_2)
  
  goles_equipo_obj <- predict(modelo, data.frame(pot_ataque_obj = x_equipo_obj$pot_ataque,
                                                 pot_defensa_riv = x_equipo_riv$pot_defensa,
                                                 goles_favor_obj = x_equipo_obj$goles_favor,
                                                 goles_contra_riv = x_equipo_riv$goles_contra,
                                                 mundial_anterior = x_equipo_obj$mundial_anterior,
                                                 mejores_cuatro = x_equipo_obj$mejores_cuatro,
                                                 cabeza_serie_obj = x_equipo_obj$cabeza_serie,
                                                 cabeza_serie_riv = x_equipo_riv$cabeza_serie,
                                                 puntaje_fifa_porcent = x_equipo_obj$puntaje_fifa / x_equipo_riv$puntaje_fifa -1))
  
  goles_equipo_riv <- predict(modelo, data.frame(pot_ataque_obj = x_equipo_riv$pot_ataque,
                                                 pot_defensa_riv = x_equipo_obj$pot_defensa,
                                                 goles_favor_obj = x_equipo_riv$goles_favor,
                                                 goles_contra_riv = x_equipo_obj$goles_contra,
                                                 mundial_anterior = x_equipo_riv$mundial_anterior,
                                                 mejores_cuatro = x_equipo_riv$mejores_cuatro,
                                                 cabeza_serie_obj = x_equipo_riv$cabeza_serie,
                                                 cabeza_serie_riv = x_equipo_obj$cabeza_serie,
                                                 puntaje_fifa_porcent = x_equipo_riv$puntaje_fifa / x_equipo_obj$puntaje_fifa -1))
  
  #Aplicamos el factor suerte
  goles_equipo_obj <- goles_equipo_obj + rnorm(1,0,0.5)
  #goles_equipo_riv <- goles_equipo_riv + rnorm(1,0,0.5)
  print("resultados:")
  print(goles_equipo_obj)
  print(goles_equipo_riv)

  # ganador {1,2} -> 1: gana Equipo1, 2: gana Equipo2
  ganador <- ifelse(goles_equipo_obj > goles_equipo_riv,1,2)
  # goles_equipo_n -> goles redondeado sin decimales
  goles_equipo_obj <- ifelse(goles_equipo_obj< 0 , 0, goles_equipo_obj)
  goles_equipo_riv <- ifelse(goles_equipo_riv< 0 , 0, goles_equipo_riv)
  goles_equipo_obj <- round(goles_equipo_obj)
  goles_equipo_riv <- round(goles_equipo_riv)
  resultado <- c(ganador,goles_equipo_obj,goles_equipo_riv)
  return(resultado)
}

entrenar_modelo_de_prediccion <- function(df_entrenamiento){
  #attach(df_entrenamiento)
  val_index <- sample(c(1:nrow(df_entrenamiento)), nrow(df_entrenamiento)*.1)
  df_entrenamiento$puntaje_fifa_porcent <- df_entrenamiento$puntaje_fifa_obj/ df_entrenamiento$puntaje_fifa_riv -1
  
  df_entrenamiento$train_sample <- "training"
  df_entrenamiento[val_index, "train_sample"] <- "validation"
  training_set <- df_entrenamiento[df_entrenamiento$train_sample =="training",]
  
  modelo <- lm(Y~pot_ataque_obj+pot_defensa_riv+goles_favor_obj+goles_contra_riv+mundial_anterior+mejores_cuatro+cabeza_serie_obj+cabeza_serie_riv+puntaje_fifa_porcent, data = df_entrenamiento)
  #sacando pot_ataque_obj sube un poco el R ajustado:
  #modelo <- lm(Y~pot_defensa_riv+goles_favor_obj+goles_contra_riv+mundial_anterior+mejores_cuatro+cabeza_serie_obj+cabeza_serie_riv+puntaje_fifa_porcent, data = df_entrenamiento)
  
  # saveRDS(modelo,'modelo.rds')
  summary(modelo)
  return(modelo)
}
