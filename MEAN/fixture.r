#rm(list = ls())
#data_dir <- '/Users/loli/Documents/MiM/2018-Q1/M??todos Estad??sticos Aplicados a Negocios/TP2'
#setwd(data_dir)
#df_grupos <- read.table("grupos_2018.csv", sep = ",", header = TRUE)
#df_grupos <- data.frame(lapply(df_grupos, as.character), stringsAsFactors=FALSE)


source('fixture.r')

predecir_partido2 <- function(e1,e2){
  goles1 <- sample(0:7, 1, replace=TRUE)
  goles2 <- sample(0:7, 1, replace=TRUE)
  return (c(ifelse(goles1>goles2,1,2),goles1,goles2))
}

ganadores_grupos <- function(df_equipos_grupo){
  grupos = c("A", "B", "C", "D", "E", "F", "G", "H")
  df_equipos_grupo$puesto <- 0
  for (grupo in grupos){
    grupo_resultados <- df_equipos_grupo[df_equipos_grupo$grupo == grupo,]
    grupo_resultados$goles <- grupo_resultados$goles_favor - grupo_resultados$goles_contra
    grupo_resultados <- grupo_resultados[order(-grupo_resultados$puntos, -grupo_resultados$goles, -grupo_resultados$goles_favor), ]
    df_equipos_grupo[df_equipos_grupo$pais == grupo_resultados[1,1],]$puesto <- 1
    df_equipos_grupo[df_equipos_grupo$pais == grupo_resultados[2,1],]$puesto <- 2
  }
  df_equipos_grupo <- df_equipos_grupo[order(df_equipos_grupo$grupo, df_equipos_grupo$puesto),]
  return (df_equipos_grupo)
}

fase_de_grupo <- function(df_grupos, df_equipos, modelo){
  df_grupos$puntos <- 0
  df_grupos$goles_favor <- 0
  df_grupos$goles_contra <- 0

  groupos = c("A", "B", "C", "D", "E", "F", "G", "H")
  for (grupo in groupos){
    equipos <- df_grupos[df_grupos$grupo == grupo, 1]
    partidos <- t(combn(equipos,2))
    i = 1
    while(i <= nrow(partidos)){
      e1 <- partidos[i,1]
      e2 <- partidos[i,2]
      resultado <- predecir_partido(e1,e2, df_equipos, modelo)
      df_grupos[df_grupos$pais == e1,]$goles_favor<- df_grupos[df_grupos$pais == e1,]$goles_favor + resultado[2]
      df_grupos[df_grupos$pais == e1,]$goles_contra <- df_grupos[df_grupos$pais == e1,]$goles_contra + resultado[3]
      df_grupos[df_grupos$pais == e2,]$goles_favor<- df_grupos[df_grupos$pais == e2,]$goles_favor + resultado[3]
      df_grupos[df_grupos$pais == e2,]$goles_contra <- df_grupos[df_grupos$pais == e2,]$goles_contra + resultado[2]

      df_grupos[df_grupos$pais == e1,]$puntos <- df_grupos[df_grupos$pais == e1,]$puntos + ifelse(resultado[2] > resultado[3], 3 ,
                                                        ifelse(resultado[2] < resultado[3], 0, 1))
      df_grupos[df_grupos$pais == e2,]$puntos <- df_grupos[df_grupos$pais == e2,]$puntos + ifelse(resultado[3] > resultado[3], 3 ,                                                                                                  
                                                                                                  ifelse(resultado[3] < resultado[2], 0, 1))

      i = i +1
    }
  }
  df_grupos <- ganadores_grupos(df_grupos)
  
  df_grupos <- df_grupos[df_grupos$puesto != 0, c('pais', 'grupo', 'puesto')]
  return(df_grupos)
}

fase_eliminatoria <- function(df_grupos, df_equipos, modelo){
  
  #octavos
  ganador_partido1 <- ifelse(predecir_partido(df_grupos[1,1], df_grupos[4,1], df_equipos, modelo)[1] == 1, df_grupos[1,1] , df_grupos[4,1])
  ganador_partido2 <- ifelse(predecir_partido(df_grupos[5,1], df_grupos[8,1], df_equipos, modelo)[1] == 1, df_grupos[5,1] , df_grupos[8,1])
  ganador_partido3 <- ifelse(predecir_partido(df_grupos[9,1], df_grupos[12,1], df_equipos, modelo)[1] == 1, df_grupos[9,1] , df_grupos[12,1])
  ganador_partido4 <- ifelse(predecir_partido(df_grupos[13,1], df_grupos[16,1], df_equipos, modelo)[1] == 1, df_grupos[13,1] , df_grupos[16,1])
  ganador_partido5 <- ifelse(predecir_partido(df_grupos[2,1], df_grupos[3,1], df_equipos, modelo)[1] == 1, df_grupos[2,1] , df_grupos[3,1])
  ganador_partido6 <- ifelse(predecir_partido(df_grupos[6,1], df_grupos[7,1], df_equipos, modelo)[1] == 1, df_grupos[6,1] , df_grupos[7,1])
  ganador_partido7 <- ifelse(predecir_partido(df_grupos[10,1], df_grupos[11,1], df_equipos, modelo)[1] == 1, df_grupos[10,1] , df_grupos[11,1])
  ganador_partido8 <- ifelse(predecir_partido(df_grupos[14,1], df_grupos[15,1], df_equipos, modelo)[1] == 1, df_grupos[14,1] , df_grupos[15,1])
  
  #cuartos
  ganador_partido9 <- ifelse(predecir_partido(ganador_partido1, ganador_partido2, df_equipos, modelo)[1] == 1, ganador_partido1 , ganador_partido2)
  ganador_partido10 <- ifelse(predecir_partido(ganador_partido5, ganador_partido6, df_equipos, modelo)[1] == 1, ganador_partido5 , ganador_partido6)
  ganador_partido11 <- ifelse(predecir_partido(ganador_partido3, ganador_partido4, df_equipos, modelo)[1] == 1, ganador_partido3 , ganador_partido4)
  ganador_partido12 <- ifelse(predecir_partido(ganador_partido7, ganador_partido8, df_equipos, modelo)[1] == 1, ganador_partido7 , ganador_partido8)
  
  #semis
  partido13 <- predecir_partido(ganador_partido9, ganador_partido11, df_equipos, modelo)
  ganador_partido13 <- ifelse(partido13[1] == 1, ganador_partido9 , ganador_partido11)
  perdedor_partido13 <- ifelse(partido13[1] == 1, ganador_partido11 , ganador_partido9)
  partido14 <- predecir_partido(ganador_partido10, ganador_partido12, df_equipos, modelo)
  ganador_partido14 <- ifelse(partido14[1] == 1, ganador_partido10 , ganador_partido12)
  perdedor_partido14 <- ifelse(partido14[1] == 1, ganador_partido12 , ganador_partido10)
  
  #tercer_puesto
  partido_tercer_puesto <- predecir_partido(perdedor_partido13, perdedor_partido14, df_equipos, modelo)
  cuarto_puesto <- ifelse(partido_tercer_puesto[1] == 1, perdedor_partido14 , perdedor_partido13)
  tercer_puesto <- ifelse(partido_tercer_puesto[1] == 1, perdedor_partido13 , perdedor_partido14)
  
  #final
  partido_final <- predecir_partido(ganador_partido13, ganador_partido14, df_equipos, modelo)
  segundo_puesto <- ifelse(partido_final[1] == 1, ganador_partido14 , ganador_partido13)
  primer_puesto <- ifelse(partido_final[1] == 1, ganador_partido13 , ganador_partido14)

  
  return(c(primer_puesto, segundo_puesto, tercer_puesto, cuarto_puesto))
}



