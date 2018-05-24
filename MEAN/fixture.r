rm(list = ls())
data_dir <- '/Users/loli/Documents/MiM/2018-Q1/M??todos Estad??sticos Aplicados a Negocios/TP2'
setwd(data_dir)
df_grupos <- read.table("grupos_2018.csv", sep = ",", header = TRUE)
df_grupos <- data.frame(lapply(df_grupos, as.character), stringsAsFactors=FALSE)


fase_de_grupo <- function(df_grupos){
  # Agrega a df_grupo nuevas columnas para los resultados de los partidos
  # df_grupos -> lista de los paises del mundial 2018 con su respectivo grupo
  # pais -> nombre del pais
  # grupo -> letra del grupo
  # puntos -> puntos del equipo (3 x partido ganado, 1 x partido empatado)
  # gf -> goles a favor del equipo
  # gc -> goles en contra del equipo
  df_grupos$puntos <- 0
  df_grupos$goles_favor <- 0
  df_grupos$goles_contra <- 0
  
  groupos = c("A", "B", "C", "D", "E", "F", "G", "H")
  for (grupo in groupos){
    equipos <- df_grupos[df_grupos$grupo == grupo,1]
    partidos <- t(combn(equipos,2))
    i = 1
    while(i <= nrow(partidos)){
      e1 <- partidos[i,1]
      e2 <- partidos[i,2]
      resultado <- predecir_partido(e1,e2)  #TODO change to real
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
  return(df_grupos)
}

predecir_partido <- function(e1, e2){
  print(paste(paste(e1, " vs. "), e2))
  resultado <- c(1,7,2) 
  return(resultado)
}

fase_eliminatoria <- function(df_grupos){
  # Simula toda la fase de eliminacion y devuelve un vector con los paises
  # en los puestos del 1 al 4
  # ganadores -> c(campeon, segundo, tercero, cuarto)
  return(ganadores)
}
