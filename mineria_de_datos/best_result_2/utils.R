#Librerias a utilizar
library(glue)
library(data.table) #Es como trabajar con DF, pero mas rapido.


# Funcion para cargar un dataset, con la opcion de seleccionar columnas y samplear un % de los datos.
load_csv_data <- function(csv_file, sample_ratio=1) {
  if (sample_ratio > 1 || sample_ratio <= 0) {
    stop(glue("sample_ratio value must be in (0, 1] not '{sample_ratio}'"))
  }
  print(glue("Reading data in {csv_file}..."))
  # Logging...
  df <- fread(csv_file, header=TRUE, sep=",", stringsAsFactors=TRUE,na.strings="",showProgress=TRUE)
  rows <- nrow(df)
  print(glue("Read {rows} rows..."))
  if (sample_ratio < 1) {
    sample_size <- as.integer(sample_ratio * rows)
    print(glue("Keeping a sample of {sample_size} rows."))
    df <- df[sample(.N, sample_size)]
  }
  return(df)
}

# Funcion para levantar todos los archivos
load_train_data <- function(data_dir, sample_ratio=1) {
  train_days <- seq(15, 21, by=1) # this is too hardcoded for my taste
  dfs <- list()
  for (i in train_days){
    csv_file <- file.path(data_dir,glue("ctr_{i}.csv"))
    df <- load_csv_data(csv_file, sample_ratio = sample_ratio)
    dfs <- rbindlist(list(dfs,df),fill=T,use.names = T)
  }
  rm(df)
  return(dfs)
}

# Funcion para levantar todos los archivos
load_train_data_temporal <- function(data_dir) {
  train_days <- seq(15, 21, by=1) # this is too hardcoded for my taste
  dfs <- list()
  for (i in train_days){

    if(i == 15) sample_ratio = 0.015625
    if(i == 16) sample_ratio = 0.03125
    if(i == 17) sample_ratio = 0.0625
    if(i == 18) sample_ratio = 0.125
    if(i == 19) sample_ratio = 0.25
    if(i == 20) sample_ratio = 0.5
    if(i == 21) sample_ratio = 1

    csv_file <- file.path(data_dir,glue("ctr_{i}.csv"))
    df <- load_csv_data(csv_file, sample_ratio = sample_ratio)
    dfs <- rbindlist(list(dfs,df),fill=T,use.names = T)
  }
  rm(df)
  return(dfs)
}

# Transform Discrete variable with bin counting
# data_set : The data set to use for creating the bins (only training data)
# categorical_var : categorical column name
# result_var : result column name (y)
# succes_val : Result column value to be used as succes for calculating probs.
# Automaticamente usa solo la data de training para generar las categorias.


bin_count <- function(data_set,categorical_var,result_var,succes_val){
  training_data_set <- data_set[train_sample == 'training',]
  props <- prop.table(table(training_data_set[[categorical_var]], training_data_set[[result_var]]),1)
  result <- props[data_set[[categorical_var]],succes_val]

}


#device_id
bin_device_id <- function (data_set){

  training <- data_set[which(data_set$train_sample=="training"),]
  d_ids <- prop.table(table(training$device_id, training$Label), 1)

  #divido los device ids en 4 grupos dependiendo de su prob de click (peores nombres ever)
  super_device_ids <- row.names(d_ids[d_ids[,1] >= 0.8,])
  alto_device_ids <- row.names(d_ids[d_ids[,1] < 0.8 & d_ids[,1] >= 0.5,])
  medio_device_ids <- row.names(d_ids[d_ids[,1] < 0.5 & d_ids[,1] >= 0.1,])
  bajo_device_ids <- row.names(d_ids[d_ids[,1] < 0.1,])

  #agrego nueva columna que tome valores 1/0.8/0.5/0.1 dependiendo del grupo al que pertenece
  result <- ifelse(data_set$device_id %in% super_device_ids, 1 ,
                   ifelse(data_set$device_id %in% alto_device_ids, 0.8,
                          ifelse(data_set$device_id %in% medio_device_ids, 0.5, 0.1)))
}
