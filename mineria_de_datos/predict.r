# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# SETUP
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('SETUP')
#Clean memmory
rm(list = ls())

#Define working dirs
data_dir <- 'C:/Users/Marcos/Documents/MiM/Mineria de Datos/tp1/data'
script_dir <- 'C:/Users/Marcos/OneDrive - Tienda Nube/MiM/Mineria de Datos/tp/tp1/script'

print('load libraries')
#Load required libraries
source(paste(script_dir,'/utils.R', sep = '')) #My custom made functions
library('caret')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA LOAD (build working data_set)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('DATA LOAD')

#data_set <- readRDS(paste(data_dir,'/data_set_full.rds', sep = ''))
data_set <- readRDS(paste(data_dir,'/data_set_0.2_v3.6.2.1.rds', sep = ''))
model <- readRDS(paste(data_dir,'/', sep = ''))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# PREDICT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('PREDICT')
# Hago la prediccion con los datos de evaluacion
preds_eval <- predict(model,
                      newdata = select(data_set[data_set$train_sample=="evaluation",],-id,-train_sample),
                      type="prob", #Devolveme las probabilidades
                      na.action = na.pass)

# Lo paso a un archivo de texto
options(scipen=10) #para sacar la notacion cientifica
#Creo el data set para hacer submit en kaggle (id, prob)
submit <- data.frame(id=data_set[data_set$train_sample=="evaluation","id"],
                    pred=preds_eval$click)
#Exporto a csv el dataset para subir a kaggle
write.table(submit,
            paste(data_dir,'/preds_submit_',format(Sys.time(), "%Y%m%d%H%M%S"),'.csv', sep = ''),
            sep=",", quote=FALSE, row.names=FALSE)

print('DONE')
