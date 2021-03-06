',# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# SETUP
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Clean memmory
rm(list = ls())
#Define working dirs
data_dir <- 'C:/Users/Marcos/Documents/MiM/Mineria de Datos/tp1/data'
script_dir <- 'C:/Users/Marcos/OneDrive - Tienda Nube/MiM/Mineria de Datos/tp/tp1/script'

#Load required libraries
source(paste(script_dir,'/utils.R', sep = '')) #My custom made functions
library('caret')
library('dplyr')

memory.limit(50000) #Incremento la memoria para que no de error

#DEFINED functions

drop_cols_ <- function(dt, drop_cols) {
  if (length(drop_cols) > 0){
      dt <- dt[, !colnames(dt) %in% drop_cols]
  }
  return (dt)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA LOAD (build working data_set)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('DATA LOAD')
print('DATA LOAD')
data_percentage_to_load <- 0.01

print('Load training data')
data_set <- load_train_data(data_dir,data_percentage_to_load) #30% of data to fit the model

#create a traingin and a validation set
print('Prepare training data')
val_index <- sample(c(1:nrow(data_set)), nrow(data_set)*.1) # 90% para training - 10% validacion
data_set$train_sample <- "training"
data_set[val_index, "train_sample"] <- "validation"  # Se deja septiembre para validación
data_set$Label <- ifelse(data_set$Label, "click", "no_click")
data_set$id <- NA # Needed to merge with eval_set

print('Load evaluation data')
eval_set <- load_csv_data(paste(data_dir,'/ctr_test.csv', sep = ''))
print('Prepare evaluation data')
eval_set$train_sample <- "evaluation" #Needed to merge with data_set
eval_set$Label <- NA #Needed to merge with data_set

print('Join data sets')
data_set <- rbind(data_set,eval_set) #merge training, validation and evaluation data sets
rm(eval_set)

print('Create temp training_set')
training_set <- data_set[data_set$train_sample =="training",]
#data_set <- readRDS(paste(data_dir,'/data_set_mini.rds', sep = ''))

str(data_set)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE ENGINEERING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('FEATURE ENGINEERING')

print('Procesing day & hour')
data_set$hour <- (as.integer(format(as.POSIXct(data_set$auction_time, origin="1970-01-01"), "%H")) + ceiling(data_set$timezone_offset)) %% 24
data_set$day <- as.integer(format(as.POSIXct(data_set$auction_time + data_set$timezone_offset * 3600, origin="1970-01-01"), "%w"))

print('Procesing Creative Size')
data_set$creative_size <- data_set$creative_height * data_set$creative_width

print('Converting categorical variables to numeric')
# data_set$action_categorical_0   <- as.numeric(data_set$action_categorical_0   )
# data_set$action_categorical_1   <- as.numeric(data_set$action_categorical_1   )
data_set$action_categorical_2   <- as.numeric(data_set$action_categorical_2   )
# data_set$action_categorical_3   <- as.numeric(data_set$action_categorical_3   )
data_set$action_categorical_4   <- as.numeric(data_set$action_categorical_4   )
data_set$action_categorical_5   <- as.numeric(data_set$action_categorical_5   )
data_set$action_categorical_6   <- as.numeric(data_set$action_categorical_6   )
data_set$action_categorical_7   <- as.numeric(data_set$action_categorical_7   )
#data_set$action_list_0          <- as.numeric(data_set$action_list_0          )
data_set$action_list_1          <- as.numeric(data_set$action_list_1          )
data_set$action_list_2          <- as.numeric(data_set$action_list_2          )
# data_set$auction_boolean_0      <- as.numeric(data_set$auction_boolean_0      )
data_set$auction_boolean_1      <- as.numeric(data_set$auction_boolean_1      )
data_set$auction_boolean_2      <- as.numeric(data_set$auction_boolean_2      )
data_set$auction_categorical_0  <- as.numeric(data_set$auction_categorical_0  )
# data_set$auction_categorical_1  <- as.numeric(data_set$auction_categorical_1  )
data_set$auction_categorical_10 <- as.numeric(data_set$auction_categorical_10 )
data_set$auction_categorical_11 <- as.numeric(data_set$auction_categorical_11 )
data_set$auction_categorical_12 <- as.numeric(data_set$auction_categorical_12 )
# data_set$auction_categorical_2  <- as.numeric(data_set$auction_categorical_2  )
data_set$auction_categorical_3  <- as.numeric(data_set$auction_categorical_3  )
data_set$auction_categorical_4  <- as.numeric(data_set$auction_categorical_4  )
# data_set$auction_categorical_5  <- as.numeric(data_set$auction_categorical_5  )
data_set$auction_categorical_6  <- as.numeric(data_set$auction_categorical_6  )
data_set$auction_categorical_7  <- as.numeric(data_set$auction_categorical_7  )
# data_set$auction_categorical_8  <- as.numeric(data_set$auction_categorical_8  )
data_set$auction_categorical_9  <- as.numeric(data_set$auction_categorical_9  )
data_set$auction_list_0         <- as.numeric(data_set$auction_list_0         )
data_set$creative_categorical_0 <- as.numeric(data_set$creative_categorical_0 )
data_set$creative_categorical_1 <- as.numeric(data_set$creative_categorical_1 )
data_set$creative_categorical_10<- as.numeric(data_set$creative_categorical_10)
# data_set$creative_categorical_11<- as.numeric(data_set$creative_categorical_11)
# data_set$creative_categorical_12<- as.numeric(data_set$creative_categorical_12)
data_set$creative_categorical_2 <- as.numeric(data_set$creative_categorical_2 )
data_set$creative_categorical_3 <- as.numeric(data_set$creative_categorical_3 )
# data_set$creative_categorical_4 <- as.numeric(data_set$creative_categorical_4 )
data_set$creative_categorical_5 <- as.numeric(data_set$creative_categorical_5 )
data_set$creative_categorical_6 <- as.numeric(data_set$creative_categorical_6 )
# data_set$creative_categorical_7 <- as.numeric(data_set$creative_categorical_7 )
data_set$creative_categorical_8 <- as.numeric(data_set$creative_categorical_8 )
# data_set$creative_categorical_9 <- as.numeric(data_set$creative_categorical_9 )
# data_set$gender                 <- as.numeric(data_set$gender                 )
# data_set$has_video              <- as.numeric(data_set$has_video              )

print('Removing unwanted variables')
useless_cols <- list('auction_time','timezone_offset','creative_width','creative_height','action_categorical_5',
                     'action_categorical_6','action_categorical_7','auction_age','auction_boolean_1','auction_boolean_2',
                      'auction_categorical_10','auction_categorical_2','auction_categorical_4','auction_list_0',
                      'creative_categorical_1','creative_categorical_10','creative_categorical_3',
                      'creative_categorical_5','creative_categorical_8','device_id','device_id_type')
data_set <- drop_cols_(data_set,useless_cols)
str(data_set)

print('saving Data Set')
saveRDS(data_set, file = paste(data_dir,'/data_set_0.4_v3.6.1.rds', sep = ''))
