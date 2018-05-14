rm(list = ls())

#Load required libraries
source('cfg.r')
source('utils.R')
library('dplyr')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA LOAD (build working data_set)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('DATA LOAD')
data_percentage_to_load <- 0.1

print('Load training data')
data_set <- load_train_data(data_dir,data_percentage_to_load) #30% of data to fit the model

#create a traingin and a validation set
print('Prepare training data')
val_index <- sample(c(1:nrow(data_set)), nrow(data_set)*.1) # 90% para training - 10% Evaluacion
data_set$train_sample <- "training"
data_set[val_index, "train_sample"] <- "validation"  # Se deja septiembre para validación
data_set$Label <- ifelse(data_set$Label, "click", "no_click")
data_set$Label <- as.factor(data_set$Label)
# data_set$id <- NA # Needed to merge with eval_set

print('saving training set')
saveRDS(data_set, file = paste(data_dir,'/train_set_',data_percentage_to_load,'.rds', sep = ''))

print('Load evaluation data')
eval_set <- load_csv_data(paste(data_dir,'/ctr_test.csv', sep = ''))
eval_set$id <- as.factor(eval_set$id)

print('saving eval set')
saveRDS(data_set, file = paste(data_dir,'/eval_set.rds', sep = ''))
