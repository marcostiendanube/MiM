rm(list = ls())

#Load required libraries
source('cfg.r')
source('utils.R')
library('dplyr')
library('FeatureHashing')

data_dir <- 'C:/Users/Marcos/OneDrive - Tienda Nube/MiM/Mineria de Datos/tp/data'
script_dir <- 'C:/Users/Marcos/OneDrive - Tienda Nube/MiM/Mineria de Datos/tp/code'

print('DATA LOAD')

data_set <- readRDS(paste(data_dir,'/data_set_0.1.rds', sep = ''))

#FUNCTIONS
drop_cols_ <- function(dt, drop_cols) {
  if (length(drop_cols) > 0){
      dt <- dt[, !colnames(dt) %in% drop_cols]
  }
  return (dt)
}

drop_zero_var_ <- function(dt,keep_cols=NULL) {
    val_count <- lapply(dt, function(x) length(unique(na.omit(x))))
    zero_var_cols <- names(which(!val_count > 1))
    drop_cols <- setdiff(zero_var_cols, keep_cols)
    dt <- drop_cols_(dt, drop_cols)
    return(dt)
}

drop_nulls_ <- function(dt,keep_cols=NULL) {
  null_count <- lapply(dt, function(x) sum(is.na(x)))
  null_cols <- names(which(null_count > 0))
  drop_cols <- setdiff(null_cols, keep_cols)
  dt <- drop_cols_(dt, drop_cols)
  return(dt)
}

drop_useless_ <- function(dt) {
  useless_cols <- list('auction_time','timezone_offset','creative_width','creative_height')
  dt <- drop_cols_(dt, useless_cols)
  return(dt)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE ENGINEERING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
print('FEATURE ENGINEERING')

print('Procesing day & hour')
data_set$hour <- (as.integer(format(as.POSIXct(data_set$auction_time, origin="1970-01-01"), "%H")) + ceiling(data_set$timezone_offset)) %% 24
data_set$day <- as.integer(format(as.POSIXct(data_set$auction_time + data_set$timezone_offset * 3600, origin="1970-01-01"), "%w"))

print('Procesing Creative Size')
data_set$creative_size <- data_set$creative_height * data_set$creative_width



# print('Converting categorical variables to numeric')
# data_set$action_categorical_0   <- as.numeric(data_set$action_categorical_0   )
# data_set$action_categorical_1   <- as.numeric(data_set$action_categorical_1   )
# data_set$action_categorical_2   <- as.numeric(data_set$action_categorical_2   )
# data_set$action_categorical_3   <- as.numeric(data_set$action_categorical_3   )
# data_set$action_categorical_4   <- as.numeric(data_set$action_categorical_4   )
# data_set$action_categorical_5   <- as.numeric(data_set$action_categorical_5   )
# data_set$action_categorical_6   <- as.numeric(data_set$action_categorical_6   )
# data_set$action_categorical_7   <- as.numeric(data_set$action_categorical_7   )
# data_set$action_list_0          <- as.numeric(data_set$action_list_0          )
# data_set$action_list_1          <- as.numeric(data_set$action_list_1          )
# data_set$action_list_2          <- as.numeric(data_set$action_list_2          )
# data_set$auction_boolean_0      <- as.numeric(data_set$auction_boolean_0      )
# data_set$auction_boolean_1      <- as.numeric(data_set$auction_boolean_1      )
# data_set$auction_boolean_2      <- as.numeric(data_set$auction_boolean_2      )
# data_set$auction_categorical_0  <- as.numeric(data_set$auction_categorical_0  )
# data_set$auction_categorical_1  <- as.numeric(data_set$auction_categorical_1  )
# data_set$auction_categorical_10 <- as.numeric(data_set$auction_categorical_10 )
# data_set$auction_categorical_11 <- as.numeric(data_set$auction_categorical_11 )
# data_set$auction_categorical_12 <- as.numeric(data_set$auction_categorical_12 )
# data_set$auction_categorical_2  <- as.numeric(data_set$auction_categorical_2  )
# data_set$auction_categorical_3  <- as.numeric(data_set$auction_categorical_3  )
# data_set$auction_categorical_4  <- as.numeric(data_set$auction_categorical_4  )
# data_set$auction_categorical_5  <- as.numeric(data_set$auction_categorical_5  )
# data_set$auction_categorical_6  <- as.numeric(data_set$auction_categorical_6  )
# data_set$auction_categorical_7  <- as.numeric(data_set$auction_categorical_7  )
# data_set$auction_categorical_8  <- as.numeric(data_set$auction_categorical_8  )
# data_set$auction_categorical_9  <- as.numeric(data_set$auction_categorical_9  )
# data_set$auction_list_0         <- as.numeric(data_set$auction_list_0         )
# data_set$creative_categorical_0 <- as.numeric(data_set$creative_categorical_0 )
# data_set$creative_categorical_1 <- as.numeric(data_set$creative_categorical_1 )
# data_set$creative_categorical_10<- as.numeric(data_set$creative_categorical_10)
# data_set$creative_categorical_11<- as.numeric(data_set$creative_categorical_11)
# data_set$creative_categorical_12<- as.numeric(data_set$creative_categorical_12)
# data_set$creative_categorical_2 <- as.numeric(data_set$creative_categorical_2 )
# data_set$creative_categorical_3 <- as.numeric(data_set$creative_categorical_3 )
# data_set$creative_categorical_4 <- as.numeric(data_set$creative_categorical_4 )
# data_set$creative_categorical_5 <- as.numeric(data_set$creative_categorical_5 )
# data_set$creative_categorical_6 <- as.numeric(data_set$creative_categorical_6 )
# data_set$creative_categorical_7 <- as.numeric(data_set$creative_categorical_7 )
# data_set$creative_categorical_8 <- as.numeric(data_set$creative_categorical_8 )
# data_set$creative_categorical_9 <- as.numeric(data_set$creative_categorical_9 )
# data_set$gender                 <- as.numeric(data_set$gender                 )
# data_set$has_video              <- as.numeric(data_set$has_video              )
# data_set$device_id_type         <- as.numeric(data_set$device_id_type         )
# data_set$device_id              <- as.numeric(data_set$device_id              )

str(data_set)

keep_cols <- c('id','Label','train_sample')

data_set <- drop_zero_var_(data_set,keep_cols)
gc()
data_set <- drop_nulls_(data_set,keep_cols)
gc()
# data_set <- drop_useless_(data_set)
# gc()

#paste(unlist(colnames(data_set)),collapse='+')
# Label+action_categorical_0+action_categorical_1+action_categorical_2+action_categorical_3+
# action_categorical_4+action_categorical_5+action_categorical_6+action_categorical_7+
# action_list_0+action_list_1+action_list_2+auction_age+auction_bidfloor+auction_boolean_0+
# auction_boolean_1+auction_boolean_2+auction_categorical_0+auction_categorical_1+
# auction_categorical_10+auction_categorical_11+auction_categorical_12+auction_categorical_2+
# auction_categorical_3+auction_categorical_4+auction_categorical_5+auction_categorical_6+
# auction_categorical_7+auction_categorical_8+auction_categorical_9+auction_list_0+
# auction_time+creative_categorical_0+creative_categorical_1+creative_categorical_10+
# creative_categorical_11+creative_categorical_12+creative_categorical_2+creative_categorical_3+
# creative_categorical_4+creative_categorical_5+creative_categorical_6+creative_categorical_7+
# creative_categorical_8+creative_categorical_9+creative_height+creative_width+device_id+
# device_id_type+gender+has_video+timezone_offset+train_sample+id

# action_categorical_0+action_categorical_1+action_categorical_2+action_categorical_3+
# action_categorical_4+action_categorical_5+action_categorical_6+action_categorical_7+
# action_list_0+action_list_1+action_list_2+auction_age+auction_bidfloor+auction_boolean_0+
# auction_boolean_1+auction_boolean_2+auction_categorical_0+auction_categorical_1+
# auction_categorical_10+auction_categorical_11+auction_categorical_12+auction_categorical_2+
# auction_categorical_3+auction_categorical_4+auction_categorical_5+auction_categorical_6+
# auction_categorical_7+auction_categorical_8+auction_categorical_9+auction_list_0+
# creative_categorical_0+creative_categorical_1+creative_categorical_10+
# creative_categorical_11+creative_categorical_12+creative_categorical_2+creative_categorical_3+
# creative_categorical_4+creative_categorical_5+creative_categorical_6+creative_categorical_7+
# creative_categorical_8+creative_categorical_9+device_id+
# device_id_type+gender+has_video

#train con m
size <- hash.size(data_set)
test <- hashed.model.matrix(~ action_categorical_0+action_categorical_1+action_categorical_2+action_categorical_3+
                            action_categorical_4+action_categorical_5+action_categorical_6+action_categorical_7+
                            action_list_0+action_list_1+action_list_2+auction_age+auction_bidfloor+auction_boolean_0+
                            auction_boolean_1+auction_boolean_2+auction_categorical_0+auction_categorical_1+
                            auction_categorical_10+auction_categorical_11+auction_categorical_12+auction_categorical_2+
                            auction_categorical_3+auction_categorical_4+auction_categorical_5+auction_categorical_6+
                            auction_categorical_7+auction_categorical_8+auction_categorical_9+auction_list_0+
                            creative_categorical_0+creative_categorical_1+creative_categorical_10+
                            creative_categorical_11+creative_categorical_12+creative_categorical_2+creative_categorical_3+
                            creative_categorical_4+creative_categorical_5+creative_categorical_6+creative_categorical_7+
                            creative_categorical_8+creative_categorical_9+device_id+
                            device_id_type+gender+has_video,
                            data_set, size, create.mapping = TRUE)

str(data_set)
head(test)

print('saving Data Set')
saveRDS(data_set, file = paste(data_dir,'/data_set_0.1_transformed_1.rds', sep = ''))
