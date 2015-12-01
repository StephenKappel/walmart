require(plyr)

get.stores <- function(){
  stores <- read.csv('../data/stores.csv')
  stores$Store <- as.factor(stores$Store)
  return(stores)
}

get.features <- function(){
  features <- read.csv('../data/features.csv')
  features$Store <- as.factor(features$Store)
  features$Date <- as.Date(features$Date)
  return(features)
}
  
get.train <- function(){
  train <- read.csv('../data/train.csv')
  train$Store <- as.factor(train$Store)
  train$Dept <- as.factor(train$Dept)
  train$Date <- as.Date(train$Date)
  return(train)
}

get.master <- function(){
  train <- get.train()
  stores <- get.stores()
  features <- get.features()
  master <- merge(train, stores, by='Store')
  master <- merge(master, features, by=c('Store', 'Date'))
  master$IsHoliday.y <- NULL  # IsHoliday is in features and train
  master <- rename(master, c('Type'='Store_Type', 'Size'='Store_Size',
                             'IsHoliday.x'='IsHoliday'))
  master <- add.normalized.col(master, 'Weekly_Sales')
  master <- add.normalized.col(master, 'Temperature')
  master <- add.normalized.col(master, 'Unemployment')
  return(master)
}

add.normalized.col <- function(master, col.name){
  master$temp <- master[, col.name]
  store.ply <- ddply(master, 'Store', summarize, Mean_X=mean(temp), 
                     SD_X=sd(temp))
  master <- merge(master, store.ply, by=c('Store'))
  master$Norm_X <- (master[,col.name] - master$Mean_X) / master$SD_X
  master$Mean_X <- NULL
  master$SD_X <- NULL
  master$temp <- NULL
  master <- rename(master, c('Norm_X'=paste('Norm_', col.name, sep='')))
  return(master)
}
