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
  master <- rename(master, c('Type'='Store.Type', 'Size'='Store.Size'))
  return(master)
}
