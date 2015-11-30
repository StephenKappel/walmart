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
  # create a column with normalized weekly sales to allow for more 
  # meaningful analysis of how external factors impact weekly store sales
  store.sales <- ddply(master, 'Store', summarize, Mean_Sales=mean(Weekly_Sales), 
                       SD_Sales=sd(Weekly_Sales))
  master <- merge(master, store.sales, by=c('Store'))
  master$Norm_Weekly_Sales <- (master$Weekly_Sales - master$Mean_Sales) / master$SD_Sales
  master$Mean_Sales <- NULL
  master$SD_Sales <- NULL
  return(master)
}
