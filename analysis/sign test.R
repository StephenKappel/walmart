library(ggplot2)
library(data.table)

source('utility.R')
master <- get.master()
attach(master)
head(master)

hist(Weekly_Sales)
hist(log(Weekly_Sales))

agg <- aggregate(Weekly_Sales ~ Store + Date + Store_Type + IsHoliday + Store_Size,master,mean)
aggT <- data.table(agg)
aggT[, size := ifelse(Store_Size > median(Store_Size), "Large",
              ifelse(Store_Size <= median("Store_Size"), "Small"))]      
wilcox.test(agg$Weekly_Sales, conf.int=TRUE)
wilcox.test(log(agg$Weekly_Sales), conf.int=TRUE)

qqnorm(agg$Weekly_Sales, main="QQ Plot of Weekly Sales")
qqline(agg$Weekly_Sales)

wilcox.test(Weekly_Sales ~ size, data=aggT)
wilcox.test(Weekly_Sales ~ IsHoliday, data=agg)
#At .05 significance level, we conclude that the weekly sales data of large vs. small stores, holdiay and non-holiday are nonidentical populations without assuming normality.

qqnorm(agg$Weekly_Sales[which(agg$Store_Type=="A")], main="QQ Plot of Store A")
qqline(agg$Weekly_Sales[which(agg$Store_Type=="A")])
qqnorm(agg$Weekly_Sales[which(agg$Store_Type=="B")], main="QQ Plot of Store B")
qqline(agg$Weekly_Sales[which(agg$Store_Type=="B")])
qqnorm(agg$Weekly_Sales[which(agg$Store_Type=="C")], main="QQ Plot of Store C")
qqline(agg$Weekly_Sales[which(agg$Store_Type=="C")])

ggplot()+geom_jitter(data=agg,aes(x=Store_Type,y=Store_Size))

StoreA_sales <- agg$Weekly_Sales[which(agg$Store_Type=="A")]
sampleA <- sample(StoreA_sales,500)
StoreB_sales <- agg$Weekly_Sales[which(agg$Store_Type=="B")]
sampleB <- sample(StoreB_sales,500)
StoreC_sales <- agg$Weekly_Sales[which(agg$Store_Type=="C")]
sampleC <- sample(StoreC_sales,500)

t.test(sampleA, sampleB, alternative="greater")
t.test(sampleB, sampleC, alternative="greater")
t.test(sampleA, sampleC, alternative="greater")

wilcox.test(sampleA, sampleB, alternative="greater")
wilcox.test(sampleB, sampleC, alternative="greater")
wilcox.test(sampleA, sampleC, alternative="greater")

# remove NA's for markdown comparison
master <- master[complete.cases(master),]


