if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
#Importing data
DailySales<-read_csv("Data/DailySales.csv")
View(DailySales)
#Removing unnecessary column
DataForAnalysis<-DailySales%>%select(-Weekday,-Month,-Day,-`annee fiscale`)
#Add a column for weekday and month
DataForAnalysis<-DataForAnalysis%>%mutate(Weekday=weekdays(Date),Month=months(Date))

#Removing NA from Total Sales, Bar Sales, Take out Sales and Poissonerie
sum(is.na(DataForAnalysis$Sales))
sum(is.na(DataForAnalysis$Bar_Sales))
sum(is.na(DataForAnalysis$Retail))
sum(is.na(DataForAnalysis$TakeOutSales))
sum(is.na(DataForAnalysis$Sales_Restaurant))
DataForAnalysis[is.na(DataForAnalysis$Sales),]$Sales<-0
DataForAnalysis[is.na(DataForAnalysis$Bar_Sales),]$Bar_Sales<-0
DataForAnalysis[is.na(DataForAnalysis$Retail),]$Retail<-0
DataForAnalysis[is.na(DataForAnalysis$TakeOutSales),]$TakeOutSales<-0
DataForAnalysis[is.na(DataForAnalysis$Sales_Restaurant),]$Sales_Restaurant<-0

#Creation of the data frame of bank holidays and other event (such as Valentine's Day), to identify them
Event<-c('2015-04-06','2015-05-10','2015-05-18','2015-06-21','2015-06-24','2015-07-01','2015-09-07','2015-10-12','2015-10-31','2015-11-11','2015-12-25','2016-01-01','2016-02-14','2016-03-17','2016-03-28','2016-05-08','2016-05-23','2016-06-19','2016-06-24','2016-07-01','2016-09-05','2016-10-10','2016-10-31','2016-11-11','2016-12-25','2017-01-01','2017-02-14','2017-03-17','2017-04-17','2017-05-14','2017-05-22','2017-06-18','2017-06-24','2017-07-01','2017-09-04','2017-10-09','2017-10-31','2017-11-11','2017-12-24','2018-01-01','2018-02-14','2018-03-17','2018-04-02','2018-04-13','2018-05-21','2018-06-17','2018-06-24','2018-07-01','2018-09-03','2018-10-08','2018-10-31','2018-11-11','2018-12-25','2019-01-01','2019-02-14','2019-03-17','2019-04-22','2019-05-12','2019-05-20','2019-06-16','2019-06-24','2019-07-01','2019-10-02','2019-10-14','2019-10-31','2019-11-11','2019-12-25')
#Add a column for EventDay
DataForAnalysis<-DataForAnalysis%>%mutate(EventDay=ifelse(Date %in% Event,TRUE,FALSE))
#Distribution of Sales by Year
DataForAnalysis%>%group_by(Year)%>%ggplot(aes(x=Date,y=Sales,group=Year))+geom_boxplot()+ggtitle("Sales distribution by Year")
#Distribution of sales by day of the week faceted by Year
DataForAnalysis%>%group_by(Weekday)%>%ggplot(aes(x=Weekday,y=Sales))+geom_boxplot()+facet_wrap(DataForAnalysis$Year)+ggtitle("Sales distribution by Day of the Week for each year")

#package for time series handling
if(!require(timetk)){
  install.packages("timetk")
}
library(timetk)
#package for ARIMA model
if(!require(forecast)){
  install.packages("forecast")
}
library(forecast)
#Creation of the train and test set
train_set<-DataForAnalysis%>%filter(Year<2019)
test_set<-DataForAnalysis%>%filter(Year==2019)
#Creation of the time series data
ts_train<-ts(train_set$Sales,start = c(2016,1),frequency = 365)