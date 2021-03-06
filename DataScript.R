if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
#Importing data
DailySales<-read_csv("https://raw.githubusercontent.com/jjamesSt/CapestoneProject/master/Data/DailySales.csv")
glimpse(DailySales)
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
#look at the structure of the data
glimpse(DataForAnalysis)
#filter for 2016,2017,2018 and 2019
DataForAnalysis<-DataForAnalysis%>%filter(Year>=2016)
#Summary statistics for the variable
summary(DataForAnalysis)
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
#Broom-style tidiers for forecast package
if(!require(sweep)){
  install.packages("sweep")
}
library(sweep) 
#Creation of the train and test set
train_set<-DataForAnalysis%>%filter(Year<2019)
test_set<-DataForAnalysis%>%filter(Year==2019)
#Create time series with just Days and total sales for the model trainings
##zoo is a package that allows to index tibble on Date column which is essential for time series
if(!require(zoo)){
  install.packages("zoo")
}
library(zoo)
#Creation of the time series data for the ARIMA model
zoo_train<-train_set%>%select(-Weekday,-Month,-Year,-EventDay,-Retail,-TakeOutSales,-Bar_Sales,-Sales_Restaurant)%>% read.zoo(.,format="%F")
ts_train<-ts(zoo_train,start=c(2016,1),end=c(2018,1),frequency=52)
#Show the time serie
plot(ts_train)
labs(title = "Sales Times Series by year")
#Build ARIMA model
#package for ARIMA model
if(!require(forecast)){
  install.packages("forecast")
}
library(forecast)
#Build ARIMA model
set.seed(412,sample.kind = "Rounding")
fit_arima<-auto.arima(ts_train)
summary(fit_arima)
#see the actual value with the prediction
sw_augment(fit_arima)
#plot the residuals of the models
sw_augment(fit_arima, timetk_idx = TRUE) %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_point(colour = 'blue', size = 2) + 
  geom_line() +
  geom_hline(yintercept = 0, color = "red") + 
  labs(title = "Residual diagnostic") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic()
#try to forecast the next 365 days (2019)
predict_arima<-forecast(fit_arima,h=4)
#Get the forecast value in a table
fcast_tbl <- sw_sweep(predict_arima, timetk_idx = TRUE)
fcast_tbl
#select test set data corresponding to the predicted data
actual_tbl <- test_set%>%filter(Date %in% test_set$Date[seq(1,29)])
#Visualize with actual value
fcast_tbl %>% 
  ggplot(aes(x = index, y = value, color = key)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lo.95,ymax = hi.95),
              fill = '#596DD5', alpha = 0.8,size = 0) +
  #Actual data
  geom_point(data = actual_tbl, aes(x = Date, y = Sales), color = 'red') +
  labs(title = "Sales Forecast: ARIMA", x = "", y = "Canadian Dollard",
       subtitle = "sw_sweep tidies the auto.arima() forecast output") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
#calculate the error
error_arima<- fcast_tbl %>% filter(key == 'forecast') %>% 
  left_join(actual_tbl, by=c("index"="date")) %>% 
  mutate(date = index,actual = price.y, pred = price.x) %>% 
  select(date,actual,pred) %>% 
  mutate(error = actual - pred,
         error_pct = error/actual)
error_arima
#transform Data to time series for linear regression
DataLr<-DataForAnalysis%>%tk_augment_timeseries_signature()
#Define train and test set
trainLr_set<-DataLr%>%filter(Date<'2019-01-01')
trainLr_set<-trainLr_set%>% select(-Retail,-TakeOutSales,-Bar_Sales,-Sales_Restaurant,-EventDay)
testLr_set<-DataLr%>%filter(Date>='2019-01-01')
testLr_set<-testLr_set%>% select(-Retail,-TakeOutSales,-Bar_Sales,-Sales_Restaurant,-EventDay)
str(trainLr_set)
str(testLr_set)
#construct the model
set.seed(42,sample.kind = "Rounding")
fit_Lr<-lm(Sales~.,data=select(trainLr_set,-Date))
summary(fit_Lr)
#make a prediction using the model 
predict_lr<-predict(fit_Lr,newdata=select(testLr_set,-Date))
#calculate the error on each model 
error_lr<-testLr_set%>%select(Date,actual=Sales)%>%
  mutate(pred=predict_lr,
         error=actual-predict_lr,
         error_pct=error/actual)
error_lr
#Plot the prediction against test data
dataToPlot<-testLr_set%>%add_column(pred=predict(fit_Lr,testLr_set)%>%tibble::enframe(name = NULL) %>% pull(value))
dataToPlot %>%
  ggplot(aes(x = Date, y = pred)) +
  geom_line() +
  geom_point()+
  geom_line(data = testLr_set,aes(x=Date,y=Sales),color="red",alpha=0.5)+
  scale_x_date(date_breaks = "1 year", date_labels = "%F") +
  scale_color_manual(
    values = c(
      "weekly_sales" = "black",
      "lm_pred" = "#fdc7d7"
    )
  ) +
  theme_classic() +
  labs(title = "model fit",
       subtitle = "test data: 2019",
       x = "Date",
       y = "daily sales (in CAD)"
  )
