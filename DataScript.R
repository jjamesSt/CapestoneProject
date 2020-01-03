library(tidyverse)
#Importing Hours worked each day data
TimeWorked <- read_csv("Data/TimeWorked.csv")
#Calculating number of hours worked for each day
TimePerDay<-TimeWorked%>%group_by(Date)%>%summarize(Total_hours=sum(`Qte(hre)`))
#Importing Daily Sales data
DailySales<-read_csv("Data/DailySales.csv")
#Creation of the data set needed for the Analysis Where we have both hours worked and sales by departement
CleanedData<-TimePerDay%>%inner_join(DailySales,by='Date')
#Removing NA from Total Sales, Bar Sales, Take out Sales and Poissonerie
CleanedData[is.na(CleanedData$Sales),]$Sales<-0
CleanedData[is.na(CleanedData$Bar_Sales),]$Bar_Sales<-0
CleanedData[is.na(CleanedData$Retail),]$Retail<-0
#Removing unnecessary column
CleanedData<-CleanedData%>%select(-Weekday,-Month,-Day,-`annee fiscale`)
#Add a column for weekday and month
CleanedData<-CleanedData%>%mutate(Weekday=weekdays(Date),Month=months(Date))
#Add a column for payDay detection on the first and on the 15th of each month
CleanedData<-CleanedData%>%mutate(PayDay=ifelse(day(Date)==1 | day(Date)==15,TRUE,FALSE))
#Creation of the data frame of bank holidays and other event (such as Valentine's Day), to identify them
Event<-c('2015-04-06','2015-05-10','2015-05-18','2015-06-21','2015-06-24','2015-07-01','2015-09-07','2015-10-12','2015-10-31','2015-11-11','2015-12-25','2016-01-01','2016-02-14','2016-03-17','2016-03-28','2016-05-08','2016-05-23','2016-06-19','2016-06-24','2016-07-01','2016-09-05','2016-10-10','2016-10-31','2016-11-11','2016-12-25','2017-01-01','2017-02-14','2017-03-17','2017-04-17','2017-05-14','2017-05-22','2017-06-18','2017-06-24','2017-07-01','2017-09-04','2017-10-09','2017-10-31','2017-11-11','2017-12-24','2018-01-01','2018-02-14','2018-03-17','2018-04-02','2018-04-13','2018-05-21','2018-06-17','2018-06-24','2018-07-01','2018-09-03','2018-10-08','2018-10-31','2018-11-11','2018-12-25','2019-01-01','2019-02-14','2019-03-17','2019-04-22','2019-05-12','2019-05-20','2019-06-16','2019-06-24','2019-07-01','2019-10-02','2019-10-14','2019-10-31','2019-11-11','2019-12-25')
#Add a column for EventDay
CleanedData<-CleanedData%>%mutate(EventDay=ifelse(Date %in% Event,TRUE,FALSE))
#Renaming the name of the column names of the data to something more readable
colnames(CleanedData)<-c("Date","Total_hours","Year","Sales","Total_Retail_sales","Take-out_sales","Bar_sales","Restaurant_sales","Weekday","Month","PayDay","EventDay")
#Adding a column for the HighSeason (opening of the terrasse betwen the 15-04 and 15-10 of each year)
CleanedData<-CleanedData%>%mutate(HighSeason=ifelse(Year==2015 & Date>=('2015-04-15') & Date<=('2015-10-15'),TRUE,ifelse(Year==2016 & Date>='2016-04-15'& Date<='2016-10-15',TRUE,ifelse(Year==2017& Date>='2017-04-15'&Date<='2017-10-15',TRUE,ifelse(Year==2018 & Date>='2018-04-15'& Date<='2018-10-15',TRUE,ifelse(Year==2019& Date>='2019-04-15'&Date<='2019-10-15',TRUE,FALSE))))))
#Distribution of Sales by Year
CleanedData%>%group_by(Year)%>%ggplot(aes(x=Date,y=Sales,group=Year))+geom_boxplot()
#Distribution of sales by day of the week faceted by Year
CleanedData%>%group_by(weekdays(Date))%>%ggplot(aes(x=weekdays(Date),y=Sales,group=weekdays(Date)))+geom_boxplot()+facet_wrap(MotherOfAllData$Year)