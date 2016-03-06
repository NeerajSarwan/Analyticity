train=read.csv('training_varanasi_boat_riders.csv')
test=read.csv('test_varanasi_boat_riders.csv')
test$riders_without_passes=0
test$riders_with_passes=0
test$total_riders=0
data=rbind(train,test)
str(data)
data$season=as.factor(data$season)
data$holiday=as.factor(data$holiday)
data$weekday=as.factor(data$weekday)
data$workingday=as.factor(data$workingday)
data$weather=as.factor(data$weather)
data$hour=as.factor(data$hour)

# splitting data into train and test data

train<-data[1:11379,]
test<-data[11380:17379,]

# checking effect of independent variables on riders_with_passes and riders_without_passes simultaneously using boxplot

boxplot(train$total_riders~train$hour,xlabel='hour',ylabel='count of riders')
boxplot(train$total_riders~train$weekday,xlabel='hour',ylabel='count of riders')
boxplot(train$riders_with_passes~train$weekday,xlabel='hour',ylabel='count of riders')
boxplot(train$riders_without_passes~train$weekday,xlabel='hour',ylabel='count of riders')
boxplot(train$riders_with_passes~train$weather,xlabel='hour',ylabel='count of riders')
boxplot(train$riders_without_passes~train$weather,xlabel='hour',ylabel='count of riders')
boxplot(train$riders_with_passes~train$year,xlabel='hour',ylabel='count of riders')
boxplot(train$riders_without_passes~train$year,xlabel='hour',ylabel='count of riders')
boxplot(train$total_riders~train$year,xlabel='hour',ylabel='count of riders')
boxplot(train$riders_with_passes~train$month,xlabel='hour',ylabel='count of riders')
boxplot(train$riders_without_passes~train$month,xlabel='hour',ylabel='count of riders')
boxplot(train$total_riders~train$month,xlabel='hour',ylabel='count of riders')

#checking correation between variables having continuous values- humidity,temp,feel_temp etc

sub=data.frame(train$riders_with_passes,train$riders_without_passes,train$total_riders,train$temp,train$humidity,train$feel_temp,train$windspeed)
cor(sub)

# converting factor into integers to facilitate decision tree implementation

train$hour=as.integer(train$hour)
test$hour=as.integer(test$hour)

# using rpart to implement decision tree algorithm

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# making hour bins

dhour=rpart(riders_with_passes~hour,data=train)
fancyRpartPlot(dhour)

# joing back train and test into data for bin creation

data=rbind(train,test)

data$dp_riders_with_passes[data$hour<7]=1
data$dp_riders_with_passes[data$hour>=20]=2
data$dp_riders_with_passes[data$hour>8 & data$hour<16]=3
data$dp_riders_with_passes[data$hour==7]=4
data$dp_riders_with_passes[data$hour==8]=5
data$dp_riders_with_passes[data$hour==18 | data$hour==19]=6
data$dp_riders_with_passes[data$hour==16 | data$hour==17]=7


dhour=rpart(riders_without_passes~hour,data=train)
fancyRpartPlot(dhour)

data$dp_riders_without_passes[data$hour<9]=1
data$dp_riders_without_passes[data$hour==9]=2
data$dp_riders_without_passes[data$hour>=20]=3
data$dp_riders_without_passes[data$hour>=18 & data$hour<20]=4
data$dp_riders_without_passes[data$hour==10 | data$hour==11]=5
data$dp_riders_without_passes[data$hour>=12 & data$hour<18]=6

# creating temp bins

d=rpart(riders_with_passes~temp,data=train)
fancyRpartPlot(d)

data$temp_part[data$temp<0.31]=1
data$temp_part[data$temp>=0.31 & data$temp<0.49]=2
data$temp_part[data$temp>=0.49 & data$temp<0.69]=3
data$temp_part[data$temp>=0.69]=4

d=rpart(riders_without_passes~temp,data=train)
fancyRpartPlot(d)

data$temp_part_without_passes[data$temp<0.35]=1
data$temp_part_without_passes[data$temp>=0.35 & data$temp<0.47]=2
data$temp_part_without_passes[data$temp>=0.47 & data$temp<0.71]=3
data$temp_part_without_passes[data$temp>=0.71]=4


#creating month bins

d=rpart(riders_with_passes~month,data=train)
fancyRpartPlot(d)

data$month_part_riders_with_passes[data$month<=3]=1
data$month_part_riders_with_passes[data$month>=10]=2
data$month_part_riders_with_passes[data$month>3 & data$month<10]=3

d=rpart(riders_without_passes~month,data=train)
fancyRpartPlot(d)

data$month_part_riders_without_passes[data$month<=2]=1
data$month_part_riders_without_passes[data$month>=10]=2
data$month_part_riders_without_passes[data$month>3 & data$month<10]=4
data$month_part_riders_without_passes[data$month==3]=3


# creating year bins : each year has been divided into 4 quarters


data$year_part[data$year==0]=1
data$year_part[data$year==0 & data$month>3]=2
data$year_part[data$year==0 & data$month>6]=3
data$year_part[data$year==0 & data$month>9]=4
data$year_part[data$year==1]=5
data$year_part[data$year==1 & data$month>3]=6
data$year_part[data$year==1 & data$month>6]=7
data$year_part[data$year==1 & data$month>9]=8


data$day_type=''
data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"

data$weekend=0
data$weekend[data$day==0 | data$day==6 ]=1

# converting discrete valued variables into factors

data$season=as.factor(data$season)
data$year=as.factor(data$year)
data$month=as.factor(data$month)
data$hour=as.factor(data$hour)
data$holiday=as.factor(data$holiday)
data$weekday=as.factor(data$weekday)
data$workingday=as.factor(data$workingday)
data$weather=as.factor(data$weather)


data$logreg=log(data$riders_with_passes+1)
data$logcas=log(data$riders_without_passes+1)

# setting a randomForest


train<-data[1:11379,]
test<-data[11380:17379,]

set.seed(415)
fit1 <- randomForest(logreg ~ hour +workingday+weekday+holiday +temp_part+humidity+feel_temp+windspeed+season+weather+dp_riders_with_passes+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
pred1=predict(fit1,test)
test$logreg=pred1

set.seed(415)
fit2 <- randomForest(logcas ~ hour +workingday+weekday+holiday +temp_part+humidity+feel_temp+windspeed+season+weather+dp_riders_with_passes+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
pred2=predict(fit2,test)
test$logcas=pred2

test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$count=test$casual+test$registered

s<-data.frame(count=test$count)
write.csv(s,file="submit.csv",row.names=FALSE)




