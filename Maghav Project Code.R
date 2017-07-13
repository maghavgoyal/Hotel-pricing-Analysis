#Project Title: Hotel Room Pricing Analysis
#Name: Maghav Goyal
#EMail: maghavgoyal@gmail.com
#College: DTU

#Setting the working directory
setwd("C:/Users/ANIL GOYAL/Desktop/New folder (2)/data sets")
city.df<-read.csv(paste("Cities42.csv",sep=""),)
head(city.df)

#Summarizing the data
summary(city.df)

#Omitting the not available data
city1.df<-na.omit(city.df)

#Corrgram
library(corrgram)
corrgram(city.df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)
cor.test(city1.df$RoomRent,city1.df$HasSwimmingPool) #highly correlated
cor.test(city1.df$RoomRent,city1.df$StarRating) #Highly correlated
cor.test(city1.df$RoomRent,city1.df$HotelCapacity)#Highly correlated
#HasSwimmingpool,StarRating and HotelCapacity are the most correlated 

#Boruta for taking out the most important variables
library(Boruta)
set.seed(123)
response<-city.df$RoomRent
bor.results<-Boruta(RoomRent~.,data=city1.df)
city1.df<-na.omit(city.df)

# 3 Rejected and 13 termed as significant
# HasSwimmingpool,StarRating and HotelCapacity are amongst the significant terms

#visualizing the data
boxplot(city1.df$HotelCapacity)
boxplot(city1.df$RoomRent~city1.df$HasSwimmingPool) # Does affect the roomrent
boxplot(city1.df$RoomRent~city1.df$StarRating) #Surely an important factor
boxplot(city1.df$RoomRent~city1.df$HotelCapacity) #As room increase the price decreases


library(ggvis)
ggvis(~RoomRent,~HotelCapacity,fill=~StarRating,data=city1.df)
ggvis(~RoomRent,~StarRating,fill=~HotelCapacity,data=city1.df) # No such trend in the above two plots
ggvis(~RoomRent,~StarRating,fill=~HasSwimmingPool,data=city1.df)# HasSwimmingPool surely affects the price

#Covariance-Variance Matrix for selected columns
x<-city1.df[,c(12,19,20)]
y<-city1.df[,-c(1,2,3,9,10,13,14,15,16)]
cov(x,y)

#Correlations between these selected rows
cor(x,y)

#Training and testing data-set
training<-city1.df[1:10000,]
testing<-city1.df[10001:13224,]


#Random Forest Algorithm  
library(rpart)
library(randomForest)
library(rpart.plot)
model.forest<-randomForest(RoomRent~HasSwimmingPool+StarRating+HotelCapacity,data=training,method="anova",ntree=6,mtry=2)
varImpPlot(model.forest)
#HotelCapacity was the more significant one in this calculation

#Regression models
fit<-lm(RoomRent~StarRating,data = city1.df)
summary(fit)

fit<-lm(RoomRent~Airport,data = city1.df)
summary(fit)                                    

fit<-lm(RoomRent~CityName,data = city1.df)
summary(fit)  

fit<-lm(RoomRent~HotelCapacity ,data = city1.df)
summary(fit)

fit<-lm(RoomRent~FreeWifi,data = city1.df)
summary(fit)                                    #Not a significant factor 

fit<-lm(RoomRent~FreeBreakfast,data = city1.df)
summary(fit)                                    #Not significant

fit<-lm(RoomRent~HasSwimmingPool,data = city1.df)
summary(fit)

fit<-lm(RoomRent~IsWeekend,data =city1.df)
summary(fit)                                         #Not significant 

fit<-lm(RoomRent~IsNewYearEve,data =city1.df)
summary(fit)                                  

fit<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+Airport+IsNewYearEve,data = city1.df)
summary(fit)



                                                    #Hypothesis 1



# Null hypothesis----Mean of prices of Hotels in metro cities are independent of prices of hotels in non-metro cities.
# Alternative hypothesis---not independent

#Converting data into metro and non-metro cities
metro.df<-city1.df[city1.df$IsMetroCity=="1",]
nonmetro.df<-city1.df[city1.df$IsMetroCity=="0",]
t.test(metro.df$RoomRent,nonmetro.df$RoomRent)
#The p-value is less than <0.05 and hence the difference between the variables is significant and hence the null hypothesis can be rejected.

ggvis(~RoomRent,~IsMetroCity,data=city1.df)

#Null hypothesis--The Mean of prices of Hotels in metro cities are greater or equal to the mean prices of hotels in non-metro cities.
# Alternative hypothesis--Metro cities' mean hotel prices are less than non-metro cities' hotel prices.

t.test(metro.df$RoomRent,nonmetro.df$RoomRent,alternative = "less")
# Using alternative="less",we see that the alternative hypothesis is that metro prices are less than non-metro and since p<0.05, null hypothesis is rejected and alternative is coreecet.

                                           
                                                   #Regression model 


fit<-lm(RoomRent~IsMetroCity,data = city1.df)
fit
summary(fit)
#The variable is significant and the coefficient of this variable is negative which shows that as IsMetroCity moves from 0 (Non-Metro city)
# to 1(metro city), the Room Rent decreases.

                                          


                                                #Hypothesis 2


# Null hypothesis---Mean prices of hotels on weeekends in metro cities is dependent on mean prices of hotels on weeekdays in metro cities.
# Alternative Hypothesis---Not independent
metroweekend.df<-metro.df[which(metro.df$IsWeekend=="1"),]
metroweekdays.df<-metro.df[which(metro.df$IsWeekend=="0"),]

ggvis(~RoomRent,~IsWeekend,data = metro.df)

t.test(metroweekend.df$RoomRent,metroweekdays.df$RoomRent)
#As t-test produces a p-value>0.05, hence null hypothesis cannot be rejected.

                                          
                                                #Regression model



fit<-lm(RoomRent~IsWeekend,data = metro.df)
summary(fit)
# Not significant co-efficient, doesnt make much difference.

ggvis(~RoomRent,~IsWeekend,data = nonmetro.df)

#Similarly, no effect on non-metro-cities' hotel prices as well due to weekend factor.
fit<-lm(RoomRent~IsWeekend,data = nonmetro.df)
summary(fit)
#Not significant coefficient, doesnt make much of a differnce.


                                                #Hypothesis 3


#Null- In a tourist destination,mean price of hotels whose distance from the airport is more than 50kms, is independent from the mean prices of hotels whose distance is less than 50km.
#Alternative- It is independent

Tourist.df<-city1.df[city1.df$IsTouristDestination=="1",]

ggvis(~RoomRent,~Airport,data =Tourist.df)

t.test(Tourist.df$RoomRent[Tourist.df$Airport >50],Tourist.df$RoomRent[Tourist.df$Airport <50])
#p-value is less than 0.05, hence the null hypothesis can be rejected.

#Null- In a tourist destination,mean price of hotels whose distance from the airport is more than 50kms, is less than or eqaul to the mean prices of hotels whose distance is less than 50km.
#Alternative- It is Greater

t.test(Tourist.df$RoomRent[Tourist.df$Airport >50],Tourist.df$RoomRent[Tourist.df$Airport <50],alternative = "greater")
#As p-value is less than 0.05, the alternative hypothesis is accepted.

                                   
                                     #Regression model

fit<-lm(RoomRent~Airport,data=Tourist.df)
summary(fit)

#As the coefficient is positive, the room rent increases with distance.



                              #Similarly, checking the hypothesis for a non-tourist destination


nonTourist.df<-city1.df[city1.df$IsTouristDestination=="0",]


t.test(nonTourist.df$RoomRent[nonTourist.df$Airport >25],nonTourist.df$RoomRent[nonTourist.df$Airport <25])
# as p-value is greater than 0.05, the null hypothesis cannot be rejected.

                                                
                                              #Regression Model


fit<-lm(RoomRent~Airport,data=nonTourist.df)
summary(fit)
#As the co-efficient is not significant, we cannot really say anything although the co-efficient is negative.



                                                #Hypothesis 4

#Null-The RoomRent of hotels and HasSwimmingPool are independent of each other. 
#Alternative- Not independent

Swim.df<-city1.df[city1.df$HasSwimmingPool=="1",]
Nonswim.df<-city1.df[city1.df$HasSwimmingPool=="0",]


ggvis(~RoomRent,~HasSwimmingPool,data = city1.df)

t.test(Swim.df$RoomRent,Nonswim.df$RoomRent)
#P-value is less than 0.05, hence the null hypothesis can be rejected.

chisq.test(city1.df$RoomRent,city1.df$HasSwimmingPool)

                                                    #Regression model

fit<-lm(RoomRent~HasSwimmingPool,data = city1.df)
summary(fit)

#The p-value is less than 0.05 and hence the co-efficient is significant. Also the coefficient is positive which indicates that
# As one moves from hotels with no swimming pool to a hotel with a swimming pool, the room rent increases.

#Studying data city-wise
ggvis(~CityName,~RoomRent,data=metro.df)
#RoomRent of hotels in Delhi have the highest roomrent out of all the metro cities.

#Considering all the factors that affect the RoomRent in Delhi
delhi.df<-city1.df[city1.df$CityName=="Delhi",]
delhi1.df<-na.omit(delhi.df)

#Regression models

fit<-lm(RoomRent~StarRating,data = delhi1.df)
summary(fit)

fit<-lm(RoomRent~Airport,data = delhi1.df)
summary(fit)                                    #Not a significant factor in Delhi

fit<-lm(RoomRent~HotelCapacity ,data = delhi1.df)
summary(fit)

fit<-lm(RoomRent~FreeWifi,data = delhi1.df)
summary(fit)                                    #Not a significant factor in Delhi

fit<-lm(RoomRent~FreeBreakfast,data = delhi1.df)
summary(fit)                                    

fit<-lm(RoomRent~HasSwimmingPool,data = delhi1.df)
summary(fit)

fit<-lm(RoomRent~IsWeekend,data =delhi1.df)
summary(fit)                                         #Not significant 

fit<-lm(RoomRent~IsNewYearEve,data =delhi1.df)
summary(fit)                                        #Not significant 

fit<-lm(RoomRent~StarRating+HasSwimmingPool+FreeBreakfast+HotelCapacity,data = delhi1.df)
summary(fit)
#Simple and multiple linear regression tell the same story except for Hotel capacity whose figures have changed.
# Which means that it must be highly correlated to some factor which is highly responsible for the hotel room rent increase.

x<-delhi1.df[,c(19)]
y<-delhi1.df[,c(20,18,13,17,12)]

cor(x,y)
#Hotel capacity was taking the credits of StarRating and HasSwimmingPool in the simple linear regression

ggvis(~RoomRent,~StarRating,fill=~HasSwimmingPool,data=delhi1.df)
#Higher Star rating hotels have swimmingpools and so their RoomRent is more.

ggvis(~CityName,~RoomRent,data=nonmetro.df)
#Jaipur has the highest roomrent and Kanpur has the lowest RoomRent in the non-metro cities.

jaipur.df<-city1.df[city1.df$CityName=="Jaipur",]
jaipur1.df<-na.omit(jaipur.df)

#Regression models

fit<-lm(RoomRent~StarRating,data = jaipur1.df)
summary(fit)

fit<-lm(RoomRent~Airport,data = jaipur1.df)
summary(fit)                                    #Not a significant factor in Jaipur

fit<-lm(RoomRent~HotelCapacity ,data = jaipur1.df)
summary(fit)                                     #Slightly significant

fit<-lm(RoomRent~FreeWifi,data =jaipur1.df)
summary(fit)                                    #Not a significant factor in Delhi

fit<-lm(RoomRent~FreeBreakfast,data =jaipur1.df)
summary(fit)                                    

fit<-lm(RoomRent~HasSwimmingPool,data =jaipur1.df)
summary(fit)

fit<-lm(RoomRent~IsWeekend,data =jaipur1.df)
summary(fit)                                         #Not significant 

fit<-lm(RoomRent~IsNewYearEve,data =jaipur1.df)
summary(fit)                                         #Not significant

fit<-lm(RoomRent~StarRating+HasSwimmingPool+FreeBreakfast+HotelCapacity,data =jaipur1.df)
summary(fit)
#Simple and multiple linear regression tell the same story except for HasSwimmingPool whose figures have changed.
# Which means that it must be highly correlated to some factor which is highly responsible for the hotel room rent increase.

x<-jaipur1.df[,c(20)]
y<-jaipur1.df[,c(19,18,13,17,12)]

cor(x,y)
#HasSwimmingPool was taking the credits of StarRating and HotelCapacity in the simple linear regression.
