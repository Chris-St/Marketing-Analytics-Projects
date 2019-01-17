library(gplots)
library(gpairs)
library(corrplot)
library(ggplot2)
library(coefplot)
library(sqldf)

adv_sales <- read.csv("C:\\Users\\cfpemw\\Documents\\MMA 2018\\MMA 831\\adv_sales.csv")
str(adv_sales)

#DATA EXPLORATION

summary(adv_sales)
gpairs(adv_sales)
corrplot.mixed(cor(adv_sales[ , c(2:8)]), upper="ellipse")

#MODELING

train <- sqldf('select * from adv_sales where townID<=750')
test <- sqldf('select * from adv_sales where townID>750')

m1 <- lm(sales~price, train)
summary(m1)

test$m1 <- predict(m1,test)
percent.errors <- abs((test$sales-test$m1)/test$sales)*100 
mean(percent.errors)

R2.m1 <- 1 - (sum((test$sales-test$m1)^2)/sum((test$sales-mean(test$sales))^2))
R2.m1

m2 <- lm(sales~price+store, train)
summary(m2)

test$m2 <- predict(m2,test)
percent.errors2 <- abs((test$sales-test$m2)/test$sales)*100 
mean(percent.errors2)

R2.m2 <- 1 - (sum((test$sales-test$m2)^2)/sum((test$sales-mean(test$sales))^2))
R2.m2

m3 <- lm(sales~price+store+billboard, train)
summary(m3)

test$m3 <- predict(m3,test)
percent.errors3 <- abs((test$sales-test$m3)/test$sales)*100 
mean(percent.errors3)

R2.m3 <- 1 - (sum((test$sales-test$m3)^2)/sum((test$sales-mean(test$sales))^2))
R2.m3

m4 <- lm(sales~price+store+billboard+printout, train)
summary(m4)

test$m4 <- predict(m4,test)
percent.errors4 <- abs((test$sales-test$m4)/test$sales)*100 
mean(percent.errors4)

R2.m4 <- 1 - (sum((test$sales-test$m4)^2)/sum((test$sales-mean(test$sales))^2))
R2.m4

m5 <- lm(sales~price+store+billboard+printout+sat, train)
summary(m5)

test$m5 <- predict(m5,test)
percent.errors5 <- abs((test$sales-test$m5)/test$sales)*100 
mean(percent.errors5)

R2.m5 <- 1 - (sum((test$sales-test$m5)^2)/sum((test$sales-mean(test$sales))^2))
R2.m5

m6 <- lm(sales~price+store+billboard+printout+sat+comp, train)
summary(m6)
par(mfrow=c(2,2))
plot(m6)

test$m6 <- predict(m6,test)
percent.errors6 <- abs((test$sales-test$m6)/test$sales)*100 
mean(percent.errors6)

R2.m6 <- 1 - (sum((test$sales-test$m6)^2)/sum((test$sales-mean(test$sales))^2))
R2.m6

#add pairwise interactions

m7 <- lm(sales~price+store+billboard+printout+sat+comp+store:billboard+store:printout+billboard:printout, train)
summary(m7)

test$m7 <- predict(m7,test)
percent.errors7 <- abs((test$sales-test$m7)/test$sales)*100 
mean(percent.errors7)

R2.m7 <- 1 - (sum((test$sales-test$m7)^2)/sum((test$sales-mean(test$sales))^2))
R2.m7

#Removing insignificant variables

m8 <- lm(sales~price+store+sat+comp+store:billboard, train)
summary(m8)
par(mfrow=c(2,2))
plot(m8)

test$m8 <- predict(m8,test)
percent.errors8 <- abs((test$sales-test$m8)/test$sales)*100 
mean(percent.errors8)

R2.m8 <- 1 - (sum((test$sales-test$m8)^2)/sum((test$sales-mean(test$sales))^2))
R2.m8

#INSIGHTS

par(mfrow=c(1,1))
plot(sales~price, data=adv_sales, xlab="Price", ylab="Sales")
abline(lm(sales~price,adv_sales), col="blue")

plot(sales~comp, data=adv_sales, xlab="Competitor Ad Spend", ylab="Sales")
abline(lm(sales~comp,adv_sales), col="red")

plot(sales~sat, data=adv_sales, xlab="Satisfaction Level", ylab="Sales")
abline(lm(sales~sat,adv_sales), col="green")

par(mfrow=c(1,3))

plot(sales~store, data=adv_sales, xlab="In Store Ad Spend", ylab="Sales")
abline(lm(sales~store,adv_sales), col="blue")

plot(sales~billboard, data=adv_sales, xlab="Billboard Ad Spend", ylab="Sales")
abline(lm(sales~billboard,adv_sales), col="blue")

plot(sales~printout, data=adv_sales, xlab="Printout Ad Spend", ylab="Sales")
abline(lm(sales~printout,adv_sales), col="blue")