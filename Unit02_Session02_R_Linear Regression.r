# Loading libraries
library(dplyr)
library(ggplot2)

# https://www.kaggle.com/datasets/stevezhenghp/airbnb-price-prediction

######################### SIMPLE LINEAR REGRESSION ############################
setwd("C:\\Users\\noman\\My Drive\\ABP\\ABP DSBA Batch 03\\Unit 02\\Session 03_LogRegContd_Rcode_decision tree")
# loading data
airbnbData <- read.csv(file = "airbnb_data.csv", header = TRUE)
View(airbnbData)
airbnbData <- airbnbData[airbnbData$city == 'NYC',]
# Using dplyr
airbnbDataSub <- airbnbData %>% select (-c(room_id, survey_id, host_id, city, room_type))

# examining cor relationships
round(cor(airbnbDataSub,use = "pairwise.complete.obs"),2) # the 'use' parameter used as bedrooms col has NA

# Exploring further relationships

# Now lets start exploring correlations
plot(airbnbDataSub$accommodates,airbnbDataSub$price)

pairs(airbnbDataSub) 

# we can also use ggpairs for quick and better correlation map
library(GGally)

pairs(~ reviews+overall_satisfaction+accommodates+bedrooms+price, data = airbnbDataSub)
ggpairs(airbnbDataSub, lower =  list(continuous = wrap("smooth", size = 1))) 

# Lets plan to reg analysis of price with accommodates
plot(airbnbDataSub$accommodates,airbnbDataSub$price)

# looks like price has some outliers
# Lets see the boxplot
boxplot(airbnbDataSub$price, ylab = "price")
boxplot(airbnbDataSub$price, horizontal=TRUE,ylab = "price")

# anyways we will deal with it later

# Lets jump into regression
lm.airbnb1 = lm(price ~ accommodates, data=airbnbDataSub)
summary(lm.airbnb1)
summary(lm.airbnb1)

confint(lm.airbnb1, level=0.95)
# CI = mean +- t value * SE / Sqrt of n

# interesting results in intercept and accommodates beta

# calculating predicted values and residuals/ errors
predict(lm.airbnb1, data.frame('accommodates' = 8), interval = "prediction")



head(airbnbDataSub)
airbnbDataSub$a.pred <- predict(lm.airbnb1) # y hat
airbnbDataSub$a.res <- resid(lm.airbnb1) # y actual - y hat
View(airbnbDataSub)

# plotting the simple regression model
plot(airbnbDataSub$accommodates,airbnbDataSub$price, ylim=c(0, 2000),pch = 19)
#plot(airbnbDataSub$accommodates,airbnbDataSub$price, ylim=c(0, 1000),pch = 19)
axis(1,     at = seq(0, 15, by = 1),    tck = 1, lty = 2, col = "gray")
axis(2,     at = seq(0, 2000, by = 500),    tck = 1, lty = 2, col = "gray")
abline(h = mean(airbnbDataSub$price), col="red", lwd=3, lty=2)
abline(lm.airbnb1, col = "blue", lwd = 2)

#Analysis of variance
anova(lm.airbnb1)

# Diagnosis plots/tests
# Common issues with Linear regression

#1. Non-linearity of the relationships
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lm.airbnb1)
# plot(a.pred, a.res, main = "Residuals vs. predicted price", xlab = "Predicted Price", ylab = "Residuals")

#2. Correlation of error terms
install.packages("car")
library(car)
durbinWatsonTest(lm.airbnb1)

#3. Non-constant variance of error terms
plot(lm.airbnb1)

#4. Outliers
plot(lm.airbnb1)
boxplot(airbnbDataSub$price)

# Outliers in Y variable
# removing outliers and doing the regression again
dim(airbnbDataSub) # 854
airbnbDataSub[airbnbDataSub$price>=1000,]
airbnbDataSub_exPrcOutlier <- airbnbDataSub[airbnbDataSub$price<1000,]
dim(airbnbDataSub_exPrcOutlier) # 851
View(airbnbDataSub_exPrcOutlier)

lm.airbnb1 = lm(price ~ accommodates, data=airbnbDataSub)
summary(lm.airbnb1)

lm.airbnb1_clear = lm(price ~ accommodates, data=airbnbDataSub_exPrcOutlier)
summary(lm.airbnb1_clear)

# Winsorizing (if anyone is interested)
IQR<-IQR(airbnbDataSub$price)
quantile(airbnbDataSub$price)
min(airbnbDataSub$price)
max(airbnbDataSub$price)
Q1<-quantile(airbnbDataSub$price)[2] # 1st quartile
Q3<-quantile(airbnbDataSub$price)[4] # 3rd quartile
install.packages("DescTools")
library(DescTools)
airbnbDataSub$priceWin<- Winsorize(airbnbDataSub$price, minval=Q1-1.5*IQR, maxval=Q3+1.5*IQR)
Q1-1.5*IQR
Q3+1.5*IQR
min(airbnbDataSub$priceWin)
max(airbnbDataSub$priceWin)


lm.airbnb1 = lm(price ~ accommodates, data=airbnbDataSub)
summary(lm.airbnb1)
lm.airbnb1_Win = lm(priceWin ~ accommodates, data=airbnbDataSub)
summary(lm.airbnb1_Win)

#5. High-leverage points
# Outliers in X variable
plot(airbnbDataSub$accommodates) # index plot
boxplot(airbnbDataSub$accommodates, ylab = "accommodates") 
dotchart(airbnbDataSub$accommodates, ylab = "accommodates",pch = 19, bg = "green", pt.cex = .5)

#Calculating Cooks distance and removing the data having Cooks distance > 1
dim(airbnbDataSub)
cooksdst <- cooks.distance(lm.airbnb1)
max(cooksdst)
length(cooksdst)
head(cooksdst)
par(mfrow = c(1, 1))

plot(cooksdst, pch=".", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
#plot(cooksdst, pch=".", cex=2, main="Influential Obs by Cooks distance", ylim = c(0,1.2))  # plot cook's distance

abline(h = 1, col="red")  # add cutoff line
text(x=1:length(cooksdst)+1, y=cooksdst, labels=ifelse(cooksdst>4*mean(cooksdst, na.rm=T),names(cooksdst),""), col="red",cex=.5)  # add labels

lm.airbnb1 = lm(price ~ accommodates, data=airbnbDataSub)
summary(lm.airbnb1)

dim(airbnbDataSub)
outliers <- as.numeric(names(cooksdst) [cooksdst > 1] ) # price ~ accommodates has two rows with cook distance high
airbnbData3_noOutl <- subset(airbnbDataSub_exPrcOutlier, !(row.names(airbnbDataSub) %in% outliers))
dim(airbnbData3_noOutl)

lm.airbnb_exOutliers = lm(price ~ accommodates, data=airbnbData3_noOutl)
summary(lm.airbnb_exOutliers)

#6. Multi-Collinearity
 # Applicable for multiple linear regression


############ END of Simple Linear Regression  ###################


############## Multiple Linear Regression #######################

# loading data

# loading data
airbnbData <- read.csv(file = "G:\\My Drive\\ABP\\ABP DSBA Batch 03\\Unit 02\\airbnb_data.csv", header = TRUE)
View(airbnbData)
airbnbData <- na.omit(airbnbData)
airbnbData <- airbnbData[airbnbData$city == 'NYC',]
# Using dplyr
airbnbDataSub <- airbnbData %>% select (-c(room_id, survey_id, host_id, city, room_type))

# Now lets start exploring correlations

pairs(airbnbDataSub) # as one of the variables are categorical var. So lets use ggpairs

# Regression models

lm.airbnb1 = lm(price ~ accommodates, data=airbnbDataSub)
summary(lm.airbnb1)
anova(lm.airbnb1)

lm.airbnb2 = lm(price ~ accommodates + reviews, data=airbnbDataSub)
summary(lm.airbnb2)
anova(lm.airbnb2)
dim(airbnbDataSub)
lm.airbnb2 = lm(price ~ reviews+overall_satisfaction+accommodates+bedrooms, data=airbnbDataSub)
summary(lm.airbnb2)
anova(lm.airbnb2)
confint(lm.airbnb2, level=0.95)

# calculating predicted values and residuals/ errors
# Predict the price for a listing with 'bedrooms' = 2, 'accommodates' = 3, 'reviews' = 60, 'overall_satisfaction' = 3

df <- data.frame('bedrooms' = 1, 'accommodates' = 5, 'reviews' = 52, 'overall_satisfaction' = 4.5)
predict(lm.airbnb2, df, interval = "prediction")
# That means, according to our model, 95% of the houses having the above factors have a price between -143.81 514.27, and the avg is 185.23

#Predicting all the house prices
airbnbDataSub$a.pred <- length(predict(lm.airbnb2)) # y hat
airbnbDataSub$a.res <- resid(lm.airbnb2) # y actual - y hat

plot(airbnbDataSub$a.pred, airbnbDataSub$a.res, main = "Residuals vs. predicted price", xlab = "Predicted Price", ylab = "Residuals")

# Diagnosis plots/tests
# Common issues with Multiple Linear regression

#1. Non-linearity of the relationships
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lm.airbnb2)
plot.show()
#2. Correlation of error terms
durbinWatsonTest(lm.airbnb2)

#3. Non-constant variance of error terms
plot(lm.airbnb2)
lm.airbnb2 = lm(price ~ reviews+overall_satisfaction+accommodates+bedrooms, data=airbnbDataSub)
summary(lm.airbnb2)
# For later, non-lin regression
airbnbDataSub$accommodatesSq = airbnbDataSub$accommodates^2
lm.airbnb2a = lm(price ~ reviews+overall_satisfaction+accommodatesSq+bedrooms, data=airbnbDataSub)
summary(lm.airbnb2a)

#4. Outliers
plot(lm.airbnb1)
boxplot(airbnbDataSub$price)

# Outliers in Y variable
# removing outliers and doing the regression again
dim(airbnbDataSub) # 854
airbnbDataSub[airbnbDataSub$price>1000,]
airbnbDataSub_exPrcOutlier <- airbnbDataSub[airbnbDataSub$price<1000,]
dim(airbnbDataSub_exPrcOutlier) # 851
View(airbnbDataSub_exPrcOutlier)

lm.airbnb2 = lm(price ~ reviews+overall_satisfaction+accommodates+bedrooms, data=airbnbDataSub)
summary(lm.airbnb2)

lm.airbnb2_ExYOutlier = lm(price ~ reviews+overall_satisfaction+accommodates+bedrooms, data=airbnbDataSub_exPrcOutlier)
summary(lm.airbnb2_ExYOutlier)

# Winsorizing
IQR<-IQR(airbnbDataSub$price)
quantile(airbnbDataSub$price)
Q1<-quantile(airbnbDataSub$price)[2] # 1st quartile
Q3<-quantile(airbnbDataSub$price)[4] # 3rd quartile
install.packages("DescTools")
library(DescTools)
airbnbDataSub$priceWin<- Winsorize(airbnbDataSub$price, minval=Q1-1.5*IQR, maxval=Q3+1.5*IQR)

lm.airbnb2 = lm(price ~ reviews+overall_satisfaction+accommodates+bedrooms, data=airbnbDataSub)
summary(lm.airbnb2)
lm.airbnb2_Win = lm(priceWin ~ reviews+overall_satisfaction+accommodates+bedrooms, data=airbnbDataSub)
summary(lm.airbnb2_Win)

#5. High-leverage points
# Outliers in X variable
boxplot(airbnbDataSub$accommodates, ylab = "accommodates") 
dotchart(airbnbDataSub$accommodates, ylab = "accommodates",pch = 19, bg = "green", pt.cex = .5)

#Calculating Cooks distance and removing the data having Cooks distance > 1
dim(airbnbDataSub_exPrcOutlier)
cooksdst <- cooks.distance(lm.airbnb2_ExYOutlier)

max(cooksdst)

plot(cooksdst, pch=".", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 1, col="red")  # add cutoff line
text(x=1:length(cooksdst)+1, y=cooksdst, labels=ifelse(cooksdst>4*mean(cooksdst, na.rm=T),names(cooksdst),""), col="red",cex=1)  # add labels

lm.airbnb2_ExYOutlier = lm(price ~ reviews+overall_satisfaction+accommodates+bedrooms, data=airbnbDataSub_exPrcOutlier)
summary(lm.airbnb2_ExYOutlier)

Xoutliers <- as.numeric(names(cooksdst) [cooksdst > 1] ) 
Xoutliers

#6. Multi-Collinearity

lm.airbnb1 = lm(price ~ reviews, data=airbnbDataSub)
summary(lm.airbnb1)

lm.airbnb2 = lm(price ~ accommodates+reviews, data=airbnbDataSub)
summary(lm.airbnb2)

lm.airbnb2 = lm(price ~ bedrooms, data=airbnbDataSub)
summary(lm.airbnb2)

lm.airbnb2 = lm(price ~ reviews+overall_satisfaction+accommodates+bedrooms, data=airbnbDataSub)
summary(lm.airbnb2)

# VIF
install.packages("car")
library(car)
vif(lm.airbnb1)

lm.airbnb2_noReview = lm(price ~ overall_satisfaction+accommodates+bedrooms, data=airbnbDataSub)
summary(lm.airbnb2_noReview)

