# Loading libraries
library(dplyr)
library(ggplot2)


######################### SIMPLE LINEAR REGRESSION ############################
setwd("C:\\Users\\noman\\My Drive\\ABP\\ABP DSBA Batch 03\\Unit 02\\Session 03_LogRegContd_Rcode_decision tree")
# loading data
airbnbData <- read.csv(file = "airbnb_data.csv", header = TRUE)
View(airbnbData)
airbnbData <- airbnbData[airbnbData$city == 'NYC',]
# creating a subset for analysis
airbnbDataSub <- airbnbData[, c("room_type","reviews","overall_satisfaction", "accommodates", "bedrooms", "price")]
airbnbDataSub <- airbnbData %>% select (-c(room_id, survey_id, host_id, city, room_type))

# Price
// one graphics in one row
par(mfrow = c(1, 1))  # Split the plotting panel into a 2 x 2 grid
plot.new()
boxplot(airbnbDataSub$price, ylab = "price")
boxplot(airbnbDataSub$price, horizontal=TRUE,ylab = "price")
boxplot(airbnbDataSub$price, ylim = c(0,400), col = "light blue", ylab = "Price",horizontal=FALSE)

abline(h = median(airbnbDataSub$price))
abline(h = mean(airbnbDataSub$price), col = "grey")

hist(airbnbDataSub$price, breaks = 100, col = "light blue", xlab = "Price")
# hist(airbnbDataSub$price, breaks = 100, col = "light blue", xlab = "Price", xlim = c(0,1000))
sd(airbnbDataSub$price)
abline(v=mean(airbnbDataSub$price), col='red', lwd=3)
abline(v=mean(airbnbDataSub$price) + sd(airbnbDataSub$price), col='brown', lwd=3)
abline(v=mean(airbnbDataSub$price) + 2*sd(airbnbDataSub$price), col='green', lwd=3)
abline(v=mean(airbnbDataSub$price) + 3*sd(airbnbDataSub$price), col='grey', lwd=3)

#reviews
boxplot(airbnbDataSub$reviews,ylab = "reviews")
boxplot(airbnbDataSub$reviews, ylim = c(0,200))
abline(h = median(airbnbDataSub$reviews))
abline(h = mean(airbnbDataSub$reviews), col = "brown")
hist(airbnbDataSub$reviews)
hist(airbnbDataSub$reviews, breaks = 100)

# Others
boxplot(airbnbDataSub$overall_satisfaction, ylab = "overall satisfactions")
boxplot(airbnbDataSub$bedrooms,ylab = "bedrooms")
boxplot(airbnbDataSub$accommodates,ylab = "accommodates")

round(cor(airbnbDataSub),2)
mean(airbnbDataSub$price)
# Exploring relationships


# Now lets start exploring correlations
plot(airbnbDataSub$accommodates,airbnbDataSub$price)

pairs(airbnbDataSub) # as one of the variables are categorical var. So lets use ggpairs
pairs(~ reviews+overall_satisfaction+accommodates+bedrooms+price, data = airbnbDataSub)

if (!require(GGally)) install.packages("GGally")
library(GGally)

ggpairs(airbnbDataSub,
        lower =  list(continuous = wrap("smooth", size = 1))) 
ggpairs(airbnbDataSub, columns=c("reviews","bedrooms"),
        lower =  list(continuous = wrap("smooth", size = 1))) 
# CI = mean +- t value * SE / Sqrt of n

# Simple lin Reg with accommodates
# what is regression?
plot(airbnbDataSub$accommodates,airbnbDataSub$price)
plot(airbnbDataSub$accommodates,airbnbDataSub$price, ylim = c(-100,700), xlim = c(1,13))
abline(h = mean(airbnbDataSub$price), col = "grey")
abline(h = 0, col = "black")
mean(airbnbDataSub$price)

lm.airbnb1 = lm(price ~ accommodates, data=airbnbDataSub)


summary(lm.airbnb1)
confint(lm.airbnb1, level=0.95)
# CI = mean +- t value * SE / Sqrt of n

# interesting results in intercept and accommodates beta

# calculating predicted values and residuals/ errors
predict(lm.airbnb1, data.frame('accommodates' = 8), interval = "prediction")

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



############ END of Simple Linear Regression  ###################
