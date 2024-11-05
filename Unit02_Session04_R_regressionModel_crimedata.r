library(readr) # to read data

crimeData <- read.delim("uscrime.txt", header = TRUE, sep = "\t", dec = ".")
str(crimeData)
library(dplyr)
glimpse(crimeData)


anyNA(crimeData)

lm.crimeData = lm(Crime ~ M + So + Ed + Po1 + Po2+LF+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob+Time, data=crimeData)
dim(crimeData)
summary(lm.crimeData)
confint(lm.crimeData, level=0.95)

df <- data.frame('M' = 14.0, 'So' = 0, 'Ed' = 10.0, 'Po1' = 12.0, 'Po2' = 15.5, 'LF' = 0.640, 'M.F' = 94.0,
                'Pop' = 150, 'NW' = 1.1, 'U1' = 0.120, 'U2' = 3.6, 'Wealth' = 3200, 'Ineq' = 20.1,
                'Prob' = 0.04, 'Time' = 39.0)

predict(lm.crimeData, df, interval = "confidence")

crimeData$a.pred <- predict(lm.crimeData) # y hat
crimeData$a.res <- resid(lm.crimeData) # y actual - y hat

plot(crimeData$a.pred,crimeData$a.res)

#Analysis of variance
anova(lm.crimeData)
plot(lm.crimeData)

lm.crimeData$coefficients