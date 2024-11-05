# Read data
dfDefault <- read.csv("default.csv")

# Show some basic info about the dataset
head(dfDefault, 10)
summary(dfDefault)
str(dfDefault)

dim(dfDefault)
#To create a boxplot:
  
# Boxplot for 'default' vs 'income'
boxplot(income ~ default, data = dfDefault, main="Boxplot", xlab="Default", ylab="Income")
boxplot(balance ~ default, data = dfDefault, main="Boxplot", xlab="Default", ylab="balance")

# Plotting the scatter plot:
# Scatter plot for 'balance' and 'income'
plot(dfDefault$balance, dfDefault$income, col=ifelse(dfDefault$default=="Yes", "red", "blue"), xlab="Balance", ylab="Income")
legend("topright", legend=c("Yes", "No"), col=c("red", "blue"), pch=1)

#Convert the categorical variable 'default' to numerical:
  
# Convert categorical to numerical
dfDefault$default_Yes <- ifelse(dfDefault$default == "Yes", 1, 0)

#Fit a linear model:
  
# Linear model
linMod <- lm(default_Yes ~ balance, data = dfDefault)
summary(linMod)

# Use the model to make predictions
# y_predLM <- predict(linMod)
dfDefault$y_predLM <- predict(linMod)

# Plot original data points
plot(dfDefault$balance, dfDefault$default_Yes, xlab="Balance", ylab="Default (Yes=1, No=0)", pch=20, col="blue")
# Add regression line to the plot
abline(linMod, col="red")

#================================================================
# Extra investigation OPTIONAL
# Plot predicted values against balance
plot(dfDefault$balance, dfDefault$y_predLM, xlab="Balance", ylab="y_pred", pch=20, col="blue")

plot(dfDefault$balance, dfDefault$y_predLM, xlab="Balance", ylim=c(-0.2, 1.2) ,ylab="y_pred", pch=20, col="blue")

# Add regression line to the plot
# abline(linMod, col="red")

# Plot predicted values (Optional: to see how well the line fits)
# points(dfDefault$balance, dfDefault$y_pred, pch=20, col="green")

# Add a legend
# legend("topright", legend=c("Original", "Predicted"), col=c("blue", "green"), pch=20)
#=========================================================

#Now, let's fit the logistic regression model:

# Fit logistic regression model
logMod <- glm(default_Yes ~ balance, data = dfDefault, family = "binomial")
summary(logMod)

# Extract coefficients
cat("Coefficients: ", coef(logMod), "\n")

# Plot the predicted probabilities:

# Predict probabilities
dfDefault$log_odds <- predict(logMod) # Log_odds 
dfDefault$y_predProb <- predict(logMod, type="response") # Prob values
head(dfDefault,100)

# Scatter plot
plot(dfDefault$balance, dfDefault$y_predProb, col="blue", xlab="Balance", ylab="Predicted Probability")
points(dfDefault$balance, dfDefault$default_Yes, col="red", pch=20)

# Finally, create a confusion matrix and compute other metrics:

# Create confusion matrix
dfDefault$y_pred <- ifelse(dfDefault$y_predProb > 0.5, 1, 0)

confusion <- table(Predicted = dfDefault$y_pred, Actual = dfDefault$default_Yes)

# Print the improved confusion matrix
print(confusion)

# Add more descriptive dimension names
dimnames(confusion) <- list('Predicted' = c('Predicted: No', 'Predicted: Yes'),
                            'Actual' = c('Actual: No', 'Actual: Yes'))

# Print the improved confusion matrix
print(confusion)

# Compute metrics
TP <- confusion[2, 2]
TN <- confusion[1, 1]
FP <- confusion[2, 1]
FN <- confusion[1, 2]

cat("TP: ", TP, "\n")
cat("TN: ", TN, "\n")
cat("FP: ", FP, "\n")
cat("FN: ", FN, "\n")

# Accuracy
accuracy <- (TP + TN) / (TP + TN + FP + FN)
cat("Accuracy: ", accuracy, "\n")

# Sensitivity
sensitivity <- TP / (TP + FN)
cat("Sensitivity: ", sensitivity, "\n")

# Specificity
specificity <- TN / (TN + FP)
cat("Specificity: ", specificity, "\n")
```