# VERSION 01 -> without train and test
# Load the required package for decision trees
library(tree)

# Setting the seed ensures reproducibility of the random sampling that we do later
set.seed(15)
setwd("C:\\Users\\noman\\My Drive\\ABP\\ABP DSBA Batch 03\\Unit 02\\Session 04_decision tree")
# Read the data into a variable
# Note: Replace the path with the location where you've stored uscrime.txt
crimeData <- read.table("uscrime.txt", header = TRUE)
# dimension of the dataset
dim(crimeData)
# Define controls for tree building. This is optional but can help in better tree formation
# control = tree.control(nobs = length(train_dataset), mindev = 0)

# Build the regression tree model based on the training dataset
# Crime is the response variable, and we include all other variables as predictors
regTreeMod <- tree(Crime ~ ., data = crimeData)
print(regTreeMod)
mean(crimeData$Crime) # 905.1

##############################
# 1) root 47 6881000  905.1
#   2) Po1 < 7.65 23  779200  669.6
#     4) Pop < 22.5 12  243800  550.5
#       8) LF < 0.5675 7   48520  466.9 *
#       9) LF > 0.5675 5   77760  667.6 *
#     5) Pop > 22.5 11  179500  799.5 *
#   3) Po1 > 7.65 24 3604000 1131.0
#     6) NW < 7.65 10  557600  886.9
#      12) Pop < 21.5 5  146400 1049.0 *
#      13) Pop > 21.5 5  147800  724.6 *
#     7) NW > 7.65 14 2027000 1305.0
#      14) Po1 < 9.65 6  170800 1041.0 *
#      15) Po1 > 9.65 8 1125000 1503.0 *

# the root node of your decision tree includes all 47 observations from your dataset, the model has a high deviance (the value after 47; indicating room for improvement), and the average value of your target variable across all these observations is 905.1. As the tree makes splits, you would expect the deviance to decrease, indicating a better fit to the data. You would also expect the average value of the target variable to change as the tree makes splits, since the average value of the target variable in each node is the predicted value for that node.

# In a regression tree, deviance at a node is calculated based on the sum of squared residuals for the observations in that node. The residual for each observation is the difference between the actual value of the response variable and the predicted value (which, at the root node, is usually the mean of the response variable for all observations in that node).

###############################

# Display a summary of the initial tree
summary(regTreeMod)

################################
# Regression tree:
# tree(formula = Crime ~ ., data = crimeData)
# Variables actually used in tree construction:
# [1] "Po1" "Pop" "LF"  "NW"
# Number of terminal nodes:  7
# Residual mean deviance:  47390 = 1896000 / 40
# Distribution of residuals:
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -573.900  -98.300   -1.545    0.000  110.600  490.100
################################
######################
# Q: Why divided by 40, not 47 for calculating residual mean deviance?
# Answer: 
# The calculation of the residual mean deviance (which is essentially the sum of squared residuals divided by the degrees of freedom) is based on the number of terminal nodes, not directly on the total number of observations in the dataset. The degrees of freedom in this context are typically calculated as the number of observations minus the number of terminal nodes. This is because each terminal node represents a partition of the data where a separate mean (in the case of regression trees) is calculated, which reduces the degrees of freedom accordingly.
#

# Build the tree again with additional controls (optional)
# Here, mincut=4, minsize=8, and mindev=0 control the size and complexity of the tree
regTreeMod <- tree(Crime ~., data = crimeData, control = tree.control(nobs = nrow(crimeData), mincut = 2, minsize = 4, mindev = 0))

################### Alternatively
# Define control parameters
controlParams <- tree.control(nobs = nrow(crimeData), mincut = 2, minsize = 4, mindev = 0)

# Create the regression tree model using the control parameters
regTreeMod <- tree(Crime ~ ., data = crimeData, control = controlParams)

###################

# Display a summary of the controlled tree
summary(regTreeMod)

# Plot the tree
plot(regTreeMod)
text(regTreeMod, pretty = 2, cex = 0.7)
title("Classification Tree - uscrime.csv - Training set")

# Cross-validation to determine the best tree size
cv_crimeData <- cv.tree(regTreeMod)
# Plot deviance (error) against tree size
plot(cv_crimeData$size, cv_crimeData$dev, type = "b")

# Prune the tree to have 4 leaves (change this based on your specific needs)
prune_crime <- prune.tree(regTreeMod, best = 4)
summary(prune_crime)
# Plot and annotate the pruned tree
plot(prune_crime)
text(prune_crime, pretty = 0)

# Predict and compare with the test set
yhat <- predict(prune_crime, newdata = crimeData)
plot(yhat, crimeData$Crime)
abline(1, 1)
# Calculate mean squared error for test data
print(sum((yhat - crimeData$Crime)^2))

print(mean((yhat - crimeData$Crime)^2))


########################################################################
# VERSION 2 -> with train and test
# Load the required package for decision trees
library(tree)

# Setting the seed ensures reproducibility of the random sampling that we do later
set.seed(15)

# Read the data into a variable
# Note: Replace the path with the location where you've stored uscrime.txt
crimeData <- read.table("C:/Users/chowd/Dropbox/ABP/ABP DSBA Batch 01/Unit 02/uscrime.txt", header = TRUE)

# Create a sampling index
# This randomly divides the dataset into a training set (70%) and a testing set (30%)
sample <- sample(c(TRUE,FALSE), nrow(crimeData), replace=TRUE, prob=c(0.7,0.3))

# Create the training dataset using the sampling index
train_dataset <- crimeData[sample, ]

# Create the testing dataset using the inverse of the sampling index
test_dataset <- crimeData[!sample, ]

# Define controls for tree building. This is optional but can help in better tree formation
# control = tree.control(nobs = length(train_dataset), mindev = 0)

# Build the regression tree model based on the training dataset
# Crime is the response variable, and we include all other variables as predictors
regTreeMod <- tree(Crime ~ ., data = train_dataset)
# Display a summary of the initial tree
summary(regTreeMod)

# Build the tree again with additional controls (optional)
# Here, mincut=4, minsize=8, and mindev=0 control the size and complexity of the tree
regTreeMod <- tree(Crime ~., data = train_dataset, control = tree.control(nobs = nrow(train_dataset), mincut = 2, minsize = 4, mindev = 0))
# Display a summary of the controlled tree
summary(regTreeMod)

# Plot the tree
plot(regTreeMod)
text(regTreeMod, pretty = 2, cex = 0.7)
title("Classification Tree - uscrime.csv - Training set")

# Cross-validation to determine the best tree size
cv_crimeData <- cv.tree(regTreeMod)
# Plot deviance (error) against tree size
plot(cv_crimeData$size, cv_crimeData$dev, type = "b")

# Prune the tree to have 4 leaves (change this based on your specific needs)
prune_crime <- prune.tree(regTreeMod, best = 4)
# Plot and annotate the pruned tree
plot(prune_crime)
text(prune_crime, pretty = 0)

# Predict and compare with the test set
yhat <- predict(prune_crime, newdata = test_dataset)
plot(yhat, test_dataset$Crime)
abline(1, 1)
# Calculate mean squared error for test data
print(mean((yhat - test_dataset$Crime)^2))

# Predict and compare with the training set
yhat <- predict(prune_crime, newdata = test_dataset)
plot(yhat, train_dataset$Crime)
abline(1, 1)
# Calculate mean squared error for training data
print(mean((yhat - train_dataset$Crime)^2))
