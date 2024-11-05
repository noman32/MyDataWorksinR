#####################
# Regression Models #
#####################

# Load necessary libraries
library(ggplot2)
install.packages("caret")
library(caret)
getwd()
setwd("C:/Users/noman/My Drive/ABP/ABP DSBA Batch 03/unit 03/Session 03")
crimeData <- read.delim("uscrime.txt", header = TRUE, sep = "\t", dec = ".")
str(crimeData)
glimpse(crimeData)
"C:/Users/noman/My Drive/ABP/ABP DSBA Batch 02/unit 03/Session 03/uscrime.txt"

# Assuming crimeData is already loaded
# Split the data into training (60%), validation (20%), and test (20%) sets
set.seed(123)
trainIndex <- sample(seq_len(nrow(crimeData)), size = 0.6 * nrow(crimeData))
temp <- setdiff(seq_len(nrow(crimeData)), trainIndex)
validIndex <- sample(temp, size = 0.2 * nrow(crimeData))
testIndex <- setdiff(temp, validIndex)

trainData <- crimeData[trainIndex, ]
dim(trainData)
validData <- crimeData[validIndex, ]
dim(validData)

testData <- crimeData[testIndex, ]

# Fit linear regression model with all predictors (lm1)
lm1 <- lm(Crime ~ ., data = trainData)
summary(lm1)
# Fit linear regression model with selected predictors (lm2)
lm2 <- lm(Crime ~ Ed + Po1 + Ineq + Prob, data = trainData)
summary(lm2)

# Function to calculate MSE and RMSE
calc_metrics <- function(model, data) {
    predictions <- predict(model, newdata = data)
    mse <- mean((data$Crime - predictions)^2)
    rmse <- sqrt(mse)
    return(c(MSE = mse, RMSE = rmse))
}
# we could do the above code out of a function as well
predictions <- predict(lm1, newdata = validData)
mse <- mean((validData$Crime - predictions)^2)
rmse <- sqrt(mse)
mse
rmse

# Calculate MSE and RMSE on validation set for both models
metrics_lm1 <- calc_metrics(lm1, validData)

metrics_lm2 <- calc_metrics(lm2, validData)

# Load necessary libraries
library(caret)

# Create a trainControl object for K-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# K-Fold Cross-Validation on validation set for lm1
set.seed(123)
cv_lm1 <- train(Crime ~ ., data = trainData, method = "lm", trControl = train_control)
cv_lm1_results <- cv_lm1$results

# K-Fold Cross-Validation on validation set for lm2
cv_lm2 <- train(Crime ~ Ed + Po1 + Ineq + Prob, data = trainData, method = "lm", trControl = train_control)
cv_lm2_results <- cv_lm2$results

# Determine the best model based on Cross-Validation results (MSE or RMSE)
if(cv_lm1_results$RMSE < cv_lm2_results$RMSE) {
    best_model <- lm1
} else {
    best_model <- lm2
}

# Calculate metrics on test set for the best model
test_metrics <- calc_metrics(lm2, testData)

summary(lm1)$adj.r.squared
# Prepare a summary table
summary_table <- data.frame(
    Model = c("lm1", "lm2"),
    Adjusted_R_squared = c(summary(lm1)$adj.r.squared, summary(lm2)$adj.r.squared),
    Validation_MSE = c(metrics_lm1["MSE"], metrics_lm2["MSE"]),
    Validation_RMSE = c(metrics_lm1["RMSE"], metrics_lm2["RMSE"]),
    CV_MSE = c(cv_lm1_results$RMSE^2, cv_lm2_results$RMSE^2),
    CV_RMSE = c(cv_lm1_results$RMSE, cv_lm2_results$RMSE),
    Test_MSE = c("-", unname(test_metrics["MSE"])),
    Test_RMSE = c("-", unname(test_metrics["RMSE"]))
)

# Print the summary table
print(summary_table)

###################################################
##### Regression tree #####
###################################################

# Load necessary libraries
library(tree)
library(caret)  # For cross-validation

# Splitting the data (Assuming crimeData is already loaded)
set.seed(123)
trainIndex <- sample(seq_len(nrow(crimeData)), size = 0.6 * nrow(crimeData))
temp <- setdiff(seq_len(nrow(crimeData)), trainIndex)
validIndex <- sample(temp, size = 0.2 * nrow(crimeData))
testIndex <- setdiff(temp, validIndex)

trainData <- crimeData[trainIndex, ]
validData <- crimeData[validIndex, ]
testData <- crimeData[testIndex, ]

# Fit decision trees
regTree1 <- tree(Crime ~ ., data = trainData)
plot(regTree1)
text(regTree1, pretty = 0)
regTree2 <- tree(Crime ~ Ed + Po1 + Ineq + Prob, data = trainData)
control_tree <- tree.control(nobs = nrow(trainData), mincut = 3, minsize = 7, mindev = 0)
regTree3 <- tree(Crime ~ ., data = trainData, control = control_tree)

# Function to calculate MSE and RMSE
calc_tree_metrics <- function(tree, data) {
    predictions <- predict(tree, newdata = data)
    mse <- mean((data$Crime - predictions)^2)
    rmse <- sqrt(mse)
    return(c(MSE = mse, RMSE = rmse))
}

# Calculate MSE and RMSE for each tree on validation set
metrics_tree1 <- calc_tree_metrics(regTree1, validData)
metrics_tree2 <- calc_tree_metrics(regTree2, validData)
metrics_tree3 <- calc_tree_metrics(regTree3, validData)

// Find the best tree
if(metrics_tree1["RMSE"] < metrics_tree2["RMSE"] & metrics_tree1["RMSE"] < metrics_tree3["RMSE"]) {
    best_tree <- regTree1
} else if(metrics_tree2["RMSE"] < metrics_tree1["RMSE"] & metrics_tree2["RMSE"] < metrics_tree3["RMSE"]) {
    best_tree <- regTree2
} else {
    best_tree <- regTree3
}

# so best is regTree2
summary(best_tree)
calc_tree_metrics(regTree2, validData)
#===================================================================================================
# Perform cross-validation
regTreeModCV <- tree(Crime ~ ., data = crimeData) # doing on the full data
cv_tree1 <- cv.tree(regTreeModCV, FUN = prune.tree)
plot(cv_tree1$size, cv_tree1$dev, type = "b")

# Determine best size (number of leaves)
opt_size_tree1 <- which.min(cv_tree1$dev)

# Prune the trees
pruned_tree1 <- prune.tree(regTreeModCV, best = opt_size_tree1)
# Plot and annotate the pruned tree
plot(pruned_tree1)
text(pruned_tree1, pretty = 0)

// use calc_tree_metrics to calculate MSE and RMSE for pruned tree
calc_tree_metrics(pruned_tree1, crimeData)

// which is better? pruned_tree1 or regTree2
calc_tree_metrics(regTree3, crimeData)
calc_tree_metrics(pruned_tree1, crimeData)

# so pruned_tree1 is better
best_tree <- pruned_tree1
# Calculate metrics on test set for the best tree
test_metrics_tree <- calc_tree_metrics(best_tree, testData)

# Prepare summary table
summary_table_trees <- data.frame(
    Tree_Model = c("regTree1", "regTree2", "regTree3", "Best Pruned Tree"),
    Validation_MSE = c(metrics_tree1["MSE"], metrics_tree2["MSE"], metrics_tree3["MSE"], NA),  # Fill NA appropriately
    Validation_RMSE = c(metrics_tree1["RMSE"], metrics_tree2["RMSE"], metrics_tree3["RMSE"], NA),  # Fill NA appropriately
    Test_MSE = c("-", "-", "-", test_metrics_tree["MSE"]),
    Test_RMSE = c("-", "-", "-", test_metrics_tree["RMSE"])
)

print(summary_table_trees)

###################################################
##### Random Forest #####
###################################################

# Load necessary libraries
library(randomForest)
library(caret)  # For cross-validation

###################################################
##### KNN #####
###################################################

# Preparing the Data and Fitting KNN Models

# Load necessary libraries
library(class)  # For KNN
library(caret)  # For data splitting and cross-validation

# Splitting the data into training (60%), validation (20%), and test (20%) sets
set.seed(123)
indexes <- createDataPartition(crimeData$Crime, p = 0.6, list = FALSE)
trainData <- crimeData[indexes, ]
tempData <- crimeData[-indexes, ]
indexes <- createDataPartition(tempData$Crime, p = 0.5, list = FALSE)
validData <- tempData[indexes, ]
testData <- tempData[-indexes, ]

# Normalize the data (KNN requires normalized data)
norm_data <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

# in order to prevent NaN values here is another function
norm_data <- function(x) {
    if(max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
        return(rep(0.5, length(x)))  # or some other default value
    } else {
        return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    }
}

trainData_norm <- as.data.frame(lapply(trainData, norm_data))
validData_norm <- as.data.frame(lapply(validData, norm_data))
testData_norm <- as.data.frame(lapply(testData, norm_data))

# Fit KNN regression model (Example with k=5)
install.packages("FNN") # for KNN regression; for KNN classification use "class"
library(FNN)
knnModel <- knn.reg(train = trainData_norm[, -ncol(trainData_norm)], 
                    test = validData_norm[, -ncol(validData_norm)], 
                    y = trainData_norm[, ncol(trainData_norm)], 
                    k = 5)

# Evaluating KNN Models on Validation Set

# Function to calculate MSE and RMSE for KNN
calc_knn_metrics <- function(predicted, actual) {
    mse <- mean((actual - predicted)^2)
    rmse <- sqrt(mse)
    return(c(MSE = mse, RMSE = rmse))
}

# Predictions
valid_predictions <- knnModel$pred

# Calculate metrics
metrics_knn <- calc_knn_metrics(valid_predictions, validData$Crime)

# Cross-Validation of KNN Models
# Cross-validation to find the best K
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)
knn_cv <- train(Crime ~ ., data = trainData_norm, method = "knn", trControl = train_control)

# Compare KNN Models and Select the Best
# Choose the best K based on cross-validation results
best_k <- knn_cv$bestTune$k

# Evaluate the Best Model on Test Data

# Fit best KNN model on the whole training set and test it
best_knnModel <- knn.reg(train = trainData_norm[, -ncol(trainData_norm)], 
                        test = testData_norm[, -ncol(testData_norm)], 
                        y = trainData_norm[, ncol(trainData_norm)], 
                        k = best_k)
#anyNA(trainData_norm)
#anyNA(testData_norm)

test_predictions <- best_knnModel$pred
test_metrics_knn <- calc_knn_metrics(test_predictions, testData$Crime)

# Prepare a Summary Table

summary_table_knn <- data.frame(
    K = c(5, best_k),  # Example: 5 and best K
    Validation_MSE = c(metrics_knn["MSE"], NA),  # Replace NA with metrics for best K
    Validation_RMSE = c(metrics_knn["RMSE"], NA),
    Test_MSE = c("-", test_metrics_knn["MSE"]),
    Test_RMSE = c("-", test_metrics_knn["RMSE"])
)

print(summary_table_knn)

# Determine the Best Model Between KNN and Other Models

# Compare the best KNN model with the best models from linear regression and decision trees based on test set metrics.


