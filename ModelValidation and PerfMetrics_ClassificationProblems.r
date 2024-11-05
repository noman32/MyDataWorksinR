#####################
# Logistic Models #
#####################
# Data Preparation:

# Load necessary libraries
library(caret)
library(pROC)

# Load the data
data <- read.csv('default.csv')
dim(data)

# Convert 'default' and 'student' to factor
data$default <- as.factor(data$default)
data$student <- as.factor(data$student)

# Split the data into training (60%), validation (20%), and test (20%) sets
set.seed(123)
indexes <- createDataPartition(data$default, p = 0.6, list = FALSE)
trainData <- data[indexes, ]
tempData <- data[-indexes, ]
indexes <- createDataPartition(tempData$default, p = 0.5, list = FALSE)
validData <- tempData[indexes, ]
testData <- tempData[-indexes, ]

# Model Fitting:

# Fit logistic regression model with all predictors
modelAll <- glm(default ~ ., data = trainData, family = "binomial")

# Fit logistic regression model with only 'balance'
modelBalance <- glm(default ~ balance, data = trainData, family = "binomial")

# Model Evaluation on Validation Set:

# Function to calculate evaluation metrics
evalModel <- function(model, data) {
    preds <- predict(model, newdata = data, type = "response")
    predClass <- ifelse(preds > 0.5, "Yes", "No")
    cm <- confusionMatrix(as.factor(predClass), as.factor(data$default))
    return(list(confusionMatrix = cm$table, accuracy = cm$overall['Accuracy'], 
                precision = cm$byClass['Precision'], recall = cm$byClass['Recall'],
                F1 = cm$byClass['F1']))
}

# Evaluate both models on validation set
evalAll <- evalModel(modelAll, validData)
evalBalance <- evalModel(modelBalance, validData)

# Cross-Validation:
# Cross-validation on training set
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
cv_modelAll <- train(default ~ ., data = trainData, method = "glm", trControl = train_control, family = "binomial", metric = "ROC")
cv_modelBalance <- train(default ~ balance, data = trainData, method = "glm", trControl = train_control, family = "binomial", metric = "ROC")

# Final Model Selection and Evaluation on Test Data:

# Select the best model based on CV results
bestModel <- if(cv_modelAll$results$ROC >= cv_modelBalance$results$ROC) modelAll else modelBalance

# Evaluate the best model on test set
finalEval <- evalModel(bestModel, testData)

# ROC Curve and AUC on Test Data:
# ROC and AUC
preds <- predict(bestModel, newdata = testData, type = "response")
roc_curve <- roc(testData$default, preds)
auc_val <- auc(roc_curve)
plot(roc_curve, main = "ROC Curve")
#abline(a = 0, b = 1, col = "red")

# finding the optimal threshold
coords <- coords(roc_curve, "best", ret = "co", best.method = "closest.topleft")
coords <- coords(roc_curve, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "closest.topleft")
coords

# Alternatively
# Extract Thresholds:
# The ROC object contains thresholds for each point on the curve. You can access them along with TPR and FPR:

thresholds <- roc_curve$thresholds
tpr <- roc_curve$sensitivities  # True Positive Rate
fpr <- roc_curve$specificities  # False Positive Rate (1 - Specificity)

# Find a Specific Threshold:

# To find the threshold for a specific point, you need to define your criteria. For example, if you're looking for the threshold that balances sensitivity and specificity, you might look for the point where TPR and FPR are closest to each other or where the Youden’s index is maximized.
# Here's an example to find the threshold closest to a specific TPR value (e.g., TPR = 0.8):

specific_tpr <- 0.8
idx <- which.min(abs(tpr - specific_tpr))
specific_threshold <- thresholds[idx]




################################
# SVM - Support Vector Machine #
################################

# Data Preparation

library(e1071)  # SVM package
library(caret)  # For data splitting and evaluation

# Load the dataset
data <- read.csv('C:\\Users\\noman\\My Drive\\ABP\\ABP DSBA Batch 03\\Unit 03\\Session 03_Model validation\\default.csv')

nrow(data)

# Convert categorical columns to factors
data$default <- as.factor(data$default)
data$student <- as.factor(data$student)

# Split the data into training, validation, and test sets
set.seed(123)
indexes <- createDataPartition(data$default, p = 0.6, list = FALSE)
trainData <- data[indexes, ]
tempData <- data[-indexes, ]
indexes <- createDataPartition(tempData$default, p = 0.5, list = FALSE)
validData <- tempData[indexes, ]
testData <- tempData[-indexes, ]

# Model Training

# SVM model with all IVs
# Retrain SVM models with probability estimates
#svmModelAll <- svm(default ~ ., data = trainData, type = 'C-classification', kernel = 'linear')
svmModelAll <- svm(default ~ ., data = trainData, type = 'C-classification', kernel = 'linear', probability = TRUE, cost = 10) # with probability estimates to later calculate AUC

# SVM model with only 'balance'
#svmModelBalance <- svm(default ~ balance, data = trainData, type = 'C-classification', kernel = 'linear', probability = TRUE) # Retrain SVM models with probability estimates
svmModelBalance <- svm(default ~ balance, data = trainData, type = 'C-classification', kernel = 'linear', probability = TRUE, cost = 10) # Retrain SVM models with probability estimates to later calculate AUC

# what is the cost parameter?
# The C parameter is a regularization parameter in SVM that controls the trade-off between achieving a low-error model and a model that fits the training data well. A higher value of C tells the SVM to try to classify all training examples correctly (high penalty for misclassified points), while a lower value encourages simplicity (wider margin) even if that simplicity leads to more misclassification errors on the training data.

# Model Evaluation on Validation Set

# Function to calculate evaluation metrics
# with AUC
evaluateModelWithAUC <- function(model, data) {
  # Predict probabilities
  probabilities <- predict(model, newdata = data, probability = TRUE)
  # Extract probabilities for the positive class (assuming 'Yes' is coded as 1)
  probYes <- attr(probabilities, "probabilities")[,2]
  
  # Generate predictions based on a threshold (e.g., 0.5)
  predictions <- ifelse(probYes > 0.5, "Yes", "No")
  predictions <- factor(predictions, levels = levels(data$default))
  
  # Calculate confusion matrix
  confusionMat <- confusionMatrix(predictions, data$default)
  
  # Calculate AUC
  rocObj <- roc(response = data$default, predictor = probYes)
  aucValue <- auc(rocObj)
  
  # Return a list containing both the confusion matrix and AUC
  return(list(confusionMatrix = confusionMat, AUC = aucValue))
}
# Example usage with svmModelAll and validData
evalAllWithAUC <- evaluateModelWithAUC(svmModelAll, validData)
evalBalanceWithAUC <- evaluateModelWithAUC(svmModelBalance, validData)

# To print the results
print(evalAllWithAUC$confusionMatrix)
print(evalAllWithAUC$AUC)

print(evalBalanceWithAUC$confusionMatrix)
print(evalBalanceWithAUC$AUC)


# Set up cross-validation
# Define train control
trainControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# Define a tuning grid, specifying a range of values for 'C'
# why different Cs?

# Balancing Act: 'C' is kind of a balance or a dial that adjusts how strict the SVM model is about misclassification errors on the training data. A high value of 'C' is like telling the model, "I really don't want you to make any mistakes on the training data," which makes the model try very hard to get every training example correct. However, this can lead to a model that is too complex and may not generalize well to unseen data (overfitting).

# Penalty for Errors: Another way to explain it is in terms of penalties. 'C' represents how big of a penalty the model gets for each misclassified training example. A high 'C' means a high penalty for every mistake, leading the model to create a very narrow street (margin) that tries to separate all points of different classes without errors. A low 'C' means the model is more forgiving of mistakes, focusing more on finding a broader street that may allow some misclassifications if it results in a simpler, more generalizable model.

# Finding the Sweet Spot: Emphasize that choosing the right value of 'C' is about finding the sweet spot between a model that is too strict (and doesn't generalize well) and one that is too lenient (and might not perform well on the training data). It's about balance and trade-offs.

# Using visual aids, such as plots showing the effect of different 'C' values on the decision boundary, can also be very helpful for students to understand this concept intuitively.

tuneGrid <- expand.grid(C = c(0.01, 0.1, 1, 10, 100))

# Train the SVM model with the tuning grid and cross-validation with all predictors
set.seed(123)
CVsvmModelAll <- train(default ~ ., data = data, method = "svmLinear", trControl = trainControl, preProcess = c("center", "scale"), metric = "ROC", tuneGrid = tuneGrid) # Also Preprocess the data by centering and scaling it; tuneGrid specifies the range of 'C' values to try


# Print the model summary
print(CVsvmModelAll)

# Plot model performance
plot(CVsvmModelAll)

# Train the SVM model with the tuning grid for the 'balance' predictor
set.seed(123)
CVsvmModelBalance <- train(default ~ balance, data = data, method = "svmLinear", trControl = trainControl, preProcess = c("center", "scale"), metric = "ROC", tuneGrid = tuneGrid)


# Print the model summary
print(svmModel)

# Plot model performance
plot(svmModel)

# Remember, the choice of C is crucial for the performance of your SVM model. A too small value might lead to underfitting (too general, not capturing the complexities of the data), while a too large value might lead to overfitting (too specific, capturing noise in the data as if it were a real pattern). It's often recommended to perform some form of hyperparameter tuning (such as grid search or random search) to find the best C value for your dataset.

# Choose the best model based on CV accuracy
CVsvmModelAll$results # in the results SD is the standard deviation of the ROC etc
CVsvmModelBalance$results$ROC

bestModel <- if(max(CVsvmModelAll$results$ROC) >= max(CVsvmModelBalance$results$ROC)) CVsvmModelAll else CVsvmModelBalance
bestModel
# Evaluate the best model on test data
finalEval <- evaluateModel(bestModel, testData)

# ROC Curve and AUC on Test Data

probPredictions <- predict(bestModel, testData, type = "prob")
positiveProbabilities <- probPredictions[, "Yes"]
roc_curve <- roc(response = testData$default, predictor = positiveProbabilities)


# Plot ROC
plot(roc_curve)
auc(roc_curve)

##################################
##################################

# some notes on max iterations and tolerance:
In the svm function from the e1071 package in R, the maximum number of iterations for the optimization algorithm can be specified using the max.iter argument. However, it's important to note that the max.iter argument is not directly exposed in the svm function's interface for all SVM types. For the SVM models trained with the e1071 package, especially with the type of C-classification and a linear kernel as in your examples, the optimization routine's maximum number of iterations is typically managed internally by the LIBSVM library, which e1071 interfaces with.

The default value for the maximum number of iterations in LIBSVM (and thus in e1071 when it calls LIBSVM) is not explicitly set, meaning the algorithm will continue until convergence within the tolerance specified by the -e option (the tolerance of the termination criterion), rather than stopping after a fixed number of iterations. This approach is designed to ensure that the optimization process is more focused on the convergence criteria rather than arbitrarily stopping after a certain number of steps.

If you need to control the maximum number of iterations, especially for large datasets or to prevent extremely long training times, you might need to look into other mechanisms or consider whether another R package for SVMs provides more direct control over this parameter. For very specific optimization control, packages that allow more detailed setting of the optimization algorithm parameters might be required.

In practical terms, if you're experiencing issues with training time or convergence with the svm function in e1071, consider adjusting other parameters that influence the optimization process, such as the convergence tolerance (tolerance parameter in some functions), or exploring feature scaling and selection to reduce the complexity of the model. For more advanced use cases, you might explore using different machine learning packages in R that offer more granular control over the training process and its optimization parameters.

# More on tolerance

In the context of the svm function from the e1071 package in R, which interfaces with the LIBSVM library, the tolerance of the termination criterion (which affects when the training process stops improving and is considered to have converged) is controlled by the epsilon parameter for regression models. However, for classification models, this control is not directly exposed through a simple parameter in the e1071 package's svm function interface.

For classification tasks using svm from e1071, the primary way to influence the stopping criteria indirectly is through the cost parameter (which influences the trade-off between margin size and misclassification rate) and by ensuring your data is appropriately scaled and prepared, which can affect the optimization process's efficiency and convergence.

If you're looking specifically to adjust the optimization algorithm's tolerance in an SVM classification context and can't find a direct way to do so with e1071, you might consider other packages or software that provides more granular control over the training process. For instance, the kernlab package offers SVM functionality with some additional control over training parameters, although the specific setting of a convergence tolerance might still be more implicit than explicit.

'''
################################
# KNN - K-Nearest Neighbors #
################################

library(class)
library(caret)
# Only doing CV and test set evaluation


# Cross-validation for both models
trainControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

cvknnModelAll <- train(default ~ ., data = trainData, method = "knn", trControl = trainControl, metric = "ROC")
cvknnModelBalance <- train(default ~ balance, data = trainData, method = "knn", trControl = trainControl, metric = "ROC")

# Final Model Selection and Evaluation on Test Data

# Choose the best model based on CV accuracy
cvknnModelAll$results # in the results SD is the standard deviation of the ROC etc
cvknnModelBalance$results$ROC

bestModel <- if(max(cvknnModelAll$results$ROC) >= max(cvknnModelBalance$results$ROC)) CVsvmModelAll else cvknnModelBalance
bestModel
# Evaluate the best model on test data
finalEval <- evaluateModel(bestModel, testData)

# ROC Curve and AUC on Test Data

probPredictions <- predict(bestModel, testData, type = "prob")
positiveProbabilities <- probPredictions[, "Yes"]
roc_curve <- roc(response = testData$default, predictor = positiveProbabilities)


# Plot ROC
plot(roc_curve)
auc(roc_curve)




# Function to perform KNN and evaluate
performKNN <- function(trainData, testData, predictors, k) {
  # Perform KNN
  knnPred <- knn(train = trainData[predictors], test = testData[predictors], cl = trainData$default, k = k)
  
  # Evaluate
  cm <- confusionMatrix(knnPred, as.factor(testData$default))
  return(list(confusionMatrix = cm$table, accuracy = cm$overall['Accuracy'], 
              precision = cm$byClass['Precision'], recall = cm$byClass['Recall'],
              F1 = cm$byClass['F1']))
}


# Set up cross-validation control
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# For all predictors
set.seed(123)
cv_modelAll <- train(default ~ ., data = trainData, method = "knn", trControl = train_control, preProcess = c("center", "scale"), tuneLength = 10, metric = "Accuracy")

# For 'balance' only
set.seed(123)
cv_modelBalance <- train(default ~ balance, data = trainData, method = "knn", trControl = train_control, preProcess = c("center", "scale"), tuneLength = 10, metric = "Accuracy")

# Using the best k from cross-validation
bestK_all <- cv_modelAll$bestTune$k
bestK_balance <- cv_modelBalance$bestTune$k

# Evaluate on validation set
evalAll_valid <- performKNN(trainData, validData, names(trainData)[-1], bestK_all) # excluding 'default' from predictors
evalBalance_valid <- performKNN(trainData, validData, "balance", bestK_balance)

# Evaluate on test set
evalAll_test <- performKNN(trainData, testData, names(trainData)[-1], bestK_all)
evalBalance_test <- performKNN(trainData, testData, "balance", bestK_balance)

# Print results for validation set
cat("Validation Set Results:\n")
cat("All Predictors - Accuracy:", evalAll_valid$accuracy, "\n")
cat("Balance Only - Accuracy:", evalBalance_valid$accuracy, "\n\n")

# Print results for test set
cat("Test Set Results:\n")
cat("All Predictors - Accuracy:", evalAll_test$accuracy, "\n")
cat("Balance Only - Accuracy:", evalBalance_test$accuracy, "\n")


### 
# IF you prefer ROC as the metric, you can use the following code to train the KNN model and evaluate it using ROC:

# If you prefer to use AUC (Area Under the Receiver Operating Characteristic Curve) as your metric for model selection and evaluation, you can modify the `trainControl` and `train` function calls to use ROC analysis. Then, you can plot the ROC curve for the model evaluations. Here's how to adjust the process:

### 1. Adjust `trainControl` for AUC

# When setting up your `trainControl`, ensure you specify `summaryFunction = twoClassSummary` to use AUC. This requires `classProbs = TRUE` because AUC is calculated from class probabilities rather than class predictions.


train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)


### 2. Train Models Using AUC as Metric

Specify `metric = "ROC"` in your `train` function calls to optimize the models based on AUC.

```r
# For all predictors
cv_modelAll <- train(default ~ ., data = trainData, method = "knn", trControl = train_control, preProcess = c("center", "scale"), tuneLength = 10, metric = "ROC")

# For 'balance' only
cv_modelBalance <- train(default ~ balance, data = trainData, method = "knn", trControl = train_control, preProcess = c("center", "scale"), tuneLength = 10, metric = "ROC")
```

### 3. Plotting AUC

To plot the ROC curve and calculate AUC for your best model on the test data, you can use the `pROC` package, which provides tools for ROC analysis.

First, ensure you have `pROC` installed and loaded:

```r
install.packages("pROC")
library(pROC)
```

Then, use the model to predict class probabilities on the test data, generate the ROC curve, and calculate AUC:

```r
# Predict class probabilities
probabilitiesAll <- predict(cv_modelAll, testData, type = "prob")[,2] # Assuming 'Yes' is the second level for default
rocAll <- roc(response = testData$default, predictor = probabilitiesAll)

# Plot ROC curve
plot(rocAll, main = "ROC Curve for All Predictors")
aucAll <- auc(rocAll)
cat("AUC for All Predictors:", aucAll, "\n")

# Repeat for model with 'balance' only if needed
probabilitiesBalance <- predict(cv_modelBalance, testData, type = "prob")[,2]
rocBalance <- roc(response = testData$default, predictor = probabilitiesBalance)
# Plot ROC curve for balance model
plot(rocBalance, main = "ROC Curve for Balance Predictor")
aucBalance <- auc(rocBalance)
cat("AUC for Balance Predictor:", aucBalance, "\n")
```

### 4. Explanation

This adjusted process emphasizes optimizing and evaluating the KNN models based on their ability to discriminate between the two classes (`default` = 'Yes' vs. 'No') using AUC as the performance metric. AUC provides a single measure of a model's effectiveness across all possible classification thresholds, with higher values indicating better model performance. The ROC curve plots the true positive rate against the false positive rate at various threshold settings, visualizing the trade-off between sensitivity and specificity.

Using AUC as the metric for model selection and evaluation is particularly useful for imbalanced datasets or when the cost of false positives differs significantly from the cost of false negatives.