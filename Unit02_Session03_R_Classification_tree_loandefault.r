# Install and load the tree package
# install.packages("tree")  # Uncomment this line if you haven't installed the package
library(tree)

# Read the dataset
dfDefault <- read.csv("default.csv")
nrow(dfDefault)
dim(dfDefault)
# Convert the dependent variable to a factor if it's not already
dfDefault$default <- as.factor(dfDefault$default)

# Fit the classification tree model
# Replace 'balance + other_predictors' with your actual predictors
classTreeMod <- tree(default ~ balance, data = dfDefault)
print(classTreeMod)

sum(dfDefault$default == "No")
sum(dfDefault$default == "Yes") 

######################
# 1) root 10000 2921.0 No ( 0.966700 0.033300 )
#  2) balance < 1472.99 9004  760.7 No ( 0.992892 0.007108 )
#    4) balance < 1099.01 7085  151.2 No ( 0.998589 0.001411 ) *
#    5) balance > 1099.01 1919  492.1 No ( 0.971860 0.028140 ) *
#  3) balance > 1472.99 996 1162.0 No ( 0.729920 0.270080 )
#    6) balance < 1856.76 786  721.0 No ( 0.828244 0.171756 ) *
#    7) balance > 1856.76 210  274.9 Yes ( 0.361905 0.638095 ) *

# 2921.0: This is the total deviance at the root node of the tree. Deviance in a classification tree is a measure of how well the tree explains the variability in the data. It's somewhat analogous to variance in regression. A higher deviance value indicates that the model has more room for improvement in terms of explaining the response variable.

# No: This indicates the most common class (or category) of the target variable at the node. In your case, the target variable is binary (likely with "Yes" and "No" outcomes), and "No" is the predominant class at the root node. It means that, before any splits are made, the majority of your data falls into the "No" category.

# 0.966700 (First Number in yprob): This is the probability of the "No" class at the root node. It means that approximately 96.67% of the observations in your dataset are classified as "No" before making any splits based on the predictor variables.

# 0.033300 (Second Number in yprob): Conversely, this is the probability of the "Yes" class at the root node. It indicates that approximately 3.33% of the observations in your dataset are classified as "Yes" initially.

# These values together give you a starting point for the classification tree. The tree then makes binary splits based on the predictor variables (e.g., balance in your model) to try and better classify each observation into "Yes" or "No", thereby reducing the overall deviance and improving the fit of the model to your data.

# To understand how deviance is calculated in a classification tree, let's consider a binary classification problem. In such problems, deviance is typically calculated using a measure based on log-likelihood. The formula used is:

# Deviance=−2×(log-likelihood of the model−log-likelihood of the saturated model)

# Here's a simplified example to illustrate this:

# Assume you have a dataset where you're trying to predict a binary outcome (Yes/No). Let's say:

#    There are 100 observations in total.
#    80 observations are classified as "No" and 20 as "Yes".

# Now, let's calculate the deviance at the root node (before any splits):

#    Calculate the probabilities:
#        P(Yes) = 20/100 = 0.20
#        P(No) = 80/100 = 0.80

#    Calculate the log-likelihood for the model:
#        For simplicity, we'll use the formula: log-likelihood = Σ(yi * log(pi) + (1 - yi) * log(1 - pi)), where yi is the actual outcome (1 for Yes, 0 for No), and pi is the predicted probability of being "Yes".
#        For our data: log-likelihood = (20 * log(0.20) + 80 * log(0.80)).

#    Calculate the log-likelihood for the saturated model:
#        A saturated model is a theoretical model with as many parameters as data points, perfectly predicting each outcome.
#        In such a model, the log-likelihood is usually higher as it predicts each observation perfectly.
#        For our binary classification with a balanced outcome (assuming perfect prediction), this might be the sum of the logs of the individual probabilities of each outcome, which are 1 for a correct prediction.

#    Calculate deviance:
#        Substitute the calculated log-likelihoods into the deviance formula.

#######################

# Plot the tree
plot(classTreeMod)
text(classTreeMod, pretty=0)

# Summary of the tree
summary(classTreeMod)

#######################
# Classification tree:
# tree(formula = default ~ balance, data = dfDefault)
# Number of terminal nodes:  4
# Residual mean deviance:  0.164 = 1639 / 9996
# Misclassification error rate: 0.0275 = 275 / 10000

######## 0.164 = 1639 / 9996

# the deviance is a measure used to quantify how well the tree fits the data. Specifically, it's often related to the log-likelihood of the model. The residual mean deviance provided in the output is a summary statistic that gives an overall indication of the tree's fit.

#In this example:

#    Residual Mean Deviance: 0.164 = 1639 / 9996

# This means the total deviance of the model is 1639, and it is divided by 9996, which represents the degrees of freedom. The degrees of freedom in this context are usually the number of observations minus the number of parameters estimated by the model (in this case, the number of terminal nodes minus one). The 'residual mean deviance' is thus an average measure of deviance per degree of freedom.

# Deviance in a classification context is calculated using a log-likelihood comparison between your model and a saturated model (a model that perfectly predicts each outcome). The formula involves calculating the negative twice the difference between the log-likelihood of your model and the log-likelihood of the saturated model. Lower deviance indicates a model that fits the data better.

# The specific calculation of deviance can vary based on the type of model and the software implementation, but it generally follows the principles of likelihood-based model comparison.

#######################

# Predict using the tree model
# This gives the predicted class for each observation
pred <- predict(classTreeMod, type = "class")

# Create a confusion matrix to evaluate the model
confusionMatrix <- table(Predicted = pred, Actual = dfDefault$default)
print(confusionMatrix)

# Cross-validation to determine the best tree size
#cvTree <- cv.tree(classTreeMod, FUN=prune.misclass)
cvTree <- cv.tree(classTreeMod)

# Plot deviance (error) against tree size for cross-validation results
plot(cvTree$size, cvTree$dev, type = "b")

# Prune the tree (optional, based on cross-validation results)
# Replace 'best=9' with the optimal size from your cross-validation
prunedTree <- prune.misclass(classTreeMod, best=3)  # 'best' is the tree size after pruning

# Plot the pruned tree
plot(prunedTree)
text(prunedTree, pretty=0)
