# Load necessary libraries
library(splines)
library(mgcv)
library(ggplot2)
library(glmnet)  # Add this for Ridge Regression

# Read and clean the data
compustat_data <- read.csv("compustat2.csv")

# Variable selection rationale: (Provide your rationale here)
selected_vars <- c("uniamiq", "cogsq", "revtq")

# Create a new data frame with only the selected columns
new_compustat_data <- compustat_data[, selected_vars]
new_compustat_data <- na.omit(new_compustat_data)

# Split data into training and testing sets
set.seed(123)
train_index <- sample(seq_len(nrow(new_compustat_data)), size = floor(0.8 * nrow(new_compustat_data)))
train_data <- new_compustat_data[train_index, ]
test_data <- new_compustat_data[-train_index, ]

# Prepare the data for Ridge Regression
X_train <- as.matrix(train_data[, c("cogsq", "revtq")])
y_train <- train_data$uniamiq
X_test <- as.matrix(test_data[, c("cogsq", "revtq")])
y_test <- test_data$uniamiq

# Fit the Ridge Regression model
fit_ridge <- glmnet(X_train, y_train, alpha = 0)

# Cross-validation to find the best lambda
cv_fit_ridge <- cv.glmnet(X_train, y_train, alpha = 0)
best_lambda <- cv_fit_ridge$lambda.min

# Refit the model using the best lambda
fit_ridge_best <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda)

# Make predictions on the test set
predicted_profit_test_ridge <- predict(fit_ridge_best, s = best_lambda, newx = X_test)

# Calculate RMSE for the test set
rmse_test_ridge <- sqrt(mean((predicted_profit_test_ridge - y_test)^2))
print(paste("Test RMSE with Ridge Regression:", rmse_test_ridge))

# Make predictions on the training set
predicted_profit_train_ridge <- predict(fit_ridge_best, s = best_lambda, newx = X_train)

# Calculate RMSE for the training set
rmse_train_ridge <- sqrt(mean((predicted_profit_train_ridge - y_train)^2))
print(paste("Training RMSE with Ridge Regression:", rmse_train_ridge))
