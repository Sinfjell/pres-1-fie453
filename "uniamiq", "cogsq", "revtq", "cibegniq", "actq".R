# Read and clean the data
compustat_data <- read.csv("compustat2.csv")

# Load necessary libraries
library(splines)
library(mgcv)
library(ggplot2)
library(glmnet)  # For Ridge Regression

# ----------------------------------------------------------------
# SECTION 1: DATA TREATMENT



# Variable selection rationale: (Provide your rationale here)
selected_vars <- c("uniamiq", "cogsq", "revtq", "cibegniq", "actq")

# Create a new data frame with only the selected columns
new_compustat_data <- compustat_data[, selected_vars]
new_compustat_data <- na.omit(new_compustat_data)

# --------------------------------------------------------
# SECTION 2: TRAINING AND TESTING SETS
# Split data into training and testing sets
set.seed(123)
train_index <- sample(seq_len(nrow(new_compustat_data)), size = floor(0.8 * nrow(new_compustat_data)))
train_data <- new_compustat_data[train_index, ]
test_data <- new_compustat_data[-train_index, ]

# --------------------------------------------------------
# SECTION 3: SPLINES

# Fit individual splines for each variable using training data
fit_spline_cogsq <- lm(uniamiq ~ bs(cogsq, df=4), data=new_compustat_data)
fit_spline_revtq <- lm(uniamiq ~ bs(revtq, df=4), data=new_compustat_data)

# Create data for plotting
plot_data_cogsq <- data.frame(cogsq = seq(min(new_compustat_data$cogsq), max(new_compustat_data$cogsq), length.out=100))
plot_data_revtq <- data.frame(revtq = seq(min(new_compustat_data$revtq), max(new_compustat_data$revtq), length.out=100))

# Predict using the fitted models
plot_data_cogsq$predicted_uniamiq <- predict(fit_spline_cogsq, newdata=plot_data_cogsq)
plot_data_revtq$predicted_uniamiq <- predict(fit_spline_revtq, newdata=plot_data_revtq)

# Plotting the spline for cogsq
ggplot(new_compustat_data, aes(x=cogsq, y=uniamiq)) +
  geom_point() +
  geom_line(data=plot_data_cogsq, aes(x=cogsq, y=predicted_uniamiq), color='red') +
  ggtitle("Spline Fit for cogsq") +
  xlab("cogsq") +
  ylab("uniamiq")

# Plotting the spline for revtq
ggplot(new_compustat_data, aes(x=revtq, y=uniamiq)) +
  geom_point() +
  geom_line(data=plot_data_revtq, aes(x=revtq, y=predicted_uniamiq), color='blue') +
  ggtitle("Spline Fit for revtq") +
  xlab("revtq") +
  ylab("uniamiq")


# --------------------------------------------------------
# SECTION 4: ADDITIVE MODEL
# Fit the additive model using training data
fit_additive_train <- gam(uniamiq ~ s(cogsq) + s(revtq) + s(cibegniq) + s(actq), data=train_data)

summary(fit_additive_train)

# Use the additive model to predict profitability on test data
predicted_profit_test <- predict(fit_additive_train, newdata=test_data)

# Use the additive model to predict profitability on training data
predicted_profit_train <- predict(fit_additive_train, newdata=train_data)

# --------------------------------------------------------
# SECTION 6: RIDGE REGRSSION

# Prepare the data for Ridge Regression
X_train <- as.matrix(train_data[, c("cogsq", "revtq", "cibegniq", "actq")])
y_train <- train_data$uniamiq
X_test <- as.matrix(test_data[, c("cogsq", "revtq", "cibegniq", "actq")])
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
# Make predictions on the training set
predicted_profit_train_ridge <- predict(fit_ridge_best, s = best_lambda, newx = X_train)

# ----------------------------------------------------------------
# SECTION 7
# Comparing RSME results

# Evaluate model performance on test data (e.g., RMSE)
actual_profit_test <- test_data$uniamiq
rmse_test <- sqrt(mean((predicted_profit_test - actual_profit_test)^2))
print(paste("Root Mean Square Error (RMSE) on Test Data:", rmse_test))

# Evaluate model performance on training data (e.g., RMSE)
actual_profit_train <- train_data$uniamiq
rmse_train <- sqrt(mean((predicted_profit_train - actual_profit_train)^2))
print(paste("Root Mean Square Error (RMSE) on Training Data:", rmse_train))

# Calculate RMSE for the training set for the Additive Model
rmse_train_additive <- sqrt(mean((predicted_profit_train - actual_profit_train)^2))
print(paste("Training RMSE with Additive Model:", rmse_train_additive))


# RMSE for Additive Model on Test Data
rmse_test_additive <- sqrt(mean((predicted_profit_test - actual_profit_test)^2))
print(paste("Test RMSE with Additive Model:", rmse_test_additive))

# Calculate RMSE for the test set
rmse_test_ridge <- sqrt(mean((predicted_profit_test_ridge - y_test)^2))
print(paste("Test RMSE with Ridge Regression:", rmse_test_ridge))

# Calculate RMSE for the training set
rmse_train_ridge <- sqrt(mean((predicted_profit_train_ridge - y_train)^2))
print(paste("Training RMSE with Ridge Regression:", rmse_train_ridge))

# Create a data frame to store RMSE results
rmse_results <- data.frame(
  Model_Type = c("Additive Model", "Ridge Regression"),
  RMSE_Test_Data = c(rmse_test_additive, rmse_test_ridge),
  RMSE_Training_Data = c(rmse_train_additive, rmse_train_ridge)
)

# Print the table
print(rmse_results)


#----------------------------------------------------------------
# SECTION X
# Evaluate for simple firm using Ridge Regression

# Create a new matrix for prediction with sample values

new_sample_matrix <- as.matrix(data.frame(cogsq = c(400), revtq = c(1200), 
                                          cibegniq = c(30), actq = c(29)))



# Use the Ridge Regression model to predict profitability for new data
predicted_profit_sample_ridge <- predict(fit_ridge_best, s = best_lambda, newx = new_sample_matrix)

# Display the predicted profitability using Ridge Regression
print("Predicted Profitability for New Sample Data using Ridge Regression:")
print(predicted_profit_sample_ridge)


