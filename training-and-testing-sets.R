# Load necessary libraries
library(splines)
library(mgcv)
library(ggplot2)

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

# Fit individual splines for each variable using training data
fit_spline_cogsq_train <- lm(uniamiq ~ bs(cogsq, df=4), data=train_data)

# Create data for plotting using training data
plot_data_cogsq <- data.frame(cogsq = seq(min(train_data$cogsq), max(train_data$cogsq), length.out=100))

# Predict using the fitted model on training data
plot_data_cogsq$predicted_uniamiq <- predict(fit_spline_cogsq_train, newdata=plot_data_cogsq)

# Plotting the spline for cogsq using training data
ggplot(train_data, aes(x=cogsq, y=uniamiq)) +
  geom_point() +
  geom_line(data=plot_data_cogsq, aes(x=cogsq, y=predicted_uniamiq), color='red') +
  ggtitle("Spline Fit for cogsq (Training Data)") +
  xlab("cogsq") +
  ylab("uniamiq")

# Fit the additive model using training data
fit_additive_train <- gam(uniamiq ~ s(cogsq) + s(revtq), data=train_data)

# Use the additive model to predict profitability on test data
predicted_profit_test <- predict(fit_additive_train, newdata=test_data)

# Add the predicted_profit as a new column in test data
test_data$predicted_profit <- predicted_profit_test

# Evaluate model performance on test data (e.g., RMSE)
actual_profit_test <- test_data$uniamiq
rmse_test <- sqrt(mean((predicted_profit_test - actual_profit_test)^2))
print(paste("Root Mean Square Error (RMSE) on Test Data:", rmse_test))

# Use the additive model to predict profitability on training data
predicted_profit_train <- predict(fit_additive_train, newdata=train_data)

# Evaluate model performance on training data (e.g., RMSE)
actual_profit_train <- train_data$uniamiq
rmse_train <- sqrt(mean((predicted_profit_train - actual_profit_train)^2))
print(paste("Root Mean Square Error (RMSE) on Training Data:", rmse_train))

# Evaluate for simple firm
# Create a new data frame for prediction with sample values
new_sample_data <- data.frame(cogsq = c(1200), revtq = c(3000))

# Use the additive model to predict profitability for new data
predicted_profit_sample <- predict(fit_additive_train, newdata = new_sample_data)

# Display the predicted profitability
print("Predicted Profitability for New Sample Data:")
print(predicted_profit_sample)

