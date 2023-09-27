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

# Assuming compustat_data and new_compustat_data are already loaded and cleaned


# Fit individual splines for each variable --------------------------------
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

# Fit the additive model
fit_additive <- gam(uniamiq ~ s(cogsq) + s(revtq), data=new_compustat_data)
summary(fit_additive)

# Use the additive model to predict profitability
predicted_profit <- predict(fit_additive, newdata=new_compustat_data)

# Add the predicted_profit as a new column
new_compustat_data$predicted_profit <- predicted_profit