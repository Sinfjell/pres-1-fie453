
# This code tests and adjusts for heteroscedasity

# loading libraries
library(lmtest)
library(sandwich)
library(splines)
library(mgcv)
library(stargazer)

# Read and clean the data
compustat_data <- read.csv("compustat2.csv")

# Create a new data frame with only the selected columns
new_compustat_data <- compustat_data[, selected_vars]
new_compustat_data <- na.omit(new_compustat_data)

# ----------------------

# Fir splines before adjusting for heterosceasity --------------------------------
fit_spline_cogsq <- lm(uniamiq ~ bs(cogsq, df=4), data=new_compustat_data)
fit_spline_revtq <- lm(uniamiq ~ bs(revtq, df=4), data=new_compustat_data)


# Function to compare t-values
compare_t_values <- function(model) {
  # Standard OLS t-values
  standard_t_values <- summary(model)$coefficients[, "t value"]
  # Robust t-values
  robust_t_values <- coeftest(model, vcov = vcovHC(model, type = "HC3"))[, "t value"]
  # Combine and compare
  comparison <- data.frame(Standard = standard_t_values, Robust = robust_t_values)
  print(comparison)
}

compare_t_values(fit_spline_cogsq)

# Fitting splines and adjusting for heteroscedasity
fit_spline_cogsq <- lm(uniamiq ~ bs(cogsq, df=4), data=new_compustat_data)
coeftest(fit_spline_cogsq, vcov = vcovHC(fit_spline_cogsq, type = "HC3"))

fit_spline_revtq <- lm(uniamiq ~ bs(revtq, df=4), data=new_compustat_data)
coeftest(fit_spline_revtq, vcov = vcovHC(fit_spline_revtq, type = "HC3"))

# plotting startgazer adjusted for robustness
stargazer(model1, model2, model3, title = "Regression Model Summaries", type = "text", 
          se = list(se[[1]], se[[2]], se[[3]]), report = "vc*t")

# Calculate robust standard errors for the models
se_fit_spline_cogsq <- sqrt(diag(vcovHC(fit_spline_cogsq, type = "HC3")))
se_fit_spline_revtq <- sqrt(diag(vcovHC(fit_spline_revtq, type = "HC3")))

# Create the stargazer table
stargazer(fit_spline_cogsq, fit_spline_revtq, title = "Regression Model Summaries", type = "text", 
          se = list(se_fit_spline_cogsq, se_fit_spline_revtq), report = "vc*t")

stargazer(fit_spline_cogsq, fit_spline_revtq, title = "Regression Model Summaries", type = "text")

