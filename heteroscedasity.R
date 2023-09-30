
# This code tests and adjusts for heteroscedasity

# loading libraries
library(lmtest)
library(sandwich)

# ----------------------
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




# ---------------------
fit_spline_cogsq <- lm(uniamiq ~ bs(cogsq, df=4), data=new_compustat_data)
coeftest(fit_spline_cogsq, vcov = vcovHC(fit_spline_cogsq, type = "HC3"))

fit_spline_revtq <- lm(uniamiq ~ bs(revtq, df=4), data=new_compustat_data)
coeftest(fit_spline_revtq, vcov = vcovHC(fit_spline_revtq, type = "HC3"))

