# packages: ggofortify, lmPerm, boot.pval
#First of all, fitting a linear regression model with temperatures (response variable) and Sun hours (explanatory)
weather <- read.csv("...\\data_csv\\sales-weather.csv", sep=";")
View(weather)
# Fit model:
m <- lm(TEMPERATURE ~ SUN_HOURS, data = weather)
summary(m)

# Plot the result:
library(ggplot2)
ggplot(weather, aes(SUN_HOURS, TEMPERATURE)) +
  geom_point() +
  geom_abline(aes(intercept = coef(m)[1], slope = coef(m)[2]),
              colour = "red")

weather$prec_dummy <- factor(weather$PRECIPITATION > 0)
m <- lm(TEMPERATURE ~ SUN_HOURS*prec_dummy, data = weather)
# Check if variables are significantly non-zero
summary(m)

# Two models:
m1 <- lm(TEMPERATURE ~ SUN_HOURS, data = weather)
m2 <- lm(TEMPERATURE ~ SUN_HOURS*prec_dummy, data = weather)
n <- nrow(weather)
models <- data.frame(Observed = rep(weather$TEMPERATURE, 2),
                     Fitted = c(predict(m1), predict(m2)),
                     Model = rep(c("Model 1", "Model 2"), c(n, n)))
ggplot(models, aes(Fitted, Observed)) +
  geom_point(colour = "blue") +
  facet_wrap(~ Model, nrow = 3) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Fitted values") + ylab("Observed values")
#Generated model2 is better at predicting high temperatures => residual plot: 
library(ggfortify)
autoplot(m2, which = 1:6, ncol = 2, label.size = 3)
#Plot will suggest if there are signs of heteroscedasticity/trends
#Check high leverages on Cook's distance
weather[c(57, 76, 83),]
sort(weather$SUN_HOURS)
min(weather$TEMPERATURE)

#Running boxcox on models to check if transformation would be useful. Make sure variables are non-negative
weather$TEMPERATUREplus10 <- weather$TEMPERATURE + 10
m <- lm(TEMPERATUREplus10 ~ SUN_HOURS*prec_dummy, data = weather)
library(MASS)
boxcox(m)
# lambda 1 value can be found by the dotted line. 


# Refitting model with lmp, to check effect still significant
library(lmPerm)
m <- lmp(TEMPERATURE ~ SUN_HOURS*prec_dummy, data = weather)
summary(m)

# Refitting model using robust regression estimator with rlm
# Boot_summary method:
library(MASS)
m <- rlm(TEMPERATURE ~ SUN_HOURS*prec_dummy, data = weather)
library(boot.pval)
boot_summary(m, type = "perc", method = "residual")
# Boot method:
library(car)
boot_res <- Boot(m, method = "residual")
# Compute 95 % confidence intervals using confint
confint(boot_res, type = "perc")
library(MASS)
m <- rlm(TEMPERATURE ~ SUN_HOURS*prec_dummy, data = weather)

# Confidence intervals
library(boot)
coefficients <- function(formula, data, i, predictions, residuals) {
  # Create the bootstrap value of response variable by
  # adding a randomly drawn residual to the value of the
  # fitted function for each observation:
  data[,all.vars(formula)[1]] <- predictions + residuals[i]
  # Fit a new model with the bootstrap value of the response
  # variable and the original explanatory variables:
  m <- rlm(formula, data = data)
  return(coef(m))
}
# Fit the linear model:
m <- rlm(TEMPERATURE ~ SUN_HOURS*prec_dummy, data = weather)
# Compute scaled and centred residuals:
res <- residuals(m)/sqrt(1 - lm.influence(m)$hat)
res <- res - mean(res)
# Run the bootstrap, extracting the model formula and the
# fitted function from the model m:
boot_res <- boot(data = weather, statistic = coefficients,
                 R = 999, formula = formula(m),
                 predictions = predict(m),
                 residuals = res)
# Compute confidence intervals:
boot.ci(boot_res, type = "perc", index = 1) # Intercept
boot.ci(boot_res, type = "perc", index = 2) # Sun hours
boot.ci(boot_res, type = "perc", index = 3) # Precipitation dummy
boot.ci(boot_res, type = "perc", index = 4) # Interaction term

#
# Computing a bootstrap prediction interval for the temperature on a day with precipitation (no sun hours)
#
m <- lm(TEMPERATURE ~ SUN_HOURS*prec_dummy, data = weather)
new_data <- data.frame(SUN_HOURS = 0, prec_dummy = "TRUE")
boot_pred <- function(data, new_data, model, i,
                      formula, predictions, residuals){
  data[,all.vars(formula)[1]] <- predictions + residuals[i]
  m_boot <- lm(formula, data = data)
  predict(m_boot, newdata = new_data) +
    sample(residuals(m_boot), nrow(new_data))
}
library(boot)
boot_res <- boot(data = m$model,
                 statistic = boot_pred,
                 R = 999,
                 model = m,
                 new_data = new_data,
                 formula = formula(m),
                 predictions = predict(m),
                 residuals = residuals(m))
# 95 % bootstrap prediction interval:
boot.ci(boot_res, type = "perc")


