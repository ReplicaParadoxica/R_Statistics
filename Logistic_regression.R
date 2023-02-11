# Computing parametric bootstrap confidence intervals and p-values for logistic regression model
#Package: arm

sharks <- read.csv("...\\data_csv\\sharks.csv", sep =";")
View(sharks)
sharks$Age <- as.numeric(sharks$Age)
sharks$Sex. <- factor(sharks$Sex, levels = c("F", "M"))
sharks$Fatal..Y.N. <- factor(sharks$Fatal..Y.N., levels = c("N", "Y"))
m <- glm(Fatal..Y.N. ~ Age + Sex., data = sharks, family = binomial)
summary(m)
library(MASS)
confint(m)
coefficients <- function(formula, data, predictions, ...) {
  # Remove rows where the response variable is missing:
  data <- na.omit(data, all.vars(formula)[1])
  # Check whether the response variable is a factor or
  # numeric, and then resample:
  if(is.factor(data[,all.vars(formula)[1]])) {
    # If the response variable is a factor:
    data[,all.vars(formula)[1]] <-
      factor(levels(data[,all.vars(formula)[1]])[1 + rbinom(nrow(data),
                                                            1, predictions)]) } else {
                                                              # If the response variable is numeric:
                                                              data[,all.vars(formula)[1]] <-
                                                                unique(data[,all.vars(formula)[1]])[1 + rbinom(nrow(data),
                                                                                                               1, predictions)] }
  m <- glm(formula, data = data, family = binomial)
  return(coef(m))
}
boot_res <- boot(data = sharks, statistic = coefficients,
                 R=999, formula = formula(m),
                 predictions = predict(m, type = "response"))
# Compute confidence intervals:
boot.ci(boot_res, type = "perc", index = 1) # Intercept
boot.ci(boot_res, type = "perc", index = 2) # Age
boot.ci(boot_res, type = "perc", index = 3) # Sex.M
# Compute p-values:
# The null hypothesis is that the effect (beta coefficient)
# is 0:
beta_null <- 0
# Set initial conditions:
in_interval <- TRUE
alpha <- 0
# Find the lowest alpha for which beta_null is in the
# interval:
while(in_interval)
{
  # Based on the asymptotic test, we expect the p-value
  # to not be close to 0. We therefore increase alpha by
  # 0.01 instead of 0.001 in each iteration.
  alpha <- alpha + 0.01
  interval <- boot.ci(boot_res,
                      conf = 1 - alpha,
                      type = "perc", index = 2)$perc[4:5]
  in_interval <- beta_null > interval[1] & beta_null < interval[2]
}
# Print the p-value:
alpha

#
# Investigating the residuals
m <- glm(Fatal..Y.N. ~ Age + Sex., data = sharks, family = binomial)
library(arm)
binnedplot(predict(m, type = "response"),
           residuals(m, type = "response"))
#Check Cook's distance
res <- data.frame(Index = 1:length(cooks.distance(m)),
                  CooksDistance = cooks.distance(m))
# Plot index against the Cook's distance to find
# influential points:
ggplot(res, aes(Index, CooksDistance)) +
  geom_point() +
  geom_text(aes(label = ifelse(CooksDistance > 0.05,
                               rownames(res), "")),
            hjust = 1.1)
#Looking into points with high leverage
sharks[116,]

