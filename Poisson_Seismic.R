#Packages: AER
?quakes
View(quakes)
m <- glm(stations ~ mag, data = quakes, family = poisson)
summary(m)
# Plot observed against fitted:
library(ggplot2)
res <- data.frame(Observed = quakes$stations,
                  Fitted = predict(m, type = "response"))
ggplot(res, aes(Fitted, Observed)) +
  geom_point(colour = "blue") +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Fitted values") + ylab("Observed values")
# Binned residual plot:
library(arm)
binnedplot(predict(m, type = "response"),
           residuals(m, type = "response"))
# Test overdispersion
library(AER)
dispersiontest(m, trafo = 1)
# Fit NB regression:
library(MASS)
m2 <- glm.nb(stations ~ mag, data = quakes)
summary(m2)
# Compare fit of observed against fitted:
n <- nrow(quakes)
models <- data.frame(Observed = rep(quakes$stations, 2),
                     Fitted = c(predict(m, type = "response"),
                                predict(m2, type = "response")),
                     Model = rep(c("Poisson", "NegBin"),
                                 c(n, n)))
ggplot(models, aes(Fitted, Observed)) +
  geom_point(colour = "blue") +
  facet_wrap(~ Model, nrow = 3) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Fitted values") + ylab("Observed values")
