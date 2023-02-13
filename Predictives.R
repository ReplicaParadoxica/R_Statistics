# Clarifying that high R^2 and low p-values say very little about predictive performance of a model 
exdata <- data.frame(x1 = c(0.87, -1.03, 0.02, -0.25, -1.09, 0.74,
                            0.09, -1.64, -0.32, -0.33, 1.40, 0.29, -0.71, 1.36, 0.64,
                            -0.78, -0.58, 0.67, -0.90, -1.52, -0.11, -0.65, 0.04,
                            -0.72, 1.71, -1.58, -1.76, 2.10, 0.81, -0.30),
                     x2 = c(1.38, 0.14, 1.46, 0.27, -1.02, -1.94, 0.12, -0.64,
                            0.64, -0.39, 0.28, 0.50, -1.29, 0.52, 0.28, 0.23, 0.05,
                            3.10, 0.84, -0.66, -1.35, -0.06, -0.66, 0.40, -0.23,
                            -0.97, -0.78, 0.38, 0.49, 0.21),
                     x3 = c(1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0,
                            1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1),
                     y = c(3.47, -0.80, 4.57, 0.16, -1.77, -6.84, 1.28, -0.52,
                           1.00, -2.50, -1.99, 1.13, -4.26, 1.16, -0.69, 0.89, -1.01,
                           7.56, 2.33, 0.36, -1.11, -0.53, -1.44, -0.43, 0.69, -2.30,
                           -3.55, 0.99, -0.50, -1.67))
#Computing expected values by formula y = 2x1 = x2 + x3 * x2
exdata$Ey = 2*exdata$x2 - exdata$x3 + exdata$x3*exdata$x2

#Plot expected vs actual
library(ggplot2)
ggplot(exdata, aes(Ey, y)) + geom_point()
# Fitting linear model to the first 20 observations
m <- lm(y ~ x1 + x2 + x3, data = exdata[1:20,])
summary(m)
ggplot(exdata[1:20,], aes(y, predict(m))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red")

exdata$predictions <- predict(m, exdata)
ggplot(exdata[21:30,], aes(y, predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red")
cor(exdata[21:30,]$y, exdata[21:30,]$predictions)

# Removing x3 due to the insignificance in the model
m <- lm(y ~ x1 + x2, data = exdata[1:20,])
summary(m)

exdata$predictions <- predict(m, exdata)
ggplot(exdata[21:30,], aes(y, predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red")
cor(exdata[21:30,]$y, exdata[21:30,]$predictions)

# Fitting correctly specified model and evaluating the results
m <- lm(y ~ x1 + x2 + x3*x2, data = exdata[1:20,])
summary(m)
exdata$predictions <- predict(m, exdata)
ggplot(exdata[21:30,], aes(y, predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red")
cor(exdata[21:30,]$y, exdata[21:30,]$predictions)