
white <- read.csv("https://tinyurl.com/winedata1",
                  sep = ";")
red <- read.csv("https://tinyurl.com/winedata2",
                sep = ";")
white$type <- "white"
  red$type <- "red"
    # Merge the datasets:
  wine <- rbind(white, red)
  wine$type <- factor(wine$type)
  summary(wine)
  
library(caret)
tc <- trainControl(method = "repeatedcv",
                   number = 10, repeats = 100,
                   savePredictions = TRUE,
                   classProbs = TRUE)
# Model 1 - two variables:
m <- train(type ~ pH + alcohol,
           data = wine,
           trControl = tc,
           method = "glm",
           family = "binomial")
# Model 2 - four variables:
m2 <- train(type ~ pH + alcohol + fixed.acidity + residual.sugar,
            data = wine,
            trControl = tc,
            method = "glm",
            family = "binomial")

#plotting ROC and calibration curves for comparison
library(MLeval)
plots <- evalm(list(m, m2),
               gnames = c("Model 1", "Model 2"))
# ROC:
plots$roc
# Calibration curves:
plots$cc
