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
                   summaryFunction = twoClassSummary,
                   savePredictions = TRUE,
                   classProbs = TRUE)
m <- train(type ~ pH + alcohol + fixed.acidity + residual.sugar,
           data = wine,
           trControl = tc,
           method = "rpart",
           metric = "ROC",
           tuneGrid = expand.grid(cp = 0))
m
# Decision tree:
library(rpart.plot)
prp(m$finalModel)

# Pruned decision tree:
m <- train(type ~ pH + alcohol + fixed.acidity + residual.sugar,
           data = wine,
           trControl = tc,
           method = "rpart",
           metric = "ROC",
           tuneGrid = expand.grid(cp = 0.01))
prp(m$finalModel)

# Using tuneGrid = expand.grid(cp = seq(0, 0.01, 0.001)) to find an optimal choice of cp
m <- train(type ~ pH + alcohol + fixed.acidity + residual.sugar,
           data = wine,
           trControl = tc,
           method = "rpart",
           metric = "ROC",
           tuneGrid = expand.grid(cp = seq(0, 0.01, 0.001)))
m
prp(m$finalModel)


# Graph illustrations
library(MLeval)
plots <- evalm(m, gnames = "Decision tree")
# ROC:
plots$roc
# 95 % Confidence interval for AUC:
plots$optres[[1]][13,]
# Calibration curves:
plots$cc


