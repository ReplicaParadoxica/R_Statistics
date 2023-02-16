# Fit a logistic ridge regression model to the wine data

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
tc <- trainControl(method = "cv",
                   number = 10,
                   savePredictions = TRUE,
                   classProbs = TRUE)
m1 <- train(type ~ pH + alcohol + fixed.acidity + residual.sugar,
            data = wine,
            method = "glmnet",
            family = "binomial",
            tuneGrid = expand.grid(alpha = 0,
                                   lambda = seq(0, 10, 0.1)),
            trControl = tc)
m1

# Setting summaryFunction = twoClassSummary in trainControl and metric = "ROC" in train
# And refitting the model using AUC to find the optimal lambda

tc <- trainControl(method = "cv",
                   number = 10,
                   summaryFunction = twoClassSummary,
                   savePredictions = TRUE,
                   classProbs = TRUE)
m2 <- train(type ~ pH + alcohol + fixed.acidity + residual.sugar,
            data = wine,
            method = "glmnet",
            family = "binomial",
            tuneGrid = expand.grid(alpha = 0,
                                   lambda = seq(0, 10, 0.1)),
            metric = "ROC",
            trControl = tc)
m2