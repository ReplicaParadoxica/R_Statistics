# Fitting a decision tree model and a random forest to the wine data

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
                   summaryFunction = twoClassSummary,
                   savePredictions = TRUE,
                   classProbs = TRUE)
m1 <- train(type ~ .,
            data = wine,
            trControl = tc,
            method = "rpart",
            metric = "ROC",
            tuneGrid = expand.grid(cp = c(0, 0.1, 0.01)))
m2 <- train(type ~ .,
            data = wine,
            trControl = tc,
            method = "rf",
            metric = "ROC",
            tuneGrid = expand.grid(mtry = 2:6))

# visually compare
m1
m2
library(MLeval)
plots <- evalm(list(m1, m2),
               gnames = c("Decision tree", "Random forest"))
# ROC:
plots$roc
# Calibration curves:
plots$cc
# Histogram
hist(predict(m2, type ="prob")[,2])