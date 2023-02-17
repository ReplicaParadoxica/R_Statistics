# Refitting OutOfSampleEv.R, but this time use ridge regression instead


library(openxlsx)
estates <- read.xlsx("...\\data_xlsx\\estates.xlsx")
estates <- na.omit(estates)

library(caret)
tc <- trainControl(method = "LOOCV")
m <- train(selling_price ~ .,
           data = estates,
           method = "glmnet",
           tuneGrid = expand.grid(alpha = 0,
                                  lambda = seq(0, 10, 0.1)),
           trControl = tc)
# Results for the best model:
m$results[which(m$results$lambda == m$finalModel$lambdaOpt),]

# Rerunning the code, allowing for higher values of lambda
m <- train(selling_price ~ .,
           data = estates,
           method = "glmnet",
           tuneGrid = expand.grid(alpha = 0,
                                  lambda = seq(10, 120, 1)),
           trControl = tc)
# Results for the best model:
m$results[which(m$results$lambda == m$finalModel$lambdaOpt),]

# In this case ridge regression did not improve the performance of the model compared to an ordinary linear regression