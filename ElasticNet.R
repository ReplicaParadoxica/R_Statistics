# Refitting the estates data, but not with elastic net.
# Goal is to observe any imprtovements in RMSE and MAE

library(openxlsx)
estates <- read.xlsx("...\\data_xlsx\\estates.xlsx")
estates <- na.omit(estates)

library(caret)
tc <- trainControl(method = "LOOCV")
m <- train(selling_price ~ .,
           data = estates,
           method = "glmnet",
           tuneGrid = expand.grid(alpha = seq(0, 1, 0.2),
                                  lambda = seq(10, 20, 1)),
           trControl = tc)
# Print best choices of alpha and lambda:
m$bestTune
# Print the RMSE and MAE for the best model:
m$results[which(rownames(m$results) == rownames(m$bestTune)),]

# Getting a slight improvement over the lasso