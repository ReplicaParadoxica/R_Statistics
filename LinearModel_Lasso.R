# Refitting the estates data, but not with Lasso.
# Goal is to observe any imprtovements in RMSE and MAE

library(openxlsx)
estates <- read.xlsx("H:\\Desktop\\Python\\Myproject\\bioinformatics_Rproject\\data_xlsx\\estates.xlsx")
estates <- na.omit(estates)
# Fit a lasso model and evaluate it with LOOCV
library(caret)
tc <- trainControl(method = "LOOCV")
m <- train(selling_price ~ .,
           data = estates,
           method = "glmnet",
           tuneGrid = expand.grid(alpha = 1,
                                  lambda = seq(0, 10, 0.1)),
           trControl = tc)
# Results for the best model:
m$results[which(m$results$lambda == m$finalModel$lambdaOpt),]

# Run to see which variables were removed
# coef(m$finalModel, m$finalModel$lambdaOpt)
