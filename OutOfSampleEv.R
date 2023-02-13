#Fitting a linear regression model to the data, with selling_price as the response variable and the remaining variables as explanatory variables.
#Performing an out-of-sample evaluation of the model
#Packages: caret
library(openxlsx)

estates <- read.xlsx("...\\data_xlsx\\estates.xlsx")
View(estates)
estates <- na.omit(estates)

library(caret)
tc <- trainControl(method = "LOOCV")
m <- train(selling_price ~ .,
           data = estates,
           method = "lm",
           trControl = tc)
