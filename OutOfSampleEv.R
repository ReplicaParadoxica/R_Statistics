#Fitting a linear regression model to the data, with selling_price as the response variable and the remaining variables as explanatory variables.
#Performing an out-of-sample evaluation of the model
#Packages: caret
library(openxlsx)

estates <- read.xlsx("H:\\Desktop\\Python\\Myproject\\bioinformatics_Rproject\\data_xlsx\\estates.xlsx")
View(estates)
estates <- na.omit(estates)

library(caret)
tc <- trainControl(method = "LOOCV")
m <- train(selling_price ~ .,
           data = estates,
           method = "lm",
           trControl = tc)

#Refitting
# Using 10-fold cross validation to evaluate.
library(caret)
# Run this several times
tc <- trainControl(method = "repeatedcv",
                   number = 10, repeats = 100)
m <- train(selling_price ~ .,
           data = estates,
           method = "lm",
           trControl = tc)
m$results

# Check MAE range each time


#Refitting, this time by bootstrap evaluation
library(caret)
# Run this several times:
tc <- trainControl(method = "boot",
                   number = 999)
m <- train(selling_price ~ .,
           data = estates,
           method = "lm",
           trControl = tc)
m$results
# variability is similar to the with repeated 10-fold cross-validation
