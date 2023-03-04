bacteria <- read.csv("...\\data_csv\\bacteria.csv")
bacteria$Time <- as.POSIXct(bacteria$Time, format = "%H:%M:%S")
#Making use of caret library, splitting into training set
library(caret)
tc <- trainControl(method = "LOOCV")
m <- train(OD ~ Time,
           data = bacteria[45:90,],
           trControl = tc,
           method = "rf",
           tuneGrid = expand.grid(mtry = 1))
#Making prediction for the whole dataset
bacteria$Predicted <- predict(m, bacteria)
library(ggplot2)
ggplot(bacteria, aes(Time, OD)) +
  geom_line() +
  geom_line(aes(Time, Predicted), colour = "red")
