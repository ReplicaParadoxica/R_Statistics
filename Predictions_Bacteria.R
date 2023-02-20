bacteria <- read.csv("...\\data_csv\\bacteria.csv")
bacteria$Time <- as.POSIXct(bacteria$Time, format = "%H:%M:%S")

library(caret)
tc <- trainControl(method = "LOOCV")
m <- train(OD ~ Time,
           data = bacteria[45:90,],
           trControl = tc,
           method = "rpart",
           tuneGrid = expand.grid(cp = 0))

#make predictions for the entire dataset and compare the results to the actual outcomes
bacteria$Predicted <- predict(m, bacteria)
library(ggplot2)
ggplot(bacteria, aes(Time, OD)) +
  geom_line() +
  geom_line(aes(Time, Predicted), colour = "red")