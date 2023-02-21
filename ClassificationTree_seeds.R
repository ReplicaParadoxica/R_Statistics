# The data is downloaded from the UCI Machine Learning Repository:
# http://archive.ics.uci.edu/ml/datasets/seeds
seeds <- read.table("https://tinyurl.com/seedsdata",
                    col.names = c("Area", "Perimeter", "Compactness",
                                  "Kernel_length", "Kernel_width", "Asymmetry",
                                  "Groove_length", "Variety"))
seeds$Variety <- factor(seeds$Variety)
library(caret)
tc <- trainControl(method = "LOOCV")
m <- train(Variety ~ Kernel_length + Compactness,
           data = seeds,
           trControl = tc,
           method = "rpart",
           tuneGrid = expand.grid(cp = 0))
contour_data <- expand.grid(
  Kernel_length = seq(min(seeds$Kernel_length), max(seeds$Kernel_length), length = 500),
  Compactness = seq(min(seeds$Compactness), max(seeds$Compactness), length = 500))
predictions <- data.frame(contour_data,
                          Variety = as.numeric(predict(m, contour_data)))
library(ggplot2)
ggplot(seeds, aes(Kernel_length, Compactness, colour = Variety)) +
  geom_point(size = 2) +
  stat_contour(aes(x = Kernel_length, y = Compactness, z = Variety),
               data = predictions, colour = "black")

#Seems good
#Most points in the lower left part belong to variety 3, most in the middle to variety 1, and most to the right to variety 2