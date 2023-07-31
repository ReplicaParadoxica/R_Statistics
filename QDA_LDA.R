# Fitting LDA/QDA

library(caret)
library(ggplot2)

seeds <- read.table("https://tinyurl.com/seedsdata",
                    col.names = c("Area", "Perimeter", "Compactness",
                                  "Kernel_length", "Kernel_width", "Asymmetry",
                                  "Groove_length", "Variety"))


seeds$Variety <- factor(seeds$Variety)
tc <- trainControl(method = "LOOCV")
m1 <- train(Variety ~ Kernel_length + Compactness,
            data = seeds,
            trControl = tc,
            method = "lda")

m2 <- train(Variety ~ Kernel_length + Compactness,
            data = seeds,
            trControl = tc,
            method = "qda")

contour_data <- expand.grid(
  Kernel_length = seq(min(seeds$Kernel_length), max(seeds$Kernel_length), length = 500),
  Compactness = seq(min(seeds$Compactness), max(seeds$Compactness), length = 500))


predictions1 <- data.frame(contour_data,
                           Variety = as.numeric(predict(m1, contour_data)))
predictions2 <- data.frame(contour_data,
                           Variety = as.numeric(predict(m2, contour_data)))


ggplot(seeds, aes(Kernel_length, Compactness, colour = Variety)) +
  geom_point(size = 2) +
  stat_contour(aes(x = Kernel_length, y = Compactness, z = Variety),
               data = predictions1, colour = "black") +
  stat_contour(aes(x = Kernel_length, y = Compactness, z = Variety),
               data = predictions2, colour = "orange")

# The decision boundaries exhibit considerable similarity and appear quite reasonable
# While QDA provides more flexible, non-linear boundaries, the disparity is not significant
# So the advantage is not THAT significant
