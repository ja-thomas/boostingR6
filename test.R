library(R6)
source("Tree.R")
source("Loss.R")
source("Booster.R")


data = list(x = as.matrix(mtcars[, -1]), y = mtcars$mpg)

boost = Booster$new(loss = L2$new())
boost$train(data, 10)
