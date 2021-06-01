
source("forest_fire.r")
set.seed(3)
X <- matrix(2, 21, 21)
X[11, 11] <- 1
# big fires
#X <- forest.fire(X, .1, .2, TRUE)
X <- forest.fire(X, .2, .4, TRUE)
