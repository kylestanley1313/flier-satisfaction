library(caret)
library(fossil)
library(mclust)


## Pre-processing
data <- read.csv("data/full_num.csv", header = TRUE)
class <- data$Class
X <- subset(data, select = -c(Class))

## Globals
Gs <- 2:6
trials <- 10
ARI <- matrix(NA, nrow = trials, ncol = length(Gs))

## Cluster
set.seed(1)
for (G in Gs) {
  for (t in 1:trials) {
    print(sprintf("G = %s, trial = %s", G, t))
    mod <- Mclust(X, G = G)
    ARI[t,G-1] <- adj.rand.index(class, mod$classification)
  }
}

round(colMeans(ARI), 3)


