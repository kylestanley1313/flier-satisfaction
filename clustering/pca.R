library(fossil)
library(mclust)


## Pre-processing
data <- read.csv("data/full_num.csv", header = TRUE)
class <- data$Class
X <- subset(data, select = -c(Class))

## PCA
X.pca <- prcomp(X, center = TRUE, scale. = TRUE, rank. = 2)

## GMM
Gs <- 2:6
trials <- 10
ARI <- matrix(NA, nrow = trials, ncol = length(Gs))

set.seed(1)
for (G in Gs) {
  for (t in 1:trials) {
    print(sprintf("G = %s, trial = %s", G, t))
    mod <- Mclust(X.pca$x, G = G)
    ARI[t,G-1] <- adj.rand.index(class, mod$classification)
  }
}
round(colMeans(ARI), 3)

set.seed(1)
mod2 <- Mclust(X.pca$x, G = 2)
mod3 <- Mclust(X.pca$x, G = 3)
mod4 <- Mclust(X.pca$x, G = 4)
mod5 <- Mclust(X.pca$x, G = 5)
mod6 <- Mclust(X.pca$x, G = 6)

par(mfrow = c(2, 3))
plot(X.pca$x, col = class, main = "True")
plot(X.pca$x, col = mod2$classification, main = "K = 2")
plot(X.pca$x, col = mod3$classification, main = "K = 3")
plot(X.pca$x, col = mod4$classification, main = "K = 4")
plot(X.pca$x, col = mod5$classification, main = "K = 5")
plot(X.pca$x, col = mod6$classification, main = "K = 6")

## NOTES:
##  - 6 cluster model performs best
##  - Possible that as K increases
