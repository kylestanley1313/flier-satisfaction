library(fossil)
library(mclust)


## Pre-processing
data <- read.csv("data/train.csv", header = TRUE, stringsAsFactors = TRUE)
class <- data$Class
X <- subset(data, select = -c(Class))
cols.factor <- c('Gender', 'Customer.Type', 'Type.of.Travel', 'satisfaction')
X[cols.factor] <- sapply(X[cols.factor], as.numeric)


## PCA
X.pca <- prcomp(X, center = TRUE, scale. = TRUE, rank. = 2)

## GMM
class.map <- c('Business' = 1, 'Eco' = 2, 'Eco Plus' = 3)
Gs <- 2:6
trials <- 5
ARI <- matrix(NA, nrow = trials, ncol = length(Gs))

set.seed(1)
for (G in Gs) {
  for (t in 1:trials) {
    print(sprintf("G = %s, trial = %s", G, t))
    mod <- Mclust(X.pca$x, G = G)
    ARI[t,G-1] <- adj.rand.index(class.map[as.character(class)], mod$classification)
  }
}
colMeans(ARI)

set.seed(1)
mod2 <- Mclust(X.pca$x, G = 2)
mod3 <- Mclust(X.pca$x, G = 3)
mod4 <- Mclust(X.pca$x, G = 4)
mod5 <- Mclust(X.pca$x, G = 5)
mod6 <- Mclust(X.pca$x, G = 6)

par(mfrow = c(2, 3))
plot(X.pca$x, col = class, main = "Truth")
plot(X.pca$x, col = mod2$classification, main = "K = 2")
plot(X.pca$x, col = mod3$classification, main = "K = 3")
plot(X.pca$x, col = mod4$classification, main = "K = 4")
plot(X.pca$x, col = mod5$classification, main = "K = 5")
plot(X.pca$x, col = mod6$classification, main = "K = 6")

## NOTES:
##  - 5 cluster model performs best
##  - Possible that as K increases
