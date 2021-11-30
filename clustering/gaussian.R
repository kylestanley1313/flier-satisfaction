library(fossil)
library(mclust)


## Pre-processing
data <- read.csv("data/train.csv", header = TRUE, stringsAsFactors = TRUE)
class <- data$Class
X <- subset(data, select = -c(Class))

## Globals
class.map <- c('Business' = 1, 'Eco' = 2, 'Eco Plus' = 3)
Gs <- 2:5
trials <- 5
ARI <- matrix(NA, nrow = trials, ncol = length(Gs))

## Cluster
set.seed(1)
for (G in Gs) {
  for (t in 1:trials) {
    print(sprintf("G = %s, trial = %s", G, t))
    mod <- Mclust(X, G = G)
    ARI[t,G-1] <- adj.rand.index(class.map[as.character(class)], mod$classification)
  }
}

colMeans(ARI)


# ## G = 2
# mod2 <- Mclust(X, G = 2)
# summary(mod2)
# adj.rand.index(class.map[as.character(class)], mod2$classification)
# 
# ## G = 3
# mod3 <- Mclust(X, G = 3)
# summary(mod3)
# adj.rand.index(class.map[as.character(class)], mod3$classification)
# 
# ## G = 4
# mod4 <- Mclust(X, G = 4)
# summary(mod4)
# adj.rand.index(class.map[as.character(class)], mod4$classification)
# 
# ## G = 5
# mod5 <- Mclust(X, G = 5)
# summary(mod5)
# adj.rand.index(class.map[as.character(class)], mod5$classification)




# ## Practice
# class <- iris$Species
# X <- iris[,1:4]
# 
# mod1 <- Mclust(X)
# summary(mod1)
# 
# mod2 <- Mclust(X, G = 3)
# summary(mod2)
# 
# clPairs(X, class)
# 
# plot(mod2)
# table(class, mod2$classification)
# 
# g1 <- sample(1:2, size=10, replace=TRUE)
# g2 <- sample(1:3, size=10, replace=TRUE)
# 
# rand.index(class, mod2$classification)
# 
# class.map <- c('setosa' = 2, 'versicolor' = 1, 'virginica' = 3)
# adj.rand.index(class.map[as.character(class)], mod2$classification)
