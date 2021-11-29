library(fossil)
library(mclust)


## Pre-processing
data <- read.csv("data/train.csv", header = TRUE, stringsAsFactors = TRUE)
class <- data$Class
X <- subset(data, select = -c(Class))

## Globals
class.map <- c('Business' = 2, 'Eco' = 1, 'Eco Plus' = 3)

## G = 2
mod2 <- Mclust(X, G = 2)
summary(mod2)
adj.rand.index(class.map[as.character(class)], mod2$classification)

## G = 3
mod3 <- Mclust(X, G = 3)
summary(mod3)
adj.rand.index(class.map[as.character(class)], mod2$classification)

## G = 4
mod4 <- Mclust(X, G = 4)
summary(mod4)
adj.rand.index(class.map[as.character(class)], mod2$classification)




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
