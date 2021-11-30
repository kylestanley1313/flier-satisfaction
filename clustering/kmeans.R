###############################################################-
## package                                                 ####
###############################################################-
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(fossil)     # rand inex
library(mclust)     # rand inex
library(gridExtra)  # griding ggplot2
###############################################################-
## data                                                    ####
###############################################################-
#setwd('/Users/chaegeunsong/GitHub/flier-satisfaction')
data <- read.csv("data/train.csv", header = TRUE, stringsAsFactors = TRUE)
class <- data$Class                   # response(label)
X <- subset(data, select = -c(Class)) # predictors

## class mapping
class.map <- c('Business' = 2, 'Eco' = 1, 'Eco Plus' = 3)
trueclass <- class.map[as.character(class)]

## turn categorical variables to numerical variables
x <- sapply(X, as.numeric)
x <- as.data.frame(x)

###############################################################-
## K-Means                                                ####
###############################################################-
## 1-----------------------------------------------------------
## Determine the optimal number of clusters by using elbow method

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(x, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 6
k.values <- 1:6

# extract wss for 1-6 clusters
set.seed(1)
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow method")
# Abrupt change at k=2 and significant reduction at k=3
# It appears to be stable at k=3
# Hence, 3 the optimal number of clusters

## Run kmeans with different initializations
# Fix k=3 and change nstart(initial assignment)
k <- 3
n.values <- 1:15

cluster.output <- function(nstart){
  kmeans(x, centers = k, nstart)$cluster
}

# get each cluster result
set.seed(1)
cluster.values <- lapply(n.values, cluster.output)

cluster.values[[2]] %>% table()
trueclass %>% table()
class %>% table()

# calculate adjusted rand index(ARI)
ARI <- rep(NA, max(n.values))
for(i in n.values){
  ARI[i] <- adj.rand.index(trueclass, cluster.values[[i]])
}
ARI

# plot the ARI
plot(n.values, ARI,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of initial assignment",
     ylab="ARI values",
     main="Adjusted rand index(ARI)")
# At k=3, it is same except when nstarat=1
# At different k, it varies a lot

## 2-----------------------------------------------------------
# When k=2 ------------------------------------------------
# Fix k=2 and change nstart(initial assignment)
k <- 2
n.values <- 1:15

cluster.output <- function(nstart){
  kmeans(x, centers = k, nstart)$cluster
}

# get each cluster result
set.seed(1)
cluster.values <- lapply(n.values, cluster.output)

# calculate adjusted rand index(ARI)
ARI2 <- rep(NA, max(n.values))
for(i in n.values){
  ARI2[i] <- adj.rand.index(trueclass, cluster.values[[i]])
}
ARI2

# plot the ARI
plot(n.values, ARI2,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of initial assignment",
     ylab="ARI values",
     main="Adjusted rand index(ARI) with k=2")

# When k=3 ------------------------------------------------ 
# Fix k=3 and change nstart(initial assignment)
k <- 3
n.values <- 1:15

cluster.output <- function(nstart){
  kmeans(x, centers = k, nstart)$cluster
}

# get each cluster result
set.seed(1)
cluster.values <- lapply(n.values, cluster.output)

# calculate adjusted rand index(ARI)
ARI3 <- rep(NA, max(n.values))
for(i in n.values){
  ARI3[i] <- adj.rand.index(trueclass, cluster.values[[i]])
}
ARI3

# plot the ARI
plot(n.values, ARI3,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of initial assignment",
     ylab="ARI values",
     main="Adjusted rand index(ARI) with k=3")

# When k=4 ------------------------------------------------ 
# Fix k=5 and change nstart(initial assignment)
k <- 4
n.values <- 1:15

cluster.output <- function(nstart){
  kmeans(x, centers = k, nstart)$cluster
}

# get each cluster result
set.seed(1)
cluster.values <- lapply(n.values, cluster.output)

# calculate adjusted rand index(ARI)
ARI4 <- rep(NA, max(n.values))
for(i in n.values){
  ARI4[i] <- adj.rand.index(trueclass, cluster.values[[i]])
}
ARI4

# plot the ARI
plot(n.values, ARI4,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of initial assignment",
     ylab="ARI values",
     main="Adjusted rand index(ARI) with k=4")

## Compare
mean(ARI2);mean(ARI3);mean(ARI4)

## visualization ---------------------------------------
k2 <- kmeans(x, centers = 2, nstart = 10)
k3 <- kmeans(x, centers = 3, nstart = 10)
k4 <- kmeans(x, centers = 4, nstart = 10)

p2 <- fviz_cluster(k2, geom = "point",  data = x) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = x) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = x) + ggtitle("k = 4")

grid.arrange(p2, p3, p4, nrow = 2)


