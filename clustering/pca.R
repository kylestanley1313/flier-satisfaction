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


# ## Visualize
# plot(X.pca$x, col = class)
# 
# 
# 
# 
# ## Practice
# mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE, scale. = TRUE, rank. = 2)
# summary(mtcars.pca)
# str(mtcars.pca)
# mtcars.pca$x
