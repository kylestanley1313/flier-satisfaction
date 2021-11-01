data <- read.csv("full.csv", header = TRUE)

## Remove irrelevant columns
data <- data[, -which(names(data) %in% c('X.1','X'))]

## Train-test split
N <- nrow(data)
n.train <- round(0.75*N)
n.test <- N - n.train

set.seed(1)
test.idx <- sample(1:N, size = n.test)

data.train <- data[-test.idx,]
data.test <- data[test.idx,]

write.csv(data.train, "train.csv")
write.csv(data.test, "test.csv")
