library(caret)


data <- read.csv("data/full.csv", header = TRUE)

## Remove irrelevant columns
data <- subset(data, select = -c(X, id))

## Remove rows with missing data
data <- na.omit(data)

## (For clustering) Create a version of data with only numeric variables
data.num <- data
data.num$Class <- as.numeric(as.factor(data.num$Class))  ## 1: Business, 2: Eco , 3: Eco Plus
dmy <- dummyVars(" ~ .", data = data.num, fullRank = TRUE)
data.num <- data.frame(predict(dmy, newdata = data.num))

## Train-test split
N <- nrow(data)
n.train <- round(0.75*N)
n.test <- N - n.train

set.seed(1)
test.idx <- sample(1:N, size = n.test)

data.train <- data[-test.idx,]
data.test <- data[test.idx,]

## Writing 
write.csv(data.num, "data/full_num.csv", row.names = FALSE)
write.csv(data.train, "data/train.csv", row.names = FALSE)
write.csv(data.test, "data/test.csv", row.names = FALSE)
