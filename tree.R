###############################################################-
## package                                                 ####
###############################################################-
library(randomForest)
library(ggplot2)
library(dplyr)
library(pROC)

###############################################################-
## data                                                    ####
###############################################################-
train <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = TRUE)

y.train <- train$satisfaction
x.train <- train %>% select(-satisfaction)

y.test <- test$satisfaction
x.test <- test %>% select(-satisfaction)

###############################################################-
## Random Forest                                           ####
###############################################################-
## Tune mtry: search for the optimal vaue of mtry for RF ------
# note that mtry indicates how many variables should be
# considered for split of the tree
set.seed(1)
tune.mtry <- tuneRF(x.train, y.train, ntreeTry=100, trace=TRUE, plot=TRUE)
tune.mtry

# Choose mtry with the smallest OOB error -> mtry = 8
tab.mtry <- tune.mtry %>% as.data.frame()
best.mtry <- tab.mtry %>% filter(OOBError==min(OOBError)) %>% .$mtry 

## Grow the RF -----------------------------------------------
set.seed(1)
run.rf <- function(){
  ptm <- proc.time()
  rf.train <<- randomForest(satisfaction~., 
                            data=train, 
                            mtry=best.mtry, 
                            ntree=100,
                            importance=TRUE)
  rf.time <<- proc.time()-ptm
  print(rf.time)
}
run.rf()

rf.train # random forest
rf.time  # computing time

# Importance 
rf.train$importance 
varImpPlot(rf.train)
# common variables in each top 5:
# inflight wifi service, online boarding, type of travel. 

# The more the accuracy/Gini of the random forest decreases 
# due to the exclusion of a single variable,
# the more important that variable is deemed.

## testing ----------------------------------------------------
# predcition
rf.pred <- predict(rf.train, newdata=x.test)
rf.pred.prob <- predict(rf.train, newdata=x.test, type='prob')

# confusion matrix
tab.test <- table( as.factor(y.test), as.factor(rf.pred))
tab.test

###############################################################-
## Model evaluatoin                                        ####
###############################################################-
## computing time ---------------------------------------------
rf.time 

## classification error----------------------------------------
## correct rate: 0.96
(correct <- ((tab.test %>% diag() %>% sum())/length(y.test)) %>% round(.,3))

## error rate: 0.04
(error <- 1-correct)

## ROC curve --------------------------------------------------
# train
rf.roc.train <- roc(y.train, rf.train$votes[,2])
plot(rf.roc.train, main='ROC curve for train')
auc(rf.roc.train) # 0.9912

# test
rf.roc.test<-roc(y.test, rf.pred.prob[,2])
plot(rf.roc.test, main='ROC curve for test')
auc(rf.roc.test) # 0.9931

## another ROC curves to draw both
library(ROSE)
roc.curve(y.train, rf.train$votes[,2], col='black', lty=1, lwd=2)
roc.curve(y.test, rf.pred.prob[,2], col='red', lty=2, lwd=2,add=TRUE)
legend("bottomright", c("train", "test"), 
       col=c('black', 'red'), lty=1:2, lwd=2)
          