###############################################################-
## package                                                 ####
###############################################################-
rm(list=ls())
library(randomForest)
library(ggplot2)
library(dplyr)
library(progress)
library(shiny)

###############################################################-
## data                                                    ####
###############################################################-
train <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = TRUE)

y.train <- train$satisfaction
x.train <- train %>% select(-satisfaction)

y.test <- train$satisfaction
x.test <- train %>% select(-satisfaction)

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
run.rf <- function(){
  ptm <- proc.time()
  rf.train <<- randomForest(satisfaction~., data=train, mtry=best.mtry, ntree=500)
  rf.time <<- proc.time()-ptm
  print(rf.time)
}
run.rf()

rf.train
rf.time
