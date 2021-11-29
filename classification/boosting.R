###############################################################-
## package                                                 ####
###############################################################-
library(gbm)
library(ggplot2)
library(dplyr)

###############################################################-
## data                                                    ####
###############################################################-
train <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = TRUE)

y.train <- train$satisfaction
x.train <- train %>% select(-satisfaction)

y.test <- test$satisfaction
x.test <- test %>% select(-satisfaction)

# make the response into binary 0,1
bi.train <- cbind(x.train, y=1*(y.train=='satisfied'))

###############################################################-
## Boosted Tree                                            ####
###############################################################-
## Hyper parameter tuning -------------------------------------
# Hyper parameter here: n.trees, interaction.depth
# Do grid search
hyper_grid <- expand.grid(
  interaction.depth = c(1, 2, 3),
  n.trees = c(100, 200, 300),               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

## Initializes the progress bar
nIter <- nrow(hyper_grid)
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nIter,  # Maximum value of the progress bar
                     style = 3,    # Progress bar style 
                     width = 50,   # Progress bar width. 
                     char = "=")   # Character used to create the bar

## grid search 
for(i in 1:nIter) {
  set.seed(1)
  # train model
  gbm.tune <- gbm(
    formula = y ~ .,
    distribution = "bernoulli",
    data = bi.train,
    n.trees = hyper_grid$n.trees[i],
    interaction.depth = hyper_grid$interaction.depth[i],
    cv.folds = 10
  )
  # add min training error to grid
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$cv.error))
  
  setTxtProgressBar(pb, i)
}

## check the result of grid search
hyper_grid %>% arrange(min_RMSE)

## get the hyper tuning parameter: n.tree=3, interaction.depth=300
ntree <- hyper_grid$n.trees[which.min(hyper_grid$min_RMSE)]
interactiondepth <- hyper_grid$interaction.depth[which.min(hyper_grid$min_RMSE)]


## Grow the Boosted Tree -------------------------------------------
set.seed(1)
run.gbm <- function(){
  ptm <- proc.time()
  gbm.train <<- gbm(y~., data = bi.train, distribution = 'bernoulli',
                    n.trees = ntree, interaction.depth = interactiondepth)
  gbm.time <<- proc.time()-ptm
  print(gbm.time)
}
run.gbm()

gbm.train # Boosted tree
gbm.time  # computing time

# get fitted probability
gbm.train.pred.prob <- predict(gbm.train, newdata=x.train, type='response')

# relative influence plot
summary(gbm.train)
# Online.boarding, Inflight.wifi.service, Type.of.Travle are important

# partial dependence plot
plot(gbm.train, i='Online.boarding')
plot(gbm.train, i='Inflight.wifi.service')
plot(gbm.train, i='Class')
plot(gbm.train, i='Type.of.Travel')


## testing ----------------------------------------------------
# prediction
gbm.pred.prob <- predict(gbm.train, newdata=x.test, type='response')
gbm.pred <- 1*(gbm.pred.prob>0.5)

# confusion matrix
tab.test <- table( as.factor(y.test), as.factor(gbm.pred))
tab.test

###############################################################-
## Model evaluation                                        ####
###############################################################-
## computing time ---------------------------------------------
gbm.time 

## classification error----------------------------------------
## correct rate: 0.951
(correct <- ((tab.test %>% diag() %>% sum())/length(y.test)) %>% round(.,3))

## error rate: 0.049
(error <- 1-correct)

## ROC curve --------------------------------------------------
library(ROSE)
roc.curve(y.train, gbm.train.pred.prob, col='black', lty=1, lwd=2, 
          main='Boosted Tree') # AUC: 0.991
roc.curve(y.test, gbm.pred.prob, col='red', lty=2, lwd=2,add=TRUE)
legend("bottomright", c('train', 'test'), 
       col=c('black', 'red'), lty=1:2, lwd=2) # AUC:0.990
