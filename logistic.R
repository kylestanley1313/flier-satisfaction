library(glmnet)

train <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = TRUE)


## Base Model ==================================================================

# NOTE: 1 --> satisfied, 0 --> neutral or dissatisfied

glm.fit <- glm(
  satisfaction ~ .,
  data = train,
  family = binomial
)
glm.probs.train <- predict(glm.fit, type = "response")
glm.probs.test <- predict(glm.fit, type = "response", newdata = test)
glm.preds.train <- rep("neutral or dissatisfied", nrow(train))
glm.preds.train[glm.probs.train > 0.5] = "satisfied"
glm.preds.test <- rep("neutral or dissatisfied", nrow(test))
glm.preds.test[glm.probs.test > 0.5] = "satisfied"
tab.train <- table(glm.preds.train, train$satisfaction)
tab.test <- table(glm.preds.test, test$satisfaction)
sprintf(
  "Proportion of test set correctly classified: %s", 
  round((tab.test[1,1] + tab.test[2,2])/nrow(test), 3)
)
# NOTE: 0.873 correct classification rate to serve as our baseline


## LASSO =======================================================================

## Model Fitting ---------------------------------------------------------------

## Re-load data without converting characters to factors
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)
X.train <- model.matrix(satisfaction ~ ., data = train)[, -1]
y.train <- train$satisfaction
X.test <- model.matrix(satisfaction ~ ., data = test)[, -1]
y.test <- test$satisfaction

## Fit LASSO model
start <- Sys.time()
fit.lasso <- glmnet(X.train, y.train, alpha = 1, family = 'binomial')
end <- Sys.time()
sprintf(
  "Fitting time: %s seconds", 
  round(end - start, 3)
)
plot(fit.lasso, xvar = 'lambda')

## Choose lambda via CV, then compute correct classification rate
set.seed(1)
start <- Sys.time()
cv.out <- cv.glmnet(X.train, y.train, alpha = 1, family = 'binomial')
end <- Sys.time()
sprintf(
  "CV time: %s seconds", 
  round(end - start, 3)
)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.probs.train <- predict(fit.lasso, s = bestlam, newx = X.train, type = "response")
lasso.probs.test <- predict(fit.lasso, s = bestlam, newx = X.test, type = "response")
lasso.preds.test <- rep("neutral or dissatisfied", nrow(test))
lasso.preds.test[lasso.probs.test > 0.5] = "satisfied"
tab.test <- table(lasso.preds.test, test$satisfaction)
sprintf(
  "Proportion of test set correctly classified: %s", 
  round((tab.test[1,1] + tab.test[2,2])/nrow(test), 3)
)
lasso.coef <- predict(fit.lasso, type = 'coefficients', s = bestlam)
lasso.coef
## NOTES: 
##    - Slightly more interpretable model as LASSO removes Gate.location and Departure.Delay.in.Minutes
##    - Same correct classification rate as baseline model (0.837)


## Model Evaluation ------------------------------------------------------------

## Computing time --> see above

## Classification error rate -- see above

## ROC curves
prob.cutoffs <- seq(0, 1, by = 0.01)
FPR.train <- rep(NA, length(prob.cutoffs))
TPR.train <- rep(NA, length(prob.cutoffs))
FPR.test <- rep(NA, length(prob.cutoffs))
TPR.test <- rep(NA, length(prob.cutoffs))

for (i in 1:length(prob.cutoffs)) {
  
  lasso.preds.test <- rep('neutral or dissatisfied', nrow(test))
  lasso.preds.test[lasso.probs.test > prob.cutoffs[i]] = 'satisfied'
  FPR.test[i] <- sum(lasso.preds.test == 'satisfied' & y.test != 'satisfied') / sum(y.test != 'satisfied')
  TPR.test[i] <- sum(lasso.preds.test == 'satisfied' & y.test == 'satisfied') / sum(y.test == 'satisfied')
  
  lasso.preds.train <- rep('neutral or dissatisfied', nrow(train))
  lasso.preds.train[lasso.probs.train > prob.cutoffs[i]] = 'satisfied'
  FPR.train[i] <- sum(lasso.preds.train == 'satisfied' & y.train != 'satisfied') / sum(y.train != 'satisfied')
  TPR.train[i] <- sum(lasso.preds.train == 'satisfied' & y.train == 'satisfied') / sum(y.train == 'satisfied')
  
}

plot(FPR.train, TPR.train, type = 'line', xlab = "FPR", ylab = "TPR", main = "Logistic Regression ROC")
lines(FPR.test, TPR.test, col = 'red')
legend(0.8, 0.2, legend = c("train", "test"), lty = c(1, 1), col = c('black', 'red'))


