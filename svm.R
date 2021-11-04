library(e1071)

train <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = TRUE)


## Fitting
fit.svm.poly <- svm(satisfaction ~ ., data = train, kernel = 'polynomial', degree = 3, cost = 10)

## Preliminary evaluation
pred.poly <- predict(fit.svm.poly, newdata = test)
tab.test <- table(true = test$satisfaction, pred = pred.poly)
sprintf(
  "Proportion of test set correctly classified: %s", 
  round((tab.test[1,1] + tab.test[2,2])/nrow(test), 3)
)
## NOTE: 0.943 correct classification rate


## ROC



## NOTE: This commented out code takes a VERY long time to run. I decided 
##       to do this cross validation in batches then save summary outputs to
##       separate _scratch.txt files that I haven't added to git. After some
##       piecemeal cross validation I found that using a polynomial kernel with
##       cost = 10, degree = 3 provided a reasonable fit.
# ## Parameter tuning
# sink('tuning_summaries_polynomial2_scratch.txt')
# set.seed(1)
# tune.out.linear <- tune(
#   svm,
#   satisfaction ~ .,
#   data = train,
#   kernel = 'linear',
#   ranges = list(
#     cost = c(0.1, 1, 10)
#   )
# )
# summary(tune.out.linear)
# 
# set.seed(1)
# tune.out.poly <- tune(
#   svm,
#   satisfaction ~ .,
#   data = train,
#   kernel = 'polynomial',
#   ranges = list(
#     cost = c(0.1, 1, 10, 100, 1000),
#     degree = c(3, 5, 7, 9)
#   )
# )
# summary(tune.out.poly)
# 
# set.seed(1)
# tune.out.radial <- tune(
#   svm,
#   satisfaction ~ .,
#   data = train,
#   kernel = 'radial',
#   ranges = list(
#     cost = c(0.1, 1, 10, 100, 1000),
#     gamma = c(0.5, 1, 2, 3, 4)
#   )
# )
# summary(tune.out.radial)
# 
# sink()


