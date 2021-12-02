###############################################################-
## EDA for classification                                  ####
###############################################################-

###############################################################-
## package                                                 ####
###############################################################-
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

###############################################################-
## data                                                    ####
###############################################################-
train <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = TRUE)

y.train <- train$satisfaction
x.train <- train %>% select(-satisfaction)

y.test <- test$satisfaction
x.test <- test %>% select(-satisfaction)

temp <- train # copy the train
names(temp)
temp %>% str()

## transform data into appropirate form for plotting-----------
temp$satisfaction <- ifelse(temp$satisfaction=='satisfied',
                            'satisfied','not satisfied') %>% as.factor()

temp$Inflight.wifi.service <- temp$Inflight.wifi.service %>%  as.factor()
temp$Online.boarding <- temp$Online.boarding %>%  as.factor()
temp$Cleanliness <- temp$Cleanliness %>% as.factor()
temp$Food.and.drink <- temp$Food.and.drink %>% as.factor()
temp$Seat.comfort <- temp$Seat.comfort %>% as.factor()

## satisfaction -----------------------------------------------
p <- ggplot(temp, aes(satisfaction,))
p1 <- p+geom_bar();p1

## categorical variables vs satisfaction
p2 <- p+geom_bar(aes(fill=Type.of.Travel),position="fill");p2
p3 <- p+geom_bar(aes(fill=Class),position="fill");p3
p9 <- p+geom_bar(aes(fill=Online.boarding),position="fill");p9
p10 <- p+geom_bar(aes(fill=Cleanliness),position="fill");p10
p11 <- p+geom_bar(aes(fill=Food.and.drink),position="fill");p11
p12 <- p+geom_bar(aes(fill=Seat.comfort),position="fill");p12
#grid.arrange(p2,p3,p9,p11, nrow=2, ncol=2)

## continuous variables vs satisfaction
p5 <- p+geom_boxplot(aes(satisfaction,Departure.Delay.in.Minutes));p5
p6 <- p+geom_boxplot(aes(satisfaction,Arrival.Delay.in.Minutes));p6
p7 <- p+geom_boxplot(aes(satisfaction,Flight.Distance));p7
p8 <- p+geom_boxplot(aes(satisfaction,Age));p8
#grid.arrange(p5,p6,p7,p8, nrow=2, ncol=2)

## mixed 2 by 2 panel
plot1 <- grid.arrange(p3,p11,p7,p8, nrow=2, ncol=2);plot1

## predictors -------------------------------------------
## conti vs conti
# ggplot(temp, aes(Flight.Distance, Arrival.Delay.in.Minutes))+
#   geom_point(aes(color=satisfaction))

plot2 <- ggplot(temp, aes(Departure.Delay.in.Minutes, Arrival.Delay.in.Minutes))+
  geom_point(aes(color=satisfaction));plot2

## cat vs cat
tb <- table(temp$Online.boarding, temp$Inflight.wifi.service)
tb <- as.data.frame(tb) 
colnames(tb)=c("Online.boarding", "Inflight.wifi.service", "Freq")

# plot3 <- ggplot(tb, aes(Online.boarding, Freq,
#                  fill=Online.boarding))+
#   geom_bar(stat = 'identity', position = 'dodge')+
#   facet_wrap(~Inflight.wifi.service);plot3

plot3 <- ggplot(tb, aes(Online.boarding, Freq,
                        fill=Inflight.wifi.service))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(~Inflight.wifi.service);plot3


## Plot to use ----------------------------------------
grid.arrange(p3,p11,p7,p8, nrow=2, ncol=2)
grid.arrange(plot2, plot3, nrow=2, ncol=1)

###############################################################-
## EDA for clustering                                      ####
###############################################################-

###############################################################-
## data                                                    ####
###############################################################-
data <- read.csv("data/full_num.csv", header = TRUE, stringsAsFactors = TRUE)
class <- data$Class                   # response(label)
X <- subset(data, select = -c(Class)) # predictors

# Change class variable from number to character
temp <- data # copy the data

temp2 <- read.csv("data/full.csv", header = TRUE, stringsAsFactors = TRUE)
temp2 <- subset(temp2, select = -c(X, id))
temp2 <- na.omit(temp2)

temp$Class <- temp2$Class


## transform data into appropirate form for plotting-----------
temp$Satisfied <- temp$satisfactionsatisfied %>% as.factor()
temp$Inflight.wifi.service <- temp$Inflight.wifi.service %>%  as.factor()
temp$Online.boarding <- temp$Online.boarding %>%  as.factor()
temp$Cleanliness <- temp$Cleanliness %>% as.factor()
temp$Food.and.drink <- temp$Food.and.drink %>% as.factor()
temp$Seat.comfort <- temp$Seat.comfort %>% as.factor()


## PCA -----------------------------------------------
X.pca <- prcomp(X, center = TRUE, scale. = TRUE, rank. = 2)

par(mfrow=c(1,1))
plot(x.pca, col=class, main='Class')
# It is hard to do clustering

## Class ------------------------------
p <- ggplot(temp, aes(Class))

p2 <- p+geom_bar(aes(fill=Satisfied),position="fill");p2
p10 <- p+geom_bar(aes(fill=Cleanliness),position="fill");p10
p7 <- p+geom_boxplot(aes(Class,Flight.Distance));p7
p8 <- p+geom_boxplot(aes(Class,Age));p8

# plot to use
plot1 <- grid.arrange(p2,p10,p7,p8, nrow=2, ncol=2);plot1

