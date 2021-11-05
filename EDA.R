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

###############################################################-
## data                                                    ####
###############################################################-
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
p2 <- p+geom_bar(aes(fill=Type.of.Travel));p2
p3 <- p+geom_bar(aes(fill=Class));p3
p9 <- p+geom_bar(aes(fill=Online.boarding));p9
p10 <- p+geom_bar(aes(fill=Cleanliness));p10
p11 <- p+geom_bar(aes(fill=Food.and.drink));p11
p12 <- p+geom_bar(aes(fill=Seat.comfort));p12
grid.arrange(p2,p3,p9,p11, nrow=2, ncol=2)

## continuous variables vs satisfaction
p5 <- p+geom_boxplot(aes(satisfaction,Departure.Delay.in.Minutes));p5
p6 <- p+geom_boxplot(aes(satisfaction,Arrival.Delay.in.Minutes));p6
p7 <- p+geom_boxplot(aes(satisfaction,Flight.Distance));p7
p8 <- p+geom_boxplot(aes(satisfaction,Age));p8
grid.arrange(p5,p6,p7,p8, nrow=2, ncol=2)

## mixed 2 by 2 panel
grid.arrange(p3,p11,p7,p8, nrow=2, ncol=2)

## predictors -------------------------------------------
## conti vs conti
ggplot(temp, aes(Flight.Distance, Arrival.Delay.in.Minutes))+
  geom_point(aes(color=satisfaction))

ggplot(temp, aes(Departure.Delay.in.Minutes, Arrival.Delay.in.Minutes))+
  geom_point(aes(color=satisfaction))

## cat vs cat
tb <- table(temp$Online.boarding, temp$Inflight.wifi.service)
tb <- as.data.frame(tb) 
colnames(tb)=c("Online.boarding", "Inflight.wifi.service", "Freq")

ggplot(tb, aes(Online.boarding, Freq,
                 fill=Inflight.wifi.service))+
  geom_bar(stat = 'identity', position = 'dodge')

## cat vs conti
