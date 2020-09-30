# Assignment 3 Sample Answer Code
# BUDT 758T, Professor Paulson
# Note: please see the sample answer key for complete answers;
#       this file contains only the necessary/helpful code.

#### Question 1

## a. Load the bikeshare data into R. I am NOT going to attach it!
## You may have loaded in your data using RStudio
## If not, you may have used:

bikeshare <- read.csv("bikeshare.csv")

## b.	Change the WEATHERSIT variable to a factor variable using as.factor() in R.

bikeshare$WEATHERSIT=as.factor(bikeshare$WEATHERSIT)

## c.	Create a new variable, REG_85, that is 1 if the percentage of registered bikes on a particular day 
##    was greater than 85% of the total count of bikes and 0 if it is not.

bikeshare$REG_85=ifelse(bikeshare$REGISTERED/bikeshare$COUNT>0.85,1,0)

## d.	Set the seed in R to 8726.

set.seed(8726)

## e.	Randomly partition the data set into a training data set and a test data set.
## Use 75% of the data as training data and hold out the remaining 25% as test data.

num_obs = nrow(bikeshare)
train_obs <- sample(num_obs,0.75*num_obs)
bike_train <- bikeshare[train_obs,]
bike_test <- bikeshare[-train_obs,]


## Question 2

log_model=glm(REG_85~.-COUNT-REGISTERED-CASUAL,data=bike_train,family = "binomial")
summary(log_model)

## part (b)

log_accuracy=rep(0,13)
log_TPR=rep(0,13)
log_TNR=rep(0,13)
cutoffs=c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99)

log_preds=predict(log_model,newdata=bike_test,type="response")

for(i in 1:13){
  
  log_class=ifelse(log_preds>cutoffs[i],1,0)
  confuse_test=table(bike_test$REG_85,log_class)
  log_accuracy[i]=(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)
  log_TPR[i]=(confuse_test[2,2])/sum(confuse_test[2,])
  log_TNR[i]=(confuse_test[1,1])/sum(confuse_test[1,])
  
}

## part (b) i

cbind(cutoffs, log_accuracy)

## part (b) ii

plot(1-log_TNR,log_TPR,xlab="FPR",ylab="TPR",type="l")

## part (b) iii

plot(cutoffs,log_TPR,xlab="Cutoff Value",ylab="Performance Measure",type="l",col="green")
lines(cutoffs,log_TNR,col="red")
lines(cutoffs,log_accuracy,col="blue")


## part (c)

log_class=ifelse(log_preds>0.45,1,0)
confuse_test=table(bike_test$REG_85,log_class,dnn=c("Actual","Predicted"))
confuse_test
(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)


## Question 3

library(MASS)

lda_model=lda(REG_85~.-COUNT-REGISTERED-CASUAL,data=bike_train)

## part a

lda_predict=predict(lda_model,newdata=bike_test)
lda_preds=lda_predict$posterior[,2]
lda_class=ifelse(lda_preds>0.45,1,0)
confuse_test=table(bike_test$REG_85,lda_class,dnn=c("Actual","Predicted"))
confuse_test
(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)

## part d

table(bike_train$WEATHERSIT,bike_train$REG_85)

## If you also checked the others:
table(bike_train$MONTH,bike_train$REG_85)
table(bike_train$HOLIDAY,bike_train$REG_85)
table(bike_train$WEEKDAY,bike_train$REG_85)


## Question 4

qda_model=qda(REG_85~.-COUNT-REGISTERED-CASUAL-WEATHERSIT,data=bike_train)

## part a

qda_predict=predict(qda_model,newdata=bike_test)
qda_preds=qda_predict$posterior[,2]
qda_class=ifelse(qda_preds>0.45,1,0)
confuse_test=table(bike_test$REG_85,qda_class,dnn=c("Actual","Predicted"))
confuse_test
(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)

