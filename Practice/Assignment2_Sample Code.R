# Assignment 2 Sample Answer Code
# BUDT 758T, Professor Paulson
# Note: please see the sample answer key for complete answers;
#       this file contains only the necessary/helpful code.

#### Question 1

## a. Load the Airlines data into R. I am NOT going to attach it!
## You may have loaded in your data using RStudio
## If not, you may have used:

airlines <- read.csv("Airlines_Data.csv")

## b.	Change the cc3_miles variable to a factor variable using as.factor() in R.

airlines$cc1_miles=as.factor(airlines$cc1_miles)
airlines$cc2_miles=as.factor(airlines$cc2_miles)
airlines$cc3_miles=as.factor(airlines$cc3_miles)

## c.	Set the seed in R to 14632.

set.seed(14632)

## d.	Randomly partition the data set into a training data set and a test data set.
## Use 65% of the data as training data and hold out the remaining 35% as test data.

num_obs = nrow(airlines)
train_obs <- sample(num_obs,0.65*num_obs)
airlines_train <- airlines[train_obs,]
airlines_test <- airlines[-train_obs,]


## Question 2

## Part (b)

boxplot(airlines_train$Bonus_trans~airlines_train$Award,xlab="Travel Award Claimed",ylab="Bonus Transactions")

## Part (c)

miles_table=table(airlines_train$cc3_miles,airlines_train$Award)
miles_table

## If you also created the testing data table:

miles_table=table(airlines_test$cc3_miles,airlines_test$Award)
miles_table


## Question 3

## part (a)

lin_model=glm(Award~Balance+Qual_miles+cc1_miles+Bonus_miles+Bonus_trans+Flight_miles_12mo+Flight_trans_12
              +Days_since_enroll,data=airlines_train)
summary(lin_model)
train_pred=predict(lin_model,newdata=airlines_train)
test_pred=predict(lin_model,newdata=airlines_test)
rmse_train=sqrt(mean((train_pred-airlines_train$Award)^2))
rmse_test=sqrt(mean((test_pred-airlines_test$Award)^2))
rmse_train
rmse_test

## part (c)

rmse_base=sqrt(mean((mean(airlines_test$Award)-airlines_test$Award)^2))
rmse_base

## Alternate, if you used the common class baseline:

rmse_base=sqrt(mean((0-airlines_test$Award)^2))
rmse_base


## part (d)

train_class=ifelse(train_pred>0.5,1,0)
test_class=ifelse(test_pred>0.5,1,0)
confuse_train=table(airlines_train$Award,train_class,dnn=c("Actual","Predicted"))
confuse_test=table(airlines_test$Award,test_class,dnn=c("Actual","Predicted"))
confuse_train
confuse_test

## part (e)

## Accuracy from the table (if you didn't calculate by hand):

(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)

## There are many different ways to determine the baseline accuracy using the most common class
## It's just the percentage of the most common class in the test data set
## For example, since 0 is the most common class:

sum(airlines_test$Award==0)/length(airlines_test$Award)

## Note that you could also get this from the confusion matrix. Just add the values for actual = 0 and divide by the total

## part (f)

new_cust=data.frame(Balance=10000, Qual_miles=20000, cc1_miles="1", Bonus_miles=20000, Bonus_trans=50, 
                    Flight_miles_12mo=25000, Flight_trans_12=25, Days_since_enroll=1000)
predict(lin_model,newdata = new_cust)

# Alternate:
predict(lin_model,newdata=data.frame(Balance=10000, Qual_miles=20000, cc1_miles="1", Bonus_miles=20000, Bonus_trans=50, 
                                     Flight_miles_12mo=25000, Flight_trans_12=25, Days_since_enroll=1000))



## Question 4

## part (a)

log_model=glm(Award~Balance+Qual_miles+cc1_miles+Bonus_miles+Bonus_trans+Flight_miles_12mo+Flight_trans_12+Days_since_enroll,
              data=airlines_train,family = "binomial")

## part (b)

summary(log_model)

## part (c)

log_preds=predict(log_model,newdata=airlines_test,type="response")
log_class=ifelse(log_preds>0.5,1,0)
table(airlines_test$Award,log_class,dnn=c("Actual","Predicted"))

## part (e)

## Using our new_cust data frame that we created above:

predict(log_model,newdata=new_cust,type="response")

