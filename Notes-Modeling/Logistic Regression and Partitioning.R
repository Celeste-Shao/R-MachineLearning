## BUDT 758T - Data Mining and Predictive Analytics
## Logistic Regression, Data Partition, Prediction, Functions, and Tables
## Instructor: Courtney Paulson
## Spring 2019

## First, read in (or import) our new friend, the beer data set
Beer <- read.csv("Beer.csv")

## Remember that we want to predict if someone is a Light beer drinker or a Regular beer drinker
## R can do dummy variables for us automatically, but it will also then choose the baseline for us automatically
## In this case, because it goes alphabetically, it will pick "Light = 0" and "Regular = 1"
## Since we want Light = 1, let's create a new variable and add it to the data:

Beer$Pref <- ifelse(Beer$Preference=="Light",1,0)

## Note that we have to add it to our data because we are going to partition our data.
## If we don't add it directly to the data set, it won't get partitioned when we split the data!

## So, now let's partition our data.
## That means we want to split our data into training and test data observations
## We want to do this randomly; in R, random seed controls the random number generator
## If we set a seed, we can get the same results any time we run the code. Let's say I pick the seed 12345.
## Then, every time I use the seed value 12345, I will get the same data split

set.seed(12345)

## First, how many observations do we have in the data set?

num_obs = nrow(Beer)

## Let's make 70% of those training observations
## First, we sample 70% of the observations (the rows) in the training data

train_obs <- sample(num_obs,0.7*num_obs)

## Then we assign the random sample to a new data set, Beer_train

Beer_train <- Beer[train_obs,]

## That maeans the remaining rows should go to another new data set, Beer_test

Beer_test <- Beer[-train_obs,]

## Let's do some preliminary data analysis using the training data, since that's what we'll use to build a model
## What if we want boxplots of numerical variables, split out by the Pref variable?

boxplot(Beer_train$Income~Beer_train$Pref)
boxplot(Beer_train$Income~Beer_train$Pref,xlab="Beer Preference",ylab="Household Income")

## We can also look at categorical variables vs. Pref using table

table(Beer_train$Gender,Beer_train$Pref)

## This looks slightly confusing, since both are 0 and 1. Let's label our variables:

table(Beer_train$Gender,Beer_train$Pref,dnn=c("Gender","Preference"))

## Note we can also switch what appears as rows vs. columns (since first is always rows to R):

table(Beer_train$Pref,Beer_train$Gender,dnn=c("Preference","Gender"))

## This works just like a matrix.
## Let's say we want to know what percentage of Preference come from each gender.
## I'll use the full Preference variable rather than Pref to make it easier to follow:
gender_table <- table(Beer_train$Gender,Beer_train$Preference)
gender_table
gender_table[,2]/sum(gender_table[,2])

##Alternately, if we didn't want to use the column number, we could specify the value of Preference:
gender_table[,"Regular"]/sum(gender_table[,"Regular"])

##Or we could look at it from the perspective of a particular gender instead:
gender_table["1",]

## Review: train a linear regression, this time to predict Pref
## Let's use glm() this time instead of lm()
fit1 <- glm(Pref~Gender+Married+Age+Income,data=Beer_train)
summary(fit1)

## To make predictions, use the predict() function on both the training and test sets
lin_preds_train <- predict(fit1,newdata=Beer_train)
lin_preds_test <- predict(fit1,newdata=Beer_test)

## Note if you looked through the Assignment 1 sample code, we can also
## use predict() for individual new data points as well:

new_beer=data.frame(Gender=0,Married=0,Income=50000,Age=50)
predict(fit1,newdata=new_beer)

## But what if my new data has wrong information?

new_beer2=data.frame(Gender="Male",Married=0,Income=50000,Age=50)
predict(fit1,newdata=new_beer2)

## Now let's calculate root mean squared error (RMSE) on the training and test data:

RMSE_train = sqrt(mean((lin_preds_train-Beer_train$Pref)^2))
RMSE_test = sqrt(mean((lin_preds_test-Beer_test$Pref)^2))

## Which data set had better predictions?

RMSE_train
RMSE_test

## One cool feature of R: you can write your own functions to make repeated calculations easier
## Let's say I know I want to calculate RMSE several times
## I can make a function (that I've named "RMSE_calc") to evaluate this for me
## measure_perf takes in a vector of predicted values ("predicted") and a vector of actual values ("actual")
## and returns the calculated root mean squared error ("RMSE"):
RMSE_calc <- function(predicted,actual){
  RMSE = sqrt(mean((predicted-actual)^2))
  return(RMSE)
}

## Let's compute RMSE on the training and test sets again
RMSE_calc(lin_preds_train, Beer_train$Pref)
RMSE_calc(lin_preds_test, Beer_test$Pref)

## Same result, but easier if I'm doing several at once!

## Now we can actually classify the observations (using a cutoff of 0.2)
class_train <- ifelse(lin_preds_train>0.2,1,0)
class_test <- ifelse(lin_preds_test>0.2,1,0)

## This gives us our confusion matrix
table(Beer_test$Pref, class_test, dnn=c("Actual","Predicted"))

## How many did we get right?
(6+18)/(6+5+1+18)

## This is the number of correct classifications/the total number of observations
## So, more generally:
sum(ifelse(Beer_test$Pref==class_test,1,0))/nrow(Beer_test)

## This is how you train a logistic regression model
fit2 <- glm(Pref~Gender+Married+Age+Income,data=Beer_train,family="binomial")
summary(fit2)

## Note the slight change to the predict function here
## "respose" tells R that we want probability predictions
## If we don't say type="response", we will get logit values (think about why!)

log_preds <- predict(fit2,newdata=Beer_test,type="response")
log_preds[1:10]

## Do the logistic classification
log_class <- ifelse(log_preds>0.2,1,0)

## Make a confusion matrix
table(Beer_test$Pref,log_class,dnn=c("Actual","Predicted"))

## Did we do better or worse than before?

