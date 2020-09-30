# Assignment 5 Sample Answer Code
# BUDT 758T, Professor Paulson
# Note: please see the sample answer key for complete answers;
#       this file contains only the necessary/helpful code.

#### Question 1

## a. Read the data set into R. I am NOT going to attach it!
## You may have loaded in your data using RStudio
## If not, you may have used:

movies_data <- read.csv("movies_data.csv")

## b. Change the month variable to a factor variable.

movies_data$month = as.factor(movies_data$month)

## c.	Set the seed in R to 3730.

set.seed(6022)

## d.	Randomly partition the data set in the following order:
##    i.	Split 30% of the observations in movies_data to use as testing data. Using these observations, create a testing data set called movies_testing.
##    ii.	Save the remaining 70% of the data as movies_rest.
##    iii.	Split 20% of the observations in movies_rest to use as validation data.  Using these observations, create a validation data set called movies_valid. 
##    iv.	Save the remaining data as movies_train.

num_obs = nrow(movies_data)
test_obs <- sample(num_obs,0.30*num_obs)
movies_test <- movies_data[test_obs,]
movies_rest <- movies_data[-test_obs,]

num_obs2 = nrow(movies_rest)
valid_obs <- sample(num_obs2,0.20*num_obs2)
movies_valid <- movies_rest[valid_obs,]
movies_train <- movies_rest[-valid_obs,]


## Question 2

## First, set up your full and null models:

movies_all <- glm(revenue~.-id-title-successful, data=movies_train)
movies_null <- glm(revenue~1, data=movies_train)

backward_model = step(movies_all, direction="backward")
forward_model = step(movies_null, scope=list(upper=movies_all), direction="forward")


## part b

summary(backward_model)
summary(forward_model)

## part c

back_preds = predict(backward_model,newdata=movies_valid)
rmse_back = sqrt(mean((movies_valid$revenue-back_preds)^2))

forward_preds = predict(forward_model,newdata=movies_valid)
rmse_forward = sqrt(mean((movies_valid$revenue-forward_preds)^2))

# Let's use backward elimination, for example:
rmse_back


## Question 3

movies_all2 <- glm(successful~.-id-title-revenue, data=movies_train, family="binomial")
movies_null2 <- glm(successful~1, data=movies_train, family="binomial")

backward_model_both2 = step(movies_all2, direction="both")
forward_model_both2 = step(movies_null2, scope=list(upper=movies_all2), direction="both")

## part b

cutoffs=c(0.2,0.4,0.5,0.6,0.8)

back_both_preds2 = predict(backward_model_both2,newdata=movies_valid,type="response")
forward_both_preds2 = predict(forward_model_both2,newdata=movies_valid,type="response")

back_both_acc=rep(0,5)
forward_both_acc=rep(0,5)

for(i in 1:5){
  
  back_class_both = ifelse(back_both_preds2>cutoffs[i],1,0)
  back_both_acc[i] = sum(back_class_both==movies_valid$successful)/nrow(movies_valid)
  forward_class_both = ifelse(forward_both_preds2>cutoffs[i],1,0)
  forward_both_acc[i] = sum(forward_class_both==movies_valid$successful)/nrow(movies_valid)
  
}

back_both_acc
forward_both_acc

## Question 5

library(glmnet)

## part b

movies_rest_X <- model.matrix( ~ .-1, movies_rest[,c(3,4,6:15)])
movies_test_X <- model.matrix( ~ .-1, movies_test[,c(3,4,6:15)])

## part c

glmnet_ridge=glmnet(movies_rest_X,movies_rest$successful,family="binomial",alpha=0)
glmnet_ridge.cv=cv.glmnet(movies_rest_X,movies_rest$successful,family="binomial",alpha=0)
plot(glmnet_ridge.cv)
best.lambda.ridge=glmnet_ridge.cv$lambda.min

glmnet_lasso=glmnet(movies_rest_X,movies_rest$successful,family="binomial",alpha=1)
glmnet_lasso.cv=cv.glmnet(movies_rest_X,movies_rest$successful,family="binomial",alpha=1)
plot(glmnet_lasso.cv)
best.lambda.lasso=glmnet_lasso.cv$lambda.min

best.lambda.ridge
best.lambda.lasso

## part d

predict(glmnet_ridge,s=best.lambda.ridge,type="coefficients")
predict(glmnet_lasso,s=best.lambda.lasso,type="coefficients")

## Question 6

acc_baseline = length(which(movies_test$successful==0))/nrow(movies_test)

## Retrain the logistic model with the same variables
stepwise_retrain = glm(successful ~ vote_count + budget + popularity + genre + vote_average + 
                         year + month + united_states + english + runtime,
                          data = movies_rest, family = "binomial")
back_probs = predict(stepwise_retrain,newdata = movies_test,type="response")
back_class = ifelse(back_probs>0.5,1,0)
acc_step = sum(ifelse(back_class==movies_test$successful,1,0))/nrow(movies_test)

ridge_probs = predict(glmnet_ridge,s=best.lambda.ridge,newx=movies_test_X,type="response")
ridge_class = ifelse(ridge_probs>0.5,1,0)
acc_ridge = sum(ifelse(ridge_class==movies_test$successful,1,0))/nrow(movies_test)

lasso_probs = predict(glmnet_lasso,s=best.lambda.lasso,newx=movies_test_X,type="response")
lasso_class = ifelse(lasso_probs>0.5,1,0)
acc_lasso = sum(ifelse(lasso_class==movies_test$successful,1,0))/nrow(movies_test)

acc_baseline
acc_step
acc_ridge
acc_lasso
