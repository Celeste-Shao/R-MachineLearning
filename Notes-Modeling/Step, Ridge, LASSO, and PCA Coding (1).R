##BUDT 758T - Data Mining and Predictive Analytics
##Professor: Courtney Paulson
##Topic: Step, Ridge, LASSO, and PCA

## For this walkthrough, consider the movies data set we previously used to demonstrate
##    Trees and KNN:

movies <- read.csv("movies.csv")

## Remember from before that we want to add a new variable: veryprofitable, where a movie is
##    very profitable if it makes 3 times as much in revenue as its budget

movies$veryprofitable <- ifelse(movies$REVENUE > 3*movies$BUDGET,1,0)

## Let's start by partitioning the data
set.seed(12345)

## First, partition 30% of the data for testing data
## I'm also going to eliminate the first column of the data set, because I don't need the titles
num_obs=nrow(movies)
test_obs = sample(num_obs, 0.3*num_obs)
movies_test <- movies[test_obs,-1]

## Save the rest of the data as the data that isn't testing
## Normally you would do a validation split here, but we don't need it for
##  Ridge/LASSO. (Why?)
movies_rest <- movies[-test_obs,-1]


## Let's try using the step() function to do variable selection to find the best model
## Remember: what criteria is R using to find the "best" model?
## Create two models, one with all possible variables and one with no variables

## First, start by creating a full model (all variables we want to use) and a null model (no variables)
movies_all <- glm(veryprofitable~.-REVENUE, data=movies_rest, family="binomial")
movies_null <- glm(veryprofitable~1, data=movies_rest, family="binomial")

## The step function always goes step(starting model, ending model (if necessary), direction to search)
## The backward elimination model does not need an ending model, because it ends when there are no more variables
backward_model = step(movies_all, direction="backward")

## Take a look at the output. What is the step() function actually doing here?
## It prints out the steps it took to get the best model. It always compares the model when it adds/removes a single
##  variable, as well as comparing to a model where <none> (no variable) is added/removed.
##  My final model is just a regular regression model, though. I can use summary, predict, etc. just like before:
summary(backward_model)

## If we didn't want it to print all the steps it took, we could suppress it with the trace option:
backward_model = step(movies_all, direction="backward", trace=0)

## For forward selection, we do need to specify an ending model (so it has a final model to check):
forward_model = step(movies_null, scope=list(upper=movies_all), direction="forward", trace=0)
summary(forward_model)

## Remember: there are three direction types, "forward", "backward", and "both" (for stepwise procedures)
## If I don't specify a direction, it will do backward elimination
## If I use "both", it will start with whatever model I give it first

forward_model_both = step(movies_null, scope=list(upper=movies_all), direction="both", trace=0)
summary(forward_model_both)


## Now let's try Ridge and LASSO using the glmnet package

library(glmnet)

## The glmnet function has a parameter called "alpha" that distinguishes what type of method to run.
## If alpha = 0, you do Ridge Regression
## If alpha = 1, you do LASSO Regression
## The glmnet function always goes glmnet(matrix of X data, vector of Y data, alpha value)

## The first advantage with Ridge: you are not required to give a grid of lambda values
## The second advantage: predictions are much easier!

## One thing to note: we do not use training/validation data here. We use the combined "rest" data set
## Why?

## Let's try running a ridge (logistic) regression with our X data (which is the first 8 columns of movies_rest)
glmnet_ridge = glmnet(as.matrix(movies_rest[,c(1:8)]),movies_rest$veryprofitable, family="binomial", alpha=0)

## Error! Why?
## glmnet expects numeric inputs, just like KNN

## Let's change IMDB_GOOD and IMDB_BAD to numeric:
movies_rest$IMDB_BAD = as.numeric(movies_rest$IMDB_BAD) - 1
movies_rest$IMDB_GOOD = as.numeric(movies_rest$IMDB_GOOD) - 1
summary(movies_rest)
colnames(movies_rest[,c(1,2,4,7,8)])
glmnet_ridge = glmnet(as.matrix(movies_rest[,c(1,2,4,7,8)]),movies_rest$veryprofitable, family="binomial", alpha=0)

## You can also do cross-validation automatically using the cv.glmnet function
## It works the same way as regular glmnet, but it runs a full cross-validation procedure for you
glmnet_ridge.cv=cv.glmnet(as.matrix(movies_rest[,c(1,2,4,7,8)]),movies_rest$veryprofitable, family="binomial", alpha=0)

## We can plot the results of the cross-validation procedure and even find the lambda where the minimum error occurs
plot(glmnet_ridge.cv)
best.lambda=glmnet_ridge.cv$lambda.min
best.lambda

## Nice side effect of glmnet: we can use the predict() function just like usual
## We have to give it a best lambda value, however, so it knows which of the CV models to use
## Let's do it here with the movies_rest data set, to see how well it predicts on the complete training/validation data
ridge.probs = predict(glmnet_ridge,s=best.lambda,newx=as.matrix(movies_rest[,c(1,2,4,7,8)]),type="response")
ridge.class = ifelse(ridge.probs>0.5,1,0)

## Now let's calculate accuracy:
sum(ifelse(ridge.class==movies_rest$veryprofitable,1,0))/nrow(movies_rest)


## LASSO works the same way as Ridge, just with alpha=1:

glmnet_lasso = glmnet(as.matrix(movies_rest[,c(1,2,4,7,8)]),movies_rest$veryprofitable,family="binomial",alpha=1)
glmnet_lasso.cv=cv.glmnet(as.matrix(movies_rest[,c(1,2,4,7,8)]),movies_rest$veryprofitable,family="binomial",alpha=1)
plot(glmnet_lasso.cv)
best.lambda2=glmnet_lasso.cv$lambda.min
best.lambda2

lasso.probs = predict(glmnet_lasso,s=best.lambda2,newx=as.matrix(movies_rest[,c(1,2,4,7,8)]),type="response")
lasso.class = ifelse(lasso.probs>0.5,1,0)
sum(ifelse(lasso.class==movies_rest$veryprofitable,1,0))/nrow(movies_rest)

## Let's say we want to see what these models actually look like
## We can actually use the predict function to print out the coefficients here:
predict(glmnet_ridge,s=best.lambda,type="coefficients")
predict(glmnet_lasso,s=best.lambda2,type="coefficients")


## One last thing we haven't seen yet: what if we wanted to include the categorical (non-numeric) variables?
## In both KNN and glmnet, we would need to create dummy variables for all categories
## That's a pain. No one wants to do that by hand
## Luckily, R has an option for you: model.matrix

## model.matrix is designed to build a complete model from a matrix, but usually we have better model options
## Instead, we can use it to just print out the matrix it would use to run a model
## That means it would calculate the dummy variables for you, just like R does automatically in a regression
## Let's use model.matrix to make all our X data (columns 1 through 8) numeric:

movies_X <- model.matrix( ~ .-1, movies[,c(1:8)])
movies_rest_X <- model.matrix( ~ .-1, movies_rest[,c(1:8)])
movies_test_X <- model.matrix( ~ .-1, movies_test[,c(1:8)])

## Note that you would normally do this before you partition the data, if you have to do it
## Why do you need to do this before you partition?


## You only have to do it when you have non-numeric data, though! If your data is numeric, this step is unnecessary

glmnet_ridge2=glmnet(movies_rest_X,movies_rest$veryprofitable,family="binomial",alpha=0)
glmnet_ridge.cv2=cv.glmnet(movies_rest_X,movies_rest$veryprofitable,family="binomial",alpha=0)
plot(glmnet_ridge.cv2)
best.lambda.new=glmnet_ridge.cv2$lambda.min
best.lambda.new

glmnet_lasso2=glmnet(movies_rest_X,movies_rest$veryprofitable,family="binomial",alpha=1)
glmnet_lasso.cv2=cv.glmnet(movies_rest_X,movies_rest$veryprofitable,family="binomial",alpha=1)
plot(glmnet_lasso.cv2)
best.lambda.new2=glmnet_lasso.cv2$lambda.min
best.lambda.new2


## Your last step in any analysis would be to take your models and apply them to your testing data set
## By calculating accuracy on the testing data, you can see which model appears best in practice
## Let's calculate testing accuracy on our models here
## We'll use:
##    (a) backward elimination model
##    (b) forward selection model
##    (c) Ridge regression with all variables and best ridge lambda
##    (d) LASSO regression with all variables and best lasso lambda

back_probs = predict(backward_model,newdata = movies_test,type="response")
back_class = ifelse(back_probs>0.5,1,0)
sum(ifelse(back_class==movies_test$veryprofitable,1,0))/nrow(movies_test)

forward_probs = predict(forward_model,newdata = movies_test,type="response")
forward_class = ifelse(forward_probs>0.5,1,0)
sum(ifelse(forward_class==movies_test$veryprofitable,1,0))/nrow(movies_test)

ridge_probs = predict(glmnet_ridge2,s=best.lambda.new,newx=movies_test_X,type="response")
ridge_class = ifelse(ridge_probs>0.5,1,0)
sum(ifelse(ridge_class==movies_test$veryprofitable,1,0))/nrow(movies_test)

lasso_probs = predict(glmnet_lasso2,s=best.lambda.new2,newx=movies_test_X,type="response")
lasso_class = ifelse(lasso_probs>0.5,1,0)
sum(ifelse(lasso_class==movies_test$veryprofitable,1,0))/nrow(movies_test)


## So which model is best here?

## One last thing: what if we wanted to try doing principal components regression?
## Let's use the original numeric variables we did with Lasso/Ridge to make it easier:

pc_rest = prcomp(movies_rest[,c(1,2,4,7,8)])
summary(pc_rest)

## Note that we're getting 100% of the variance (almost; this is rounding) in just the first PC
## So let's extract the first PC, since pc_rest$x gives us PCs for each observation in our data set:

head(pc_rest$x)
PC1 = pc_rest$x[,1]

## Let's compare a linear model to predict REVENUE with a PCR model to predict REVENUE
## We'll use the full five variables (with the 9th variable, REVENUE) for the LM
## And only the first PC for PCR

lm_model=lm(REVENUE~., data=movies_rest[,c(1,2,4,7,8,9)])
pc_model=lm(movies_rest$REVENUE~PC1)

summary(lm_model)
summary(pc_model)

## Almost the same overall statistics for the model, but with only one variable
## Note the slight differences exist because we have not gotten exactly 100% information
##    But we have gotten extremely close
