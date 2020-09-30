## BUDT 758T - Data Mining and Predictive Analytics
## Ensemble Methods
## Instructor: Courtney Paulson
## Spring 2019

## For continuity, let's consider using our example data set
##    containing recent movie information

movies <- read.csv("movies.csv")
View(movies)

## Remember from previously, we add a new variable: veryprofitable,
##    where a movie is very profitable if it makes 3 times as much
##    in revenue as its budget. Let's make this a factor from the start
##    This will mean we are doing classification trees

movies$veryprofitable <- ifelse(movies$REVENUE > 3*movies$BUDGET,1,0)
movies$veryprofitable <- as.factor(movies$veryprofitable)

##Split the data into training, validation, and test data
##remember: random seed controls the random number generator
set.seed(1234)

## First, partition 30% of the data for testing data
num_obs=nrow(movies)
test_obs = sample(num_obs, 0.3*num_obs)
movies_test <- movies[test_obs,]

## Save the rest of the data as the data that isn't testing
movies_rest <- movies[-test_obs,]

## Again, like LASSO and Ridge we don't have validation data here.
## Why not?

## Let's try some trees!
library(tree)

## Bagging

## Here's how you would do a single bootstrap sample
## Take a sample *of the same size* as your data set
## Sample with replacement so some rows are repeated
num_rest=nrow(movies_rest)
bootstrap_sample=sample(seq(1,num_rest),num_rest,replace=T)

## Create the tree like you normally would (like we did in the Trees and KNN code)
##  Leave out REVENUE (since we used that to create veryprofitable with BUDGET and
##  can only include one of them) and PROD_COMPANY, since it has too many levels
##  Note we also leave out TITLE here by doing "-1" in the data call

bag.tree=tree(veryprofitable~.-REVENUE-PROD_COMPANY,data=movies_rest[bootstrap_sample,-1])

## You've seen these before!
summary(bag.tree)
plot(bag.tree)
text(bag.tree,pretty=1)

## It's a pretty messy tree, but that's good for bagging! We don't do validation
##    or cross-validation. We let them grow as much as they like

## What if we want to do a complete bagging procedure, not just one tree?
## Let's try 100 trees
B=100

## We want to save all our trees, so the best way to do that in R is to create a vector
## Then each element in the vector will be a list (a complete model)
## We want to create a vector of 100 trees:
bagged.trees=vector("list", B)

## One way to do a number of repeated samples/models is to do a for loop
## This will index by i. As i increases from 1 to B=100, it will run the code in the loop
## So this code will run 100 times, and each time it will save a tree in a separate list in the vector
## If i=1, this will be the first slot in the vector; if i=2, the second; etc.
for(i in 1:B){
  
  bootstrap_sample=sample(seq(1,num_rest),num_rest,replace=T)
  bagged.trees[[i]]=tree(veryprofitable~.-REVENUE-PROD_COMPANY,data=movies_rest[bootstrap_sample,-1])
  
}

## Now we can do just what we did before for any of the 100 individual trees:
summary(bagged.trees[[1]])
plot(bagged.trees[[1]])
text(bagged.trees[[1]],pretty=1)

summary(bagged.trees[[25]])
plot(bagged.trees[[25]])
text(bagged.trees[[25]],pretty=1)

## How does an individual tree do on prediction for the test data?
bag_preds=predict(bagged.trees[[1]],newdata=movies_test)
bag_probs=bag_preds[,2]
bag_class=ifelse(bag_probs>0.5,1,0)

table(movies_test$veryprofitable,bag_class)
sum(ifelse(bag_class==movies_test$veryprofitable,1,0))/nrow(movies_test)

## That's useful code if you had to do it yourself, but usually we prefer the randomForest library
library(randomForest)

## How many variables do we have in our data?
ncol(movies_rest)

## So bagging is the same as running a RF with m=10 variables (since veryprofitable is 1 variable)
##    if we used all the variables. Above, we left out REVENUE, PROD_COMPANY,
##    and TITLE, so we really have m=7 here for bagging
## We want to look at variable importance plots, so keep importance=TRUE

bag.trees=randomForest(veryprofitable~.-REVENUE-PROD_COMPANY,data=movies_rest[,-1],ntree=100,mtry=7,importance=TRUE)

bag.trees
importance(bag.trees)
varImpPlot(bag.trees)

## This ran 100 trees. If you leave out ntree, the default is 500 trees
## If you leave out mtry, the default for a classification tree is sqrt(p) (in this case sqrt(9)=3)
## Now we have a random forest!

rf.trees=randomForest(veryprofitable~.-REVENUE-PROD_COMPANY,data=movies_rest[,-1],importance=TRUE)

## Let's predict for the first value in movies_test: movies_test[1,]
## We can do a general classification with cutoff=0.5:
predict(rf.trees,newdata=movies_test[1,])

## But what if we want to calculate probabilities and do a cutoff ourselves?
predict(rf.trees,newdata=movies_test[1,],type="prob")

## Then we get back to something that looked similar to what we had for trees before (see above)
rf_preds=predict(rf.trees,newdata=movies_test,type="prob")
rf_probs=rf_preds[,2]
rf_class=ifelse(rf_probs>0.5,1,0)

table(movies_test$veryprofitable,rf_class)
sum(ifelse(rf_class==movies_test$veryprofitable,1,0))/nrow(movies_test)

## So is our random forest an improvement? Does it even beat the baseline?

## How did the full bagging do?
bagging_preds=predict(bag.trees,newdata=movies_test,type="prob")
bagging_probs=bagging_preds[,2]
bagging_class=ifelse(bagging_probs>0.5,1,0)

table(movies_test$veryprofitable,bagging_class)
sum(ifelse(bagging_class==movies_test$veryprofitable,1,0))/nrow(movies_test)


## How about boosting?
library(gbm)

## The one downside to gbm in R: it requires numeric variables
## So, let's turn our factor into a 0 and 1 numeric variable and tell it to use a binomial ("bernoulli") distribution
##    with numeric variables from our data set
boost.movies=gbm(as.numeric(veryprofitable)-1~RUNTIME+YEAR+BUDGET+as.numeric(IMDB_GOOD)
                 +as.numeric(IMDB_BAD),data=movies_rest,distribution="bernoulli",
                 n.trees=500)
summary(boost.movies)

## Same procedure as before
predict(boost.movies,newdata=movies_test[1,],n.trees=500,type="response")
boosting_probs=predict(boost.movies,newdata=movies_test,n.trees=500,type="response")

boosting_class=ifelse(boosting_probs>0.5,1,0)

table(movies_test$veryprofitable,boosting_class)
sum(ifelse(boosting_class==movies_test$veryprofitable,1,0))/nrow(movies_test)

## So, are we getting massive increases in accuracy using the ensemble methods
##    here? Remember, we aren't guaranteed to do better with ensemble methods!

