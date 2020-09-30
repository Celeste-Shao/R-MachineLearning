## BUDT 758T - Data Mining and Predictive Analytics
## Decision Trees and kNN (with optional performance curves walkthrough)
## Professor: Courtney Paulson
## Spring 2019

## Consider a new data set containing recent movie information
movies <- read.csv("movies.csv")
View(movies)

## Let's say we want to add a new variable: veryprofitable, where a movie is
##    very profitable if it makes 3 times as much in revenue as its budget

movies$veryprofitable <- ifelse(movies$REVENUE > 3*movies$BUDGET,1,0)

## We could have created this variable as a factor, but since we did a numeric
##    variable, we need to change it to a factor in order to get a classification
##    tree (since numeric Y variables will read as regression trees)

movies$veryprofitable <- as.factor(movies$veryprofitable)

##Split the data into training, validation, and test data
##remember: random seed controls the random number generator
set.seed(12345)

## First, partition 30% of the data for testing data
test_instn = sample(nrow(movies), 0.3*nrow(movies))
movies_test <- movies[test_instn,]

## Save the rest of the data as the data that isn't testing
movies_rest <- movies[-test_instn,]

## Partition 30% of the remaining data as validation data
valid_instn = sample(nrow(movies_rest), 0.30*nrow(movies_rest))
movies_valid <- movies_rest[valid_instn,]
movies_train <- movies_rest[-valid_instn,]

## Use the tree library
library(tree)

## Train a classification tree
movies.tree=tree(veryprofitable~RUNTIME+BUDGET+YEAR+IMDB_GOOD+MONTH,movies_train)
summary(movies.tree)
plot(movies.tree)
text(movies.tree)

## This is not an easy-to-read, nice-looking tree! Try to make it look nicer
plot(movies.tree)
text(movies.tree,pretty=1)

## Still not easy to read! This tree is probably too complicated
## Let's prune it back to have only 5 terminal nodes
movies.pruned=prune.tree(movies.tree,best=5)
summary(movies.pruned)
plot(movies.pruned)
text(movies.pruned,pretty=1)

## Let's use the tree to do probability predictions for the validation data
tree_preds <- predict(movies.pruned,newdata=movies_valid)

## Look at the first ten predictions
tree_preds[1:10,]

## We only want the Y=1 probability predictions
tree_probs=tree_preds[,2]

## Let's do our usual classification on the probabilities
##    and check a confusion matrix:

tree_class=ifelse(tree_probs>0.5,1,0)
table(movies_valid$veryprofitable,tree_class,dnn=c("Actual","Predicted"))
acc_tree=sum(ifelse(tree_class==movies_valid$veryprofitable,1,0))/nrow(movies_valid)
acc_tree


## For kNN, use the class library
library(class)

## kNN doesn't use a model, so make data frames with all the X variables you want to use
## Use colnames(movies) to find which columns you need
colnames(movies)

## Our tree variables were the 2,3,4,5, and 8 columns
train.X=movies_train[,c(2:5,8)]
valid.X=movies_valid[,c(2:5,8)]
test.X=movies_test[,c(2:5,8)]

train.profit=movies_train$veryprofitable
valid.profit=movies_valid$veryprofitable
test.profit=movies_test$veryprofitable

## Use 1 nearest neighbor to predict validation values
## Use training X, validation X, and training Y with k value
knn.pred=knn(train.X,valid.X,train.profit,k=1)

## Doesn't work! kNN uses distance, so we need to use numeric variables
train.X=movies_train[,c(2,3,5)]
valid.X=movies_valid[,c(2,3,5)]
test.X=movies_test[,c(2,3,5)]

train.profit=as.numeric(movies_train$veryprofitable)
valid.profit=as.numeric(movies_valid$veryprofitable)
test.profit=as.numeric(movies_test$veryprofitable)

## Try it again
knn.pred=knn(train.X,valid.X,train.profit,k=1)

## How do our predictions do?
table(valid.profit,knn.pred)

## Note: when converting back, it will change 0 and 1 to 1 and 2!
## You can convert back to original values by subtracting by 1
## But you don't have to do this for results to hold

## Try it:
knn.pred=knn(train.X,valid.X,train.profit-1,k=1)
table(valid.profit-1,knn.pred,dnn=c("Actual","Predicted"))

## Same table!

## Note there is always one last step in a data analysis:
##    Compare models using the testing data!
##    I will leave this as an exercise for you, since it 
##    is the same as the code above



## Here is some optional additional code that can make doing performance measures
##    much easier! It uses the ROCR package, and it fits across all cutoffs to help
##    you choose the best without having to do it manually.
##    This is not in Assignment 4, but it may be interesting to you for future reference.

library(ROCR)

## Fit an ROCR prediction object with the tree predictions and the true validation
## data predictions. This calculates all performance measures at all cutoffs for you--very useful!
## Here, we will use the tree_probs that we fit from our tree above:

pred <- prediction(tree_probs,movies_valid$veryprofitable)

## This essentially creates a confusion matrix for every possible cutoff value

## Let's compare TPR and TNR
## Note: you can use ?performance to see all measures for evaluation
## For example, measure = "acc" is for Accuracy calculations
## You can calculate more than one at a time, but if you do it this way,
##  your Y value is the performance measure (e.g. TPR) and your X value is
##  cutoff. This is nice for making plots, like the ROC curve or spider plot

tpr.perf = performance(pred, measure = "tpr")
tnr.perf = performance(pred, measure = "tnr")

## Plot TPR values on y-axis between 0 and 1, then add TNR
plot(tpr.perf,ylim=c(0,1))
plot(tnr.perf, add=T)

## We can also find specific values of our performance measures
## What's our best value for TPR?
## Find the value that maximizes our tpr.perf
## This is much more advanced code, involving slots and lists

## First, find the slot in the list which maximizes TPR
best = which.max(slot(tpr.perf,"y.values")[[1]])

## What is the max TPR at that slot?
max.tpr = slot(tpr.perf,"y.values")[[1]][best]

## What cutoff value is associated with that slot?
max.cutoff = slot(tpr.perf,"x.values")[[1]][best]

## Check the TPR value and the cutoff it occurred at
print(c(TPR = max.tpr, cutoff = max.cutoff))

## We can use this ROCR package to do ROC and lift curves as well
## Use abline() to do a baseline
roc.valid = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.valid,colorize=T)
abline(a=0,b=1,lty=3)


