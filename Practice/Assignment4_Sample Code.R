# Assignment 4 Sample Answer Code
# BUDT 758T, Professor Paulson
# Note: please see the sample answer key for complete answers;
#       this file contains only the necessary/helpful code.

#### Question 1

## a. Read the data set into R. I am NOT going to attach it!
## You may have loaded in your data using RStudio
## If not, you may have used:

movies_data <- read.csv("movies_data.csv")

## b.	Change the successful variable to a factor variable.

movies_data$successful=as.factor(movies_data$successful)

## c.	Set the seed in R to 3730.

set.seed(3730)

## d.	Randomly partition the data set in the following order:
##    i.	Split 25% of the observations in movies_data to use as testing data. Using these observations, create a testing data set called movies_testing.
##    ii.	Save the remaining 75% of the data as movies_rest.
##    iii.	Split 25% of the observations in movies_rest to use as validation data.  Using these observations, create a validation data set called movies_valid. 
##    iv.	Save the remaining data as movies_train.

num_obs = nrow(movies_data)
test_obs <- sample(num_obs,0.25*num_obs)
movies_test <- movies_data[test_obs,]
movies_rest <- movies_data[-test_obs,]

num_obs2 = nrow(movies_rest)
valid_obs <- sample(num_obs2,0.25*num_obs2)
movies_valid <- movies_rest[valid_obs,]
movies_train <- movies_rest[-valid_obs,]

## e.	How many observations do you now have in (1) the full data set
##    (2) the training data set, (3) the validation data set, and
##    (4) the testing data set?

nrow(movies_data)
nrow(movies_train)
nrow(movies_valid)
nrow(movies_test)

## Question 2: No R Code!

## Question 3

library(tree)

movies.tree=tree(successful~.-id-revenue-title,data=movies_train)
summary(movies.tree)

## part a
plot(movies.tree)
text(movies.tree,pretty=1)

## Question 4

## part a

summary(movies_valid$successful)

## part c

### Note that this is intentionally a very inefficient way to code this!
###   I've left it this way to show you every single step you need to
###   take for this problem, but this is definitely not an example of
###   good coding!

movies.prune2=prune.tree(movies.tree,best=2)
movies.prune3=prune.tree(movies.tree,best=3)
movies.prune4=prune.tree(movies.tree,best=4)
movies.prune5=prune.tree(movies.tree,best=5)
movies.prune6=prune.tree(movies.tree,best=6)

tree_preds2 <- predict(movies.prune2,newdata=movies_valid)[,2]
tree_preds3 <- predict(movies.prune3,newdata=movies_valid)[,2]
tree_preds4 <- predict(movies.prune4,newdata=movies_valid)[,2]
tree_preds5 <- predict(movies.prune5,newdata=movies_valid)[,2]
tree_preds6 <- predict(movies.prune6,newdata=movies_valid)[,2]
tree_preds7 <- predict(movies.tree,newdata=movies_valid)[,2]

tree_class2=ifelse(tree_preds2>0.5,1,0)
acc2=sum(ifelse(tree_class2==movies_valid$successful,1,0))/nrow(movies_valid)

tree_class3=ifelse(tree_preds3>0.5,1,0)
acc3=sum(ifelse(tree_class3==movies_valid$successful,1,0))/nrow(movies_valid)

tree_class4=ifelse(tree_preds4>0.5,1,0)
acc4=sum(ifelse(tree_class4==movies_valid$successful,1,0))/nrow(movies_valid)

tree_class5=ifelse(tree_preds5>0.5,1,0)
acc5=sum(ifelse(tree_class5==movies_valid$successful,1,0))/nrow(movies_valid)

tree_class6=ifelse(tree_preds6>0.5,1,0)
acc6=sum(ifelse(tree_class6==movies_valid$successful,1,0))/nrow(movies_valid)

tree_class7=ifelse(tree_preds7>0.5,1,0)
acc7=sum(ifelse(tree_class7==movies_valid$successful,1,0))/nrow(movies_valid)

print(c(acc2,acc3,acc4,acc5,acc6,acc7))
cbind(1:7,c(0.61,acc2,acc3,acc4,acc5,acc6,acc7))

## part d

movies.retrain=tree(successful~.-id-revenue-title,data=movies_rest)
movies.retrain6=prune.tree(movies.retrain,best=6)

plot(movies.retrain6)
text(movies.retrain6,pretty=1)

plot(movies.prune6)
text(movies.prune6,pretty=1)

## Question 5

library(class)

train.X=movies_train[,c(4,6,10:15)]
train.Y=as.numeric(movies_train$successful)-1

valid.X=movies_valid[,c(4,6,10:15)]
valid.Y=as.numeric(movies_valid$successful)-1

rest.X=movies_rest[,c(4,6,10:15)]
rest.Y=as.numeric(movies_rest$successful)-1

test.X=movies_test[,c(4,6,10:15)]
test.Y=as.numeric(movies_test$successful)-1

knn.pred1=knn(train.X,valid.X,train.Y,k=1)
knn.pred3=knn(train.X,valid.X,train.Y,k=3)
knn.pred5=knn(train.X,valid.X,train.Y,k=5)
knn.pred10=knn(train.X,valid.X,train.Y,k=10)
knn.pred25=knn(train.X,valid.X,train.Y,k=25)

knn.acc1=sum(ifelse(knn.pred1==movies_valid$successful,1,0))/nrow(movies_valid)
knn.acc3=sum(ifelse(knn.pred3==movies_valid$successful,1,0))/nrow(movies_valid)
knn.acc5=sum(ifelse(knn.pred5==movies_valid$successful,1,0))/nrow(movies_valid)
knn.acc10=sum(ifelse(knn.pred10==movies_valid$successful,1,0))/nrow(movies_valid)
knn.acc25=sum(ifelse(knn.pred25==movies_valid$successful,1,0))/nrow(movies_valid)

print(c(knn.acc1,knn.acc3,knn.acc5,knn.acc10,knn.acc25))
cbind(c(1,3,5,10,25),c(knn.acc1,knn.acc3,knn.acc5,knn.acc10,knn.acc25))

## Question 6

## part b

tree.pred.test=predict(movies.retrain6,newdata=movies_test)[,2]
tree.class.test=ifelse(tree.pred.test>0.5,1,0)
knn.pred.test=knn(rest.X,test.X,rest.Y,k=10)

tree.acc=sum(ifelse(tree.class.test==movies_test$successful,1,0))/nrow(movies_test)
knn.acc=sum(ifelse(knn.pred.test==movies_test$successful,1,0))/nrow(movies_test)

tree.acc
knn.acc
summary(movies_test$successful)
