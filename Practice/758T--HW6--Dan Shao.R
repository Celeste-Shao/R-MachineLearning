# 0. Data Preparation:
# Read file
college <- read.csv('/Users/charmain/Desktop/Master Degree/Term 2/Data Mining & Predictive/Assignment/Assignment data/College.csv')
View(college)
# Set seed
set.seed(91101)

# Partition 35% of the data for testing data
num_obs=nrow(college)
test_obs = sample(num_obs, 0.35*num_obs)
college_test <- college[test_obs,]

## Save the rest of the data
college_rest <- college[-test_obs,]

# 1. PCA
# a.
pca = prcomp(college_rest[,c(3:18)])
summary(pca)
# b.
plot(pca,main = 'Plot of PCA')
# c.
PC = pca$x[,c(1,2,3)]

lm_model=lm(Grad.Rate~., data=college_rest[,c(3:19)])
pc_model=lm(college_rest$Grad.Rate~PC)

summary(lm_model)
summary(pc_model)

# 2. Bootstrap: single tree
library(tree)
# Bootstrap sample
num_rest=nrow(college_rest)
bootstrap_single=sample(seq(1,num_rest),num_rest,replace=T)

bag.tree=tree(Grad.Rate~.,data=college_rest[bootstrap_single,-1])
summary(bag.tree)
plot(bag.tree)
text(bag.tree,pretty=1)

bag_single_pre = predict(bag.tree,newdata = college_test)
RMSE_2d = sqrt(sum((bag_single_pre-college_test$Grad.Rate)^2)/num_test)
RMSE_2d

# 3. Bagging: 200 tree
library(randomForest)

bag.trees=randomForest(Grad.Rate~.-X,data=college_rest,ntree=200,mtry=17,importance=TRUE)
bag.trees
importance(bag.trees)
varImpPlot(bag.trees)

bag_200_pre = predict(bag.trees,newdata = college_rest)
RMSE_3b = sqrt(sum((bag_200_pre-college_rest$Grad.Rate)^2)/num_rest)
RMSE_3b

num_test = nrow(college_test)
bag_200_test = predict(bag.trees,newdata = college_test)
RMSE_3c = sqrt(sum((bag_200_test-college_test$Grad.Rate)^2)/num_test)
RMSE_3c

bag_single_test = predict(bag.tree,newdata = college_test)
RMSE_3c_single = sqrt(sum((bag_single_test-college_test$Grad.Rate)^2)/num_test)
RMSE_3c_single

# 4. Random Forest: 200 trees, 4 random variables
rf.trees=randomForest(Grad.Rate~.-X,data=college_rest,ntree=200,mtry=4,importance=TRUE)
rf.trees
importance(rf.trees)
varImpPlot(rf.trees)

rf_200_test = predict(rf.trees,newdata = college_test)
RMSE_4c = sqrt(sum((rf_200_test-college_test$Grad.Rate)^2)/num_test)
RMSE_4c

# 5. Boosting: GBM, 200 trees
library(gbm)
boost=gbm(Grad.Rate~.-X-Private+as.numeric(ifelse(Private=='Yes',1,0)),
                 data=college_rest,distribution="gaussian",n.trees=200)
summary(boost)

boost_200_test = predict(boost,newdata = college_test,n.trees=200)
RMSE_5b = sqrt(sum((boost_200_test-college_test$Grad.Rate)^2)/num_test)
RMSE_5b

# 6. Repeat ensemble methods using 1000 trees
# 6.1 Bagging
bag.1000=randomForest(Grad.Rate~.-X,data=college_rest,ntree=1000,mtry=17,importance=TRUE)

bag_1000_test = predict(bag.1000,newdata = college_test)
RMSE_6.1 = sqrt(sum((bag_1000_test-college_test$Grad.Rate)^2)/num_test)
RMSE_6.1

# 6.2 Random Forest
rf.1000=randomForest(Grad.Rate~.-X,data=college_rest,ntree=1000,mtry=4,importance=TRUE)

rf_1000_test = predict(rf.1000,newdata = college_test)
RMSE_6.2 = sqrt(sum((rf_1000_test-college_test$Grad.Rate)^2)/num_test)
RMSE_6.2

# 6.3 Boosting GBM
boost_1000=gbm(Grad.Rate~.-X-Private+as.numeric(ifelse(Private=='Yes',1,0)),
                 data=college_rest,distribution="gaussian",n.trees=1000)

boost_1000_test = predict(boost_1000,newdata = college_test,n.trees=1000)
RMSE_6.3 = sqrt(sum((boost_1000_test-college_test$Grad.Rate)^2)/num_test)
RMSE_6.3



