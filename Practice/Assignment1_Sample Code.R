# Assignment 1 Sample Answer Code
# BUDT 758T, Professor Paulson
# Note: please see the sample answer key for complete answers;
#       this file contains only the necessary/helpful code.

#### Question 1

## a. Load the Airlines data into R and attach it to your R session.
## You may have loaded in your data using RStudio
## If not, you may have used:

airlines <- read.csv("Airlines_Data.csv")
attach(airlines)

## b.	Change the cc3_miles variable to a factor variable using as.factor() in R.

cc3_miles=as.factor(cc3_miles)

## c.	Create two new variables for the data set:

Years_since_enroll=Days_since_enroll/365
Loyal_customer=ifelse(Years_since_enroll>5,1,0)

## d.	Calculate the mean and median of the Balance variable.

mean(Balance)
median(Balance)

## e.	Present a blue histogram of the Balance variable.

hist(Balance,col = "blue")

## f.	Calculate the mean of Award.

mean(Award)


#### Question 2

## a.	Run a simple linear regression in R using Award to predict Balance.
##    Report the summary of the regression.

model1=glm(Balance~Award) ## Alternately: model1=lm(Balance~Award)
summary(model1)

## b.	Create and report a scatterplot with the variable Balance on the 
##    y-axis and Award on the x-axis. Add the regression line from the
##    model in part (a) to the plot in red with a line thickness of 3.

plot(Award, Balance)
abline(model1, col="red", lwd=2)


#### Question 3

## a.	  Run a multiple linear regression in R using Award, cc3_miles, 
##      Years_since_enroll, Loyal_customer, Bonus_miles, and Bonus_trans
##      to predict Balance. Report the summary of the regression.

model2=glm(Balance~Award+cc3_miles+Years_since_enroll+Loyal_customer+Bonus_miles+Bonus_trans)
## Alternately: model2=lm(Balance~Award+cc3_miles+Years_since_enroll+Loyal_customer+Bonus_miles+Bonus_trans)
summary(model2)

## d.	What is the predicted balance for a customer who has:
#     Award = 0
#     cc3_miles = 3
#     Years_since_enroll = 6
#     Loyal_customer = 1
#     Bonus_miles = 20000
#     Bonus_trans = 20

## There are multiple ways to do this. One is to plug these in to the model.
## An easier way in R is to use the predict() function:

newrow <- data.frame(Award=0, cc3_miles = "3", Years_since_enroll = 6,
                     Loyal_customer = 1, Bonus_miles = 20000,
                     Bonus_trans = 20)
predict(model2, newdata = newrow)

## e. The consultant appears to think there is an interaction between
##    Loyal_customer and Bonus_trans:

model3=glm(Balance~Award+cc3_miles+Years_since_enroll+Loyal_customer+Bonus_miles+Bonus_trans+Loyal_customer*Bonus_trans)
## Alternately: model3=lm(Balance~Award+cc3_miles+Years_since_enroll+Loyal_customer+Bonus_miles+Bonus_trans+Loyal_customer*Bonus_trans)
summary(model3)


#### Question 4

## b.	  Run a multiple linear regression in R using Balance, cc3_miles, 
##      Years_since_enroll, Loyal_customer, Bonus_miles, and Bonus_trans
##      to predict Award. Report the summary of the regression.

model4=glm(Award~Balance+cc3_miles+Years_since_enroll+Loyal_customer+Bonus_miles+Bonus_trans)
## Alternately: model4=lm(Award~Balance+cc3_miles+Years_since_enroll+Loyal_customer+Bonus_miles+Bonus_trans)
summary(model4)
