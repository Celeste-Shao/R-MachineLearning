# BUDT 758T: Spring 2019
# Instructor: Courtney Paulson
## Note: this is a basic R command file for you to review before
## Thursday's (Jan. 24) class, where we'll be doing more advanced R work.
## Make sure you know what the code in this file is doing!

# Basic Commands

### Note that using a "#" before a line will comment it out of your code (that line won't be run)
### So as long as I put a "#" first, I can write whatever I want ~*@./

### To run a line of code in R, use the "Run" button at the top of this box (upper righthand corner)
### Or (easier!) use "control+enter" on Windows or "command-enter" on Mac

## Probably the most important command to know is ?
## If you put a ? before a function, it will bring up the help file for that function
## Example:

?glm


## You can always put your data in yourself rather than loading it
## The c() function tells R to put the values in the parentheses together
## Here are some examples you can run to see how data is loaded into R manually:

x<-c(1,3,2,5)
x
x=c(1,6,2)
x
y=c(1,4,3)
x+y

## The ls() ("list") function will always tell you what variables/objects you've already loaded into your R session
## More conveniently, though, in RStudio these are listed in the upper right corner

ls()

## The rm() ("remove") function removes any variables/objects in the parentheses

rm(x,y)
ls()

## You can also load in your data as a matrix rather than a vector
## The matrix() function always follows the same convention:
# matrix(values for matrix, number of rows, number of columns)
# So, to put the numbers 1 through 4 in a matrix with 2 rows and 2 columns:

x=matrix(c(1,2,3,4),2,2)
x[1]
x[2]
x[3]
x[4]
x

## More often, you want a specific row or column of the matrix.
## For example, maybe I only want the second column:

x[,2]

## Or only the second row:

x[2,]

## You can also run useful data functions like you can in Excel
## For example, the ifelse() function tests if something is true.
## If it is, R will return the first option.
## If it is not, R will return the second option.

ifelse(x==2,"yes","no")

## You can use this to make a new variable if you want as well:

z=ifelse(x==2,1,0)
z

# Common mathematical functions like square root (sqrt) will apply to the entire matrix

sqrt(x)

# You should see:
#       [,1] [,2]
# [1,] 1.00 1.73
# [2,] 1.41 2.00

x^2

# You should see:
#       [,1] [,2]
# [1,]    1    9
# [2,]    4   16

## Other potentially useful functions:
# rnorm: return a random number from a normal distribution with mean = 0 and sd = 1 (but you can change these values!)
# cor: correlation between two variables
# mean: mean of a variable/object
# var: variance of a variable/object
# sqrt(var()): how to get standard deviation (square root of the variance)
# sd(y): same as sqrt(var())!

x=rnorm(50)
y=x+rnorm(50,sd=.1)
cor(x,y)
mean(y)
var(y)
sqrt(var(y))
sd(y)


# Indexing Data

### This shows you how R refers to/stores various data as matrices

## Matrices are always formatted as Matrix[row number, column number]
## So the third entry in the second row of matrix A would be A[2,3]
## You can also refer to all rows or all columns by not specifying
## Example: you want the entire third column of matrix A, so use A[,3]

A=matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]

## If you DON'T want certain entries in the matrix, use a negative sign
## Example: you want only the first three rows of matrix A, not the fourth: A[-4,]

A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)



# Basic Graphics

## plot() is the basic scatterplot option in R

?plot
plot(x,y)
plot(x,y,xlab="X Variable", ylab="Y Variable", main="Plot of X vs Y",pch=20)
lines(x,y)

## seq() gives a sequence of numbers from your minimum value to your maximum value
x=seq(1,10)
x
x=1:10
x

# Example: Libraries

## Whenever you want to use a specialized package, first install it.
## You can do this with Tools --> Install Packages...
## Then, if you need to use it during your R session, call it into your library:

library(MASS)
library(ISLR)


# Additional Graphical and Numerical Summaries

## The "Auto" data set is a data set that comes automatically loaded
## in the ISLR package (from the class textbook)

View(Auto)
?Auto

## In R, you refer to a specific variable in a data set by using data$variable
## Example: Cylinders and Mpg are two variables in the auto data set:

plot(Auto$cylinders,Auto$mpg)

## But make sure you specify the data set!

plot(cylinders,mpg)

## If you're going to be using the same data set multiple times and don't want to keep typing it
## Use the attach() function to attach it to your R session:

attach(Auto)
plot(cylinders,mpg)

## Sometimes you may need/want to change the type of a variable
## One common option: change a variable to a factor variable

cylinders=as.factor(cylinders)

## You have a number of features with the plot() function:

plot(cylinders,mpg)
plot(cylinders,mpg,col="red")
plot(cylinders,mpg,col="red",varwidth=T)
plot(cylinders,mpg,col="red",varwidth=T,horizontal=T)
plot(cylinders,mpg,col="red",varwidth=T,xlab="Cylinders",ylab="MPG")

## hist() is the function for a histogram:

hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)

## pairs() does a scatterplot matrix for you

pairs(Auto)

## You can also specify a subset of variables you want to actually use
## Note putting data=Auto at the end of a command like this also tells R to use the Auto data set
### (Though we wouldn't need it since we attached Auto here)

pairs(~mpg+displacement+horsepower+weight+acceleration,data=Auto)


## The identify() function is a neat option to name specific points on a graph

plot(horsepower,mpg)
identify(horsepower,mpg,name)

## Summary always does a 5-number summary (+ median) of a variable/data set

summary(Auto)
summary(mpg)
