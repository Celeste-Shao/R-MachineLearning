## BUDT 758T Additional Topic: PCA with Text
## Professor: Courtney Paulson
## Topic: PCA and Text Analysis

library(tm)
library(SnowballC)
library(lsa)

## For this walkthrough, consider the Tide pods review data set.
## This is up on Canvas with this lab if you want to see it.
## As you would expect, it's reviews of Tide pods!

Tide_data=read.csv("Tide_data.csv")

## First, let's try a few numeric variables and see if it might make sense to do PCA
pairs(~document_word_count+standard_isnarative
      +standard_ispersonal,data=Tide_data, 
      main="Simple Scatterplot Matrix")

## Let's try PCA on these three variables and see what happens
pca_model=prcomp(Tide_data[,c(6,12,13)])
summary(pca_model)

## Create new variables for these three
PC_scores=pca_model$x
dim(PC_scores)
head(PC_scores)

## We would probably only want to use the first PC, so let's add it to our data set:
Tide_data$PC1=PC_scores[,1]

## We could then use this in a regression just like any other variable
## For example, let's see what happens if we try to predict RATING

## First: let's use our three variables from before, document_word_count, standard_isnarative
##        and standard_ispersonal

model1=lm(RATING~document_word_count+standard_isnarative+standard_ispersonal,data=Tide_data)
summary(model1)

## Now, let's try with only the first PC:

model2=lm(RATING~PC1,data=Tide_data)
summary(model2)

## These aren't good models, but notice the standard error: same for both models
## We've also made an improvement in our variables/p-values/adjusted R-squared

## Let's do something slightly more interesting: text analysis

tide_reviews=Tide_data$content_cleaned

## Remember from our text mining unit?

corp <- Corpus(VectorSource(tide_reviews))

## Get rid of whitespace, punctuation
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)

## Remember stopwords?

stopwords("english")
corp <- tm_map(corp, removeWords, stopwords("english"))

## Remember stemming?
corp <- tm_map(corp, stemDocument)

## Now turn your corpus into a Term Document Matrix
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

## Remember TF-IDF?
tfidf <- weightTfIdf(tdm)
inspect(tfidf)

## Latent Semantic Analysis
## Very similar to PCA (same idea)
## This extracts "concepts" from the TF-IDF matrix
## We don't want to use the whole TF-IDF matrix, so instead we choose the best concepts from it
## Here, we pick 20 concepts

lsa.tfidf <- lsa(tfidf, dim = 20)
words.df <- as.data.frame(as.matrix(lsa.tfidf$dk))
View(words.df)
dim(words.df)

## These are pretty meaningless to us, just like PCA scores are
## However, we can use them to run analysis just like PCA

words.df$Rating=Tide_data$RATING
model3=lm(Rating~.,data=words.df)
summary(model3)


lsa.tfidf2 <- lsa(tfidf, dim = 30)
words.df2 <- as.data.frame(as.matrix(lsa.tfidf2$dk))
words.df2$Rating=Tide_data$RATING
model4=lm(Rating~.,data=words.df2)
summary(model4)

lsa.tfidf3 <- lsa(tfidf, dim = 50)
words.df3 <- as.data.frame(as.matrix(lsa.tfidf3$dk))
words.df3$Rating=Tide_data$RATING
model5=lm(Rating~.,data=words.df3)
summary(model5)

## We can use this just like we would any other data
## It looks like our model is getting better, but let's try prediction
## Do our usual training and testing split:

set.seed(1234)
num_obs=nrow(words.df2)
training_obs=sample(num_obs,0.70*num_obs)
words.train1=words.df[training_obs,]
words.test1=words.df[-training_obs,]

pred.model1=lm(Rating~.,data=words.train1)
pred.ratings1=predict(pred.model1,newdata=words.test1)

words.train2=words.df2[training_obs,]
words.test2=words.df2[-training_obs,]

pred.model2=lm(Rating~.,data=words.train2)
pred.ratings2=predict(pred.model2,newdata=words.test2)

words.train3=words.df3[training_obs,]
words.test3=words.df3[-training_obs,]

pred.model3=lm(Rating~.,data=words.train3)
pred.ratings3=predict(pred.model3,newdata=words.test3)

## Calculate RMSE
sqrt(mean(sum((pred.ratings1-words.test1$Rating)^2)))
sqrt(mean(sum((pred.ratings2-words.test2$Rating)^2)))
sqrt(mean(sum((pred.ratings3-words.test3$Rating)^2)))


## But how do we decide an optimal value?
## The lsa package has an option for that: dimcalc_share()


lsa.best <- lsa(tfidf, dims=dimcalc_share())
words.best <- as.data.frame(as.matrix(lsa.best$dk))
words.best$Rating=Tide_data$RATING
model.best=lm(Rating~.,data=words.best)
summary(model.best)

best.train=words.best[training_obs,]
best.test=words.best[-training_obs,]

pred.best=lm(Rating~.,data=best.train)
ratings.best=predict(pred.best,newdata=best.test)

sqrt(mean(sum((ratings.best-best.test$Rating)^2)))


