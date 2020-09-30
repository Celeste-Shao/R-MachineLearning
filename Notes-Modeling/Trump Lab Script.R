library(pROC)
library(tidyverse)
library(lubridate)
library(RTextTools)
library(maxent)

set.seed(2)

##this pulls the full set of Trump's tweets between 2015 and 2018
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
all_tweets <- map(2015:2018, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  mutate(created_at = parse_date_time(created_at, "a b! d! H!:M!:S! z!* Y!")) %>%
  tbl_df()
summary(all_tweets)

# segment into retweets and real tweets
retweets <- subset(all_tweets, startsWith(text, ".@") | startsWith(text,'"@') | startsWith(text,'RT'))
real_tweets <- subset(all_tweets, !(startsWith(text, ".@") | startsWith(text,'"@') | startsWith(text,'RT')))


#trump's campaign was announced on june 16, 2015
#trump stopped using an iphone on march 25, 2017

#filter to only include tweets after he announced his candidacy
#also only include iPhone or Android tweets (there are a few misc others)
pol_trump <- subset(real_tweets, real_tweets$created_at >= "2015-06-16")
pol_trump <- subset(pol_trump, pol_trump$source == 'Twitter for iPhone' | pol_trump$source == 'Twitter for Android')
pol_trump$source <- ifelse(pol_trump$source == 'Twitter for iPhone', 'iPhone', 'Android')
table(pol_trump$source)

#these are the tweets that were created before the Android went away
#i.e. we can use the phone type as a proxy label in this period
labeled_inds <- pol_trump$created_at <= "2017-03-25"

#these are the tweets where we don't have a proxy label (i.e. after the Android stopped tweeting)
unlabeled_inds <- pol_trump$created_at >= "2017-03-26"

#create a feature matrix of trump tweet terms
matrix <- create_matrix(pol_trump$text, language="english", removeSparseTerms=0.99, removeStopwords=TRUE, removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)

#divide the feature matrix into labeled and unlabeled tweets based on the dates above
mat <- as.matrix(matrix)
labeled_tweets <- mat[labeled_inds,]
unlabeled_tweets <- mat[unlabeled_inds,]

#pull out the class labels
labels <- pol_trump$source[labeled_inds]

#divide the labeled tweets into training and test data
train_inds <- sample(nrow(labeled_tweets), .7*nrow(labeled_tweets))
train_x <- labeled_tweets[train_inds,]
test_x <- labeled_tweets[-train_inds,]
train_y <- labels[train_inds]
test_y <- labels[-train_inds]

#train a model to predict the class label (iPhone vs. Android)
model <- maxent(train_x, train_y)

## Look at model weights
model_weights=model@weights

## Consider the first 10 weights:
model_weights[1:10,]


## What is Feature 17?
colnames(train_x)[17]
m17 <- subset(model_weights,model_weights[,3] == 17)


## What is Feature 17's relationship to tweeting from the Android? iPhone?
## How about Feature 171?
colnames(train_x)[171]
m171 <- subset(model_weights,model_weights[,3] == 171)


#evaluate the predictions in terms of accuracy and AUC
predictions <- predict(model, test_x)
table(test_y, as.factor(predictions[,1]), dnn=c("actual", "predicted"))
recall_accuracy(test_y, predictions)

#get the AUC
test_y_num <- ifelse(test_y == 'Android', 1,0)
pred_num <- as.numeric(predictions[,3])
roc_obj <- roc(test_y_num, pred_num)
auc(roc_obj)

# 68 tweets predicted as iPhone but actually Android. What are those tweets?
not_iphone <- test_x[(test_y=="Android" & test_y!=as.factor(predictions[,1])),]
mislabeled_tweets=rownames(not_iphone)

## For example, compare tweet #1 to tweet #67
mislabeled_tweets[1]
mislabeled_tweets[67]

## What are the most frequent terms used in these tweets?

term.freq <- colSums(as.matrix(not_iphone))

## Check any word that appears at least 5 times
term.freq <- subset(term.freq, term.freq>=5)
term.freq=sort(term.freq)

df2 <- data.frame(term=names(term.freq), freq = term.freq)
ggplot(df2, aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("Terms")+ylab("Count") + coord_flip()

## In descending order
df2 <- transform(df2, term=reorder(term,freq)) 
ggplot(df2, aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("Terms")+ylab("Count") + coord_flip()


#look at the predicted labels on the unlabeled (recent) tweets
#which are the most likely to be Trump tweets?
#which are the most likely to be campaign tweets?
predictions_use <- as.numeric(predict(model, unlabeled_tweets)[,3])
unlabeled_original <- pol_trump[unlabeled_inds,]
unlabeled_original$prob <- predictions_use

#sort to see most vs. least likely
unlabeled_sorted <- unlabeled_original[order(unlabeled_original$prob),]

#go back and look at the misclassified tweets. what do you think was the cause of their misclassifications?
#try to improve your classifications by changing the featurization.
#OR, can you improve it with more historical tweets?
#other ideas?


