## BUDT 758T: Extra Topic, Web Scraping
## Professor: Courtney Paulson, Spring 2019
## Note: web scraping is a huge field; this covers only some of the basics!
## I've also included some text string analysis/cleanup options here
## So this goes well with the text mining slides/code also up on Canvas

## Note about APIs:
## Some of you may be familiar with APIs already, but if you aren't:
## https://www.r-bloggers.com/accessing-apis-from-r-and-a-little-r-programming/
## This R bloggers post goes through some great API usage in R

## This script goes through scraping yourself rather than through APIs
## Note that you will need the CSS selector of the data you want in order to scrape it
## A useful tool for this is Selector Gadget: https://selectorgadget.com/
## Click on the information you want to scrape on the website
## If you get too much information highlighted, click on the text
## you DON'T want until only the information you want to scrape is left
## Selector Gadget should then show you what the associated CSS tag is

library(rvest)

#Specifying the url for desired website to be scraped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2017,2017&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
## Note that to do this, I select the rankings with Selector Gadget
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)

#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

#Using CSS selectors to scrape the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')
description_data <- html_text(description_data_html)
head(description_data)

## Need to remove the \n values here
description_data<-gsub("\n","",description_data)
head(description_data)


#Using CSS selectors to scrape the movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')
runtime_data <- html_text(runtime_data_html)
head(runtime_data)
runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)
head(runtime_data)

#Using CSS selectors to scrape the movie genre section
genre_data_html <- html_nodes(webpage,'.genre')
genre_data <- html_text(genre_data_html)
head(genre_data)
genre_data<-gsub("\n","",genre_data)
genre_data<-gsub(" ","",genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)

#Converting each genre from text to factor
genre_data<-as.factor(genre_data)

head(genre_data)

#Using CSS selectors to scrape the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')
rating_data <- html_text(rating_data_html)
rating_data<-as.numeric(rating_data)

#Using CSS selectors to scrape the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
votes_data <- html_text(votes_data_html)
votes_data<-gsub(",","",votes_data)
votes_data<-as.numeric(votes_data)

#Let's have another look at the votes data
head(votes_data)

#Using CSS selectors to scrape the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')

directors_data <- html_text(directors_data_html)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

#Using CSS selectors to scrape the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
actors_data <- html_text(actors_data_html)
actors_data<-as.factor(actors_data)

###But what happens when we do metascore?
#Using CSS selectors to scrape the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)

#Let's have a look at the metascore data
head(metascore_data)

#Data-Preprocessing: removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)

#Lets check the length of metascore data
length(metascore_data)

### Why is this not 100? Try to figure out how you would deal with this!

## Last step: we probably want to create a data frame with all this information

imdb_data=data.frame(Rank = rank_data, Title = title_data,
                     Description = description_data, Runtime = runtime_data,
                     Genre = genre_data, Rating = rating_data,Votes = votes_data, 
                     Director = directors_data, Actor = actors_data)

## Other types of data: Tables

library(XML)

## A messy webpage with Asian population tables
url = "http://www.worldometers.info/world-population/asia-population/"

## This will read in all tables on the webpage
tables = readHTMLTable(url,stringsAsFactors=FALSE)

## How many tables does R think there are?
summary(tables)

## Let's look at each table individually
tables[[1]]
tables[[2]]
tables[[3]]
tables[[4]]

## Are these numerical values?
typeof(tables[[3]]$Year)


# Let's just read the third table directly by itself:
doc = htmlParse(url)
tableNodes = getNodeSet(doc, "//table")
tb = readHTMLTable(tableNodes[[3]])

# What if we wanted to change the column names?

tb = readHTMLTable(tableNodes[[2]],
    header = c("Area","Current Population"),
    trim = TRUE, stringsAsFactors = FALSE)

# What if we wanted to change the column types?
tb = readHTMLTable(tableNodes[[2]],
                   header = c("Area","Current Population"),
                   colClasses = c("character","integer"),
                   trim = TRUE, stringsAsFactors = FALSE)

## Why didn't this work? What should you do instead?
tb$`Current Population`<-gsub("\\(","",tb$`Current Population`)
tb$`Current Population`<-gsub("\\)","",tb$`Current Population`)
tb$`Current Population`<-gsub(",","",tb$`Current Population`)

tb$`Current Population`=as.numeric(tb$`Current Population`)

## Remember, we also had an example of pulling from Twitter in the Trump Lab:

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
all_tweets <- map(2015:2018, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  mutate(created_at = parse_date_time(created_at, "a b! d! H!:M!:S! z!* Y!")) %>%
  tbl_df()

## This is much more complex! What is it doing in each line?

