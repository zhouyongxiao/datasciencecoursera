## load all necessary packages and set multi-core CPU
library(tidyverse) #keepin' things tidy
library(tidytext) #package for tidy text analysis (Check out Julia Silge's fab book!)
library(glue) #for pasting strings
library(data.table) #for rbindlist, a faster version of rbind
library(doParallel)
library(stringi)
library(tm)
library(tau)
library(plyr)
library(dplyr)
library(readr)
library(plotly)
library(RWeka)
cl <- makeCluster(detectCores()[[1]])
registerDoParallel(12)
getDoParWorkers()

## set working directory and load data
wd <- getwd()
wd <- paste(wd,'/final/en_US', sep = "")
setwd(wd)
twitter_raw <- readLines("en_US.twitter.txt", warn = FALSE)
blogs_raw <- readLines("en_US.blogs.txt", warn = FALSE)
news_raw <- readLines("en_US.news.txt", warn = FALSE)
twitter <- sample(as.list(twitter_raw), floor(length(as.list(twitter_raw))/20))
blogs <- sample(as.list(blogs_raw), floor(length(as.list(blogs_raw))/20))
news <- sample(as.list(news_raw), floor(length(as.list(news_raw))/20))
#twitter <- read.delim("en_US.twitter.txt", header = FALSE, sep = "\n")
#blogs <- read.delim("en_US.blogs.txt", header = FALSE, sep = "\n")
#news <- read.delim("en_US.news.txt", header = FALSE, sep = "\n")

## create a function to calculate length of a string
length_cal <- function(stri){
        stri <- as.character(stri)
        sapply(strsplit(stri, " "), length)}
dmt <- sapply(twitter,length_cal)
dmb <- sapply(blogs,length_cal)
dmn <- sapply(news,length_cal)

## create a function to calculate love and hate frequency
lvct <- function(stri){stri <- as.character(stri); grepl("love",stri)}
htct <- function(stri){stri <- as.character(stri); grepl("hate",stri)}
lv <- lapply(twitter, lvct)
lvl <- length(lv[lv=="TRUE"])
ht <- lapply(twitter, htct)
htl <- length(ht[ht=="TRUE"])
lvl/htl

## create a function to find biostats
biostats <- function(stri){stri <- as.character(stri); grepl("biostats",stri)}
bio <-lapply(twitter, biostats)
bios <- twitter[which(bio == "TRUE")]

sen <- function(stri){stri <- as.character(stri); grepl("A computer once beat me at chess, but it was no match for me at kickboxing",
                                                             stri)}
senct <-lapply(twitter, sen)
length(senct[senct=="TRUE"])


## count word frequency in blogs 
removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x) #remove url
removeHashTags <- function(x) gsub("#\\S+", "", x)
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)

#data <- list(blogs = blogs, twitter = twitter, news = news)

filter_blogdata <- lapply(blogs,removeURL)
filter_blogdata <- lapply(filter_blogdata,removeHashTags)
filter_blogdata <- tm::VCorpus(VectorSource(filter_blogdata))
filter_blogdata <- tm::tm_map(filter_blogdata, removeWords, stopwords('english'))
filter_blogdata <- tm::tm_map(filter_blogdata, removePunctuation)
filter_blogdata <- tm::tm_map(filter_blogdata, removeNumbers)
tdm_blogdata <- tm::TermDocumentMatrix(filter_blogdata)
freq.blog <- data.frame("frequency" = slam::row_sums(tdm_blogdata), "word" = tdm_blogdata$dimnames$Terms)
freq.blog <- (freq.blog[order(freq.blog$frequency, decreasing = TRUE),])
#Check the news 
filter_newsdata <- lapply(news,removeURL)
filter_newsdata <- lapply(filter_newsdata,removeHashTags)
filter_newsdata <- tm::VCorpus(VectorSource(filter_newsdata))
filter_newsdata <- tm::tm_map(filter_newsdata, removeWords, stopwords('english'))
filter_newsdata <- tm::tm_map(filter_newsdata, removeNumbers)
filter_newsdata <- tm::tm_map(filter_newsdata, removePunctuation)
tdm_newsdata <- tm::TermDocumentMatrix(filter_newsdata)
freq.news <- data.frame("frequency" = slam::row_sums(tdm_newsdata), "word" = tdm_newsdata$dimnames$Terms)
freq.news <- (freq.news[order(freq.news$frequency, decreasing = TRUE),])

#check twitter
filter_twitterdata <- lapply(twitter,removeURL)
filter_twitterdata <- lapply(filter_twitterdata,removeHashTags)
filter_twitterdata <- tm::VCorpus(VectorSource(filter_twitterdata))
filter_twitterdata <- tm::tm_map(filter_twitterdata, removeWords, stopwords('english'))
filter_twitterdata <- tm::tm_map(filter_twitterdata, removePunctuation)
filter_twitterdata <- tm::tm_map(filter_twitterdata, removeNumbers)
tdm_twitterdata <- tm::TermDocumentMatrix(filter_twitterdata)
freq.twitter <- data.frame("frequency" = slam::row_sums(tdm_twitterdata), "word" = tdm_twitterdata$dimnames$Terms)
freq.twitter <- (freq.twitter[order(freq.twitter$frequency, decreasing = TRUE),])

BigramTokenizer <- function(x)
                unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
TrigramTokenizer <- function(x)
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
FourgramTokenizer <- function(x)
        unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
#FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

bigram.tdm.blog <- tm::TermDocumentMatrix(filter_blogdata, control = list(tokenize = BigramTokenizer))
freq.bigram.blog <- data.frame("frequency" = slam::row_sums(bigram.tdm.blog), "word" = bigram.tdm.blog$dimnames$Terms)
freq.bigram.blog <- (freq.bigram.blog[order(freq.bigram.blog$frequency, decreasing = TRUE),])

bigram.tdm.news <- tm::TermDocumentMatrix(filter_newsdata, control = list(tokenize = BigramTokenizer))
freq.bigram.news <- data.frame("frequency" = slam::row_sums(bigram.tdm.news), "word" = bigram.tdm.news$dimnames$Terms)
freq.bigram.news <- (freq.bigram.news[order(freq.bigram.news$frequency, decreasing = TRUE),])

bigram.tdm.twitter <- tm::TermDocumentMatrix(filter_twitterdata, control = list(tokenize = BigramTokenizer))
freq.bigram.twitter <- data.frame("frequency" = slam::row_sums(bigram.tdm.twitter), "word" = bigram.tdm.twitter$dimnames$Terms)
freq.bigram.twitter <- (freq.bigram.twitter[order(freq.bigram.twitter$frequency, decreasing = TRUE),])

trigram.tdm.blog <- tm::TermDocumentMatrix(filter_blogdata, control = list(tokenize = TrigramTokenizer))
freq.trigram.blog <- data.frame("frequency" = slam::row_sums(trigram.tdm.blog), "word" = trigram.tdm.blog$dimnames$Terms)
freq.trigram.blog <- (freq.trigram.blog[order(freq.trigram.blog$frequency, decreasing = TRUE),])

trigram.tdm.news <- tm::TermDocumentMatrix(filter_newsdata, control = list(tokenize = TrigramTokenizer))
freq.trigram.news <- data.frame("frequency" = slam::row_sums(trigram.tdm.news), "word" = trigram.tdm.news$dimnames$Terms)
freq.trigram.news <- (freq.trigram.news[order(freq.trigram.news$frequency, decreasing = TRUE),])

trigram.tdm.twitter <- tm::TermDocumentMatrix(filter_twitterdata, control = list(tokenize = TrigramTokenizer))
freq.trigram.twitter <- data.frame("frequency" = slam::row_sums(trigram.tdm.twitter), "word" = trigram.tdm.twitter$dimnames$Terms)
freq.trigram.twitter <- (freq.trigram.twitter[order(freq.trigram.twitter$frequency, decreasing = TRUE),])
