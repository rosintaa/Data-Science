library(tm)
library(wordcloud2)
library(twitteR)
library(rtweet)

reqURL <- "http://api.twitter.com/oath/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
api_key<- "LK3HoU3kjEltOtNVCpcg4J448"
api_secret<- "EFQfzAMpQAWQwlpeg0jsIChJQkwvJYXfNfnZgyi3YCrQ3Ku7l8"
access_token<- "1322548274838949891-wxfiZhaox9ZUK4KnrwHlNTDz2iybVv"
access_token_secret<- "yf2mAbS3aKnp2A7nLDh9qv8RryLSerbsfn81GtEIFhR9s"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tw = searchTwitter('#shopeeid',n = 1000,retryOnRateLimit = 10e5) 
saveRDS(tw,file = 'tweet.rds')

tw <- readRDS('tweet.rds')
d = twListToDF(tw)
komen <- d$text
komenc <- Corpus(VectorSource(komen))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
twitclean <- tm_map(komenc, removeURL)
removeNL <- function(y) gsub("\n", " ", y)
twitclean <- tm_map(twitclean, removeNL)
replacecomma <- function(y) gsub(",", "", y)
twitclean <- tm_map(twitclean, replacecomma)
removeRT <- function(y) gsub("RT ", "", y)
twitclean <- tm_map(twitclean, removeRT)
removetitik2 <- function(y) gsub(":", "", y)
twitclean <- tm_map(twitclean, removetitik2)
removetitikkoma <- function(y) gsub(";", " ", y)
twitclean <- tm_map(twitclean, removetitikkoma)
removeamp <- function(y) gsub("&amp;", "", y)
twitclean <- tm_map(twitclean, removeamp)
removeUN <- function(z) gsub("@\\w+", "", z)
twitclean <- tm_map(twitclean, removeUN)
remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)
twitclean <- tm_map(twitclean, removePunctuation)
twitclean <- tm_map(twitclean, tolower)
myStopwords = readLines("stopwords.txt")
twitclean <- tm_map(twitclean,removeWords,myStopwords)
twitclean <- tm_map(twitclean , removeWords,c('b','rp',''))

dataframe<-data.frame(text=unlist(sapply(twitclean, `[`)), stringsAsFactors=F)
View(dataframe)
write.csv(dataframe,file = 'tweetclean.csv')
