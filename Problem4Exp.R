library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(pacman)

# Declare Twitter API Credentials
api_key <- "cPUm9nuc6OSfUa6PS5CeYw1pb" 
api_secret <- "Ybb33RKYJXtJ4lYzu3DJfbu6MgO72se2O9Gw35LzDNYbazEv4U" 
token <- "3599656937-W0eQk7ICa548kzz2niDepAVWNegurSB8O8NZKDy"
token_secret <- "KBrMzisW8hTSrBZe75WY88t1wSeR24QBe76GghTgBxr5n"

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).

tweets1 <- searchTwitter("NY rentals OR rental in NY OR from:craignewmark OR from:NewYorkHabitat", n=10000, lang="en", since="2016-02-08")
tweets2 <- searchTwitter("'NY rentals'", n=1000, lang="en",since="2016-02-08")
tweets3 <- searchTwitter("'rental in NY'", n=1000, lang="en", since="2016-02-08")
tweets4 <- searchTwitter("'nyc rent'", n=1000, lang="en", since="2016-02-01")
tweets5 <- searchTwitter("'nyc rentals'", n=1000, lang="en", since="2016-02-01")

#Storing the data in df files from a variables
tweetsnyc.df <- twListToDF(tweets1)
tweetsnyc2.df <- twListToDF(tweets2)
tweetsnyc3.df <- twListToDF(tweets3)
tweetsnyc4.df <- twListToDF(tweets4)
tweetsnyc5.df <- twListToDF(tweets5)

#Storing the data from multiple df files to a single df file
tweetsnyc.df<-rbind(tweetsnyc.df,tweetsnyc2.df)
tweetsnyc.df<-rbind(tweetsnyc.df,tweetsnyc3.df)
tweetsnyc.df<-rbind(tweetsnyc.df,tweetsnyc4.df)
tweetsnyc.df<-rbind(tweetsnyc.df,tweetsnyc5.df)

#Storing the data in a single variable from a df file
tweetstxt <-tweetsnyc.df


#code to install sentiment packagee
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, installr)
install.Rtools()
install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")


# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
tweetstxt = sapply(tweetstxt$text, try.error)

tweetstxt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetstxt)
# remove at people
tweetstxt = gsub("@\\w+", "", tweetstxt)
# remove punctuation
tweetstxt = gsub("[[:punct:]]", "", tweetstxt)
# remove numbers
tweetstxt = gsub("[[:digit:]]", "", tweetstxt)
# remove html links
tweetstxt = gsub("http\\w+", "", tweetstxt)
# remove unnecessary spaces
tweetstxt = gsub("[ \t]{2,}", "", tweetstxt)
tweetstxt = gsub("^\\s+|\\s+$", "", tweetstxt)

# remove NAs in some_txt
tweetstxt = tweetstxt[!is.na(tweetstxt)]
names(tweetstxt) = NULL

library(sentiment)
# classify emotion
class_emo = classify_emotion(tweetstxt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
emotion[is.na(emotion)] = "happy"

# classify polarity
class_pol = classify_polarity(tweetstxt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]


# data frame with results
sent_df = data.frame(text=tweetstxt, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))



# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +geom_bar(aes(y=..count.., fill=emotion)) +scale_fill_brewer(palette="Dark2") +labs(x="emotion categories", y="number of tweets") +ggtitle("Sentiment Analysis of Tweets about RealDirect\n(classification by emotion)") +theme(plot.title = element_text(size=12, face="bold"))

ggplot(sent_df, aes(x=polarity)) +geom_bar(aes(y=..count.., fill=polarity)) +scale_fill_brewer(palette="RdGy") +labs(x="polarity categories", y="number of tweets") +ggtitle("Sentiment Analysis of Tweets about RealDirect\n(classification by polarity)") +theme(plot.title = element_text(size=12))

