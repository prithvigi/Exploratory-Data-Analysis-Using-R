library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
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

tweetsnyc.df <- twListToDF(tweets1)
tweetsnyc2.df <- twListToDF(tweets2)
tweetsnyc3.df <- twListToDF(tweets3)

tweetsnyc.df<-rbind(tweetsnyc.df,tweetsnyc2.df)
tweetsnyc.df<-rbind(tweetsnyc.df,tweetsnyc3.df)

tweetstxt <-tweetsnyc.df

#Converting to JSON format
twt<-toJSON(tweetstxt)
write(twt,file="tweet.json")