library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)

library(cld3)
setwd("~/GitHub/FloodAttitude_DDES/")
tweets <- read.csv("./dat/MontrealFlood_Pilot.csv")

## Processing the Tweet data
text <- data.frame(date = tweets$date, tweets = tweets$tweet, retweets = tweets$retweets_count, favs = tweets$likes_count)

#These ones reformat the date, tweets, favourites and retweets field to characters and numeric
text$date <- as.character(text$date)
text$tweets <- as.character(text$tweets) 
text$retweets <- as.numeric(text$retweets)
text$favs <- as.numeric(text$favs)


##To summarize the division of tweets in languages in the tweets data frame
langs <- detect_language(text$tweets)
text$tweets <- as.character(text$tweets)
l <- data.frame(langs)
l$langs <- as.character(l$langs)
num_lang <- l %>% group_by(langs) %>% summarize(count=n())

# This will output a data frame with a count for the number of tweets in particular languages
tidy_tweets <- text %>% unnest_tokens(word, tweets) #separates table of tweets nto words
cleaned_tweets <- tidy_tweets %>% anti_join(get_stopwords()) #removes stopwords



########
######## Sentiment Analysis is possible below this line
########

#gets the most common words
tidy_tweets %>% count(word, sort = TRUE)
View(tidy_tweets)

#bing analysis by date (Binary - +ive/-ive)
bing <- get_sentiments("bing")
bing
tweetsent <- tidy_tweets %>% inner_join(bing) %>%
    mutate(score = retweets + favs/2 + 1) %>%
    group_by(sentiment, date) %>%
    summarize(mood = sum(score)) %>%
    ggplot(aes(x=date, y=mood, color=sentiment)) + geom_point()
tweetsent


##Afinn analysis (score)

tidy_tweets %>% count(word, sort = TRUE) 

#Filters and reveals how many joy words are used in tidy tweets
afinn <- get_sentiments("afinn") 
afinn
tweetsent <- 






##nrc - Category analysis
#Filters and reveals how many joy words are used in tidy tweets
#nrc_joy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
#tidy_tweets %>% semi_join(nrc_joy) %>% count(word, sort = TRUE)

