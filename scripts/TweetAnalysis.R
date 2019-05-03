library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)

library(cld3)

tweets <- read.csv("MontrealFlood_Pilot.csv")

## Processing the Tweet data
text <- data.frame(date = tweets$date, tweets = tweets$tweet)

#These ones reformat the date and tweets field to characters
text$date <- as.character(text$date)
text$tweets <- as.character(text$tweets) 


##To count the number of tweets in languages in the tweets data frame
langs <- detect_language(tweets$tweet)
tweets$tweet <- as.character(tweets$tweet)
l <- data.frame(langs)
l$langs <- as.character(l.langs)
num_lang <- l %>% group_by(langs) %>% summarize(count=n())
# This will output a data frame with a count for the number of tweets in particular languages







tidy_tweets <- text %>% unnest_tokens(word, tweets) #separates table of tweets nto words
cleaned_tweets <- tidy_tweets %>% anti_join(get_stopwords()) #removes stopwords

########
######## Sentiment Analysis is possible below this line
########

#gets the most common words

tidy_tweets %>% count(word, sort = TRUE) 

#Filters and reveals how many joy words are used in tidy tweets
nrc_joy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
tidy_tweets %>% semi_join(nrc_joy) %>% count(word, sort = TRUE)

#bing analysis by date
bing <- get_sentiments("bing")
tweetsent <- tidy_tweets %>% inner_join(bing) %>% count(date, sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)
tweetsent$date <- as.Date(tweetsent$date, "%Y-%M-%d")

plot(tweetsent$sentiment ~ tweetsent$date, xlab="Date", ylab="Sentiment", main="Bing sentiment score")

