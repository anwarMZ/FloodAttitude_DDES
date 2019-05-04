library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)

library(cld3)
setwd("~/GitHub/FloodAttitude_DDES/")
tweets <- read.csv("./dat/FullMontreal_data.csv")

## Processing the Tweet data
text <- data.frame(date = tweets$date, tweets = tweets$tweet, retweets = tweets$retweets_count, favs = tweets$likes_count, id = tweets$id)

#These ones reformat the date, tweets, favourites and retweets field to characters and numeric
text$date <- as.character(text$date)
text$tweets <- as.character(text$tweets) 
text$retweets <- as.numeric(text$retweets)
text$favs <- as.numeric(text$favs)
text$id <- as.character(text$id)

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

# combining tweets and calculating mean mood per tweet
tweetsent <- tidy_tweets %>% 
  as_tibble %>% 
  inner_join(afinn %>% ungroup, by ="word") %>%
  group_by(date, id, retweets, favs) %>%
  summarize(mean_emotion = mean(score)) %>% 
  mutate(popularity_index = retweets + favs/2 + 0)


whotweet <- tweets %>% 
  select(id, username) %>% 
  mutate(id = as.character(id))

tweet_who_mood <- tweetsent %>% 
  left_join(whotweet, by = "id")

tweet_who_mood$username %>% unique %>% length

tweet_who_mood %>% ungroup %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  ggplot(aes(x=date, y=mean_emotion, group = username)) +
  geom_line(alpha = 0.2)


tweet_who_mood %>% ungroup %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  ggplot(aes(x=date, y=mean_emotion, size = log(popularity_index + 1))) +
  geom_point(alpha = 0.08) + 
  theme_minimal()

# how do people feel?
tweet_who_mood %>% 
  glimpse %>% 
  group_by(username) %>% 
  summarize(mean_mean_emo = mean(mean_emotion),
            tweetcount = n()) %>% 
  mutate(moodrank = dense_rank(mean_mean_emo)) %>% 
  ggplot(aes(x = moodrank, y = mean_mean_emo, size = tweetcount)) + 
  geom_point()

curve(qnorm)

tweet_who_mood %>% 
  glimpse %>% 
  group_by(username) %>% 
  summarize(mean_mean_emo = mean(mean_emotion),
            tweetcount = n()) %>% 
  ggplot(aes(x = mean_mean_emo)) + geom_histogram(binwidth = 0.5)

tweet_who_mood %>% ungroup %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  ggplot(aes(x=date, y=mood)) +
  geom_hex()




#nrc - Category analysis
#Filters and reveals how many joy words are used in tidy tweets
nrc <- get_sentiments("nrc")
nrc
tweetsent <- tidy_tweets %>% 
  ungroup %>% 
  inner_join(nrc %>% ungroup, by = "word") %>%
  #mutate(score = retweets + favs/2 + 1) %>%
  group_by(sentiment, date) %>%
  tally %>% 
  ggplot(aes(x=date, y=n, color=sentiment)) + geom_point()
tweetsent

