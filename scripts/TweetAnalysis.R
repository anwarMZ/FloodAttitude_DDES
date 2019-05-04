library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(cld3)

setwd("~/Desktop/Data-driven/FloodAttitude_DDES")
tweets <- read.csv("./dat/FullMontreal_data.csv")

## Processing the Tweet data
text <- data.frame(date = tweets$date, tweets = tweets$tweet, retweets = tweets$retweets_count, favs = tweets$likes_count)

# These ones reformat the date, tweets, favourites and retweets field to characters and numeric
text$date <- as.character(text$date)
text$tweets <- as.character(text$tweets) 
text$retweets <- as.numeric(text$retweets)
text$favs <- as.numeric(text$favs)

## To summarize the division of tweets in languages in the tweets data frame
langs <- detect_language(text$tweets)
text$tweets <- as.character(text$tweets)
l <- data.frame(langs)
l$langs <- as.character(l$langs)
num_lang <- l %>% group_by(langs) %>% summarize(count=n())

# This will output a data frame with a count for the number of tweets in particular languages
tidy_tweets <- text %>% unnest_tokens(word, tweets) #separates table of tweets into words
cleaned_tweets <- tidy_tweets %>% anti_join(get_stopwords()) #removes stopwords



########
######## Sentiment Analysis is possible below this line
########

# gets the most common words
tidy_tweets %>% count(word, sort = TRUE)
View(tidy_tweets)

# bing analysis by date (+ive/-ive)
bing <- get_sentiments("bing")
bing
tweetsent <- tidy_tweets %>% 
    inner_join(bing) %>%
    mutate(score = retweets + favs/2 + 1) %>%
    group_by(sentiment, date) %>%
    summarize(mood = sum(score)) %>%
    ggplot(aes(x=as.Date(date), y=mood, color=sentiment)) + geom_point() + geom_line(aes(group = sentiment))
tweetsent + ggtitle("Temporal variation in sentiments over time") + xlab("Date") + ylab("Mood") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

# plot total difference
changes_total <- changes_w %>% 
  spread(key = sentiment, value = mood) %>%
  mutate(diff = positive - negative) %>%
  ggplot(aes(x = as.Date(date), y = diff)) + geom_point() + geom_line()
changes_total + ggtitle("Difference in sentiments over time") + ylab("Difference in sentiment values") + xlab("Date") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Date") + ylab("Mood") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

################# redo steps above but for popular tweets only ##################
# Filter popular tweets (minimum 5 retweets/favs)
pop_tweets <- text %>%
  mutate(popularity = retweets + favs) %>%
  filter(popularity > 20)

# This will output a data frame with a count for the number of tweets in particular languages
tidy_pop_tweets <- pop_tweets %>% 
  unnest_tokens(word, tweets)                # separates table of tweets nto words
cleaned_pop_tweets <- tidy_pop_tweets %>% 
  anti_join(get_stopwords())                # removes stopwords

# bing analysis by date (+ive/-ive) weighted by score (retweets/likes)
bing <- get_sentiments("bing")
pop_changes <- tidy_pop_tweets %>% 
  inner_join(bing) %>%
  mutate(score = retweets + favs/2 + 1) %>%
  group_by(sentiment, date) %>%
  summarize(mood = sum(score))

# plot +/- changes
plot_pop_changes <- pop_changes %>%
  ggplot(aes(x=as.Date(date), y=mood, color=sentiment)) + geom_point() + geom_line(aes(group = sentiment)) 
plot_pop_changes + ggtitle("Temporal variation in sentiments over time for popular tweets") + ylab("Mood") +
  xlab("Date") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# calculate total difference
pop_total <- pop_changes %>% 
  spread(key = sentiment, value = mood) %>%
  mutate(diff = positive - negative) %>%
  ggplot(aes(x = as.Date(date), y = diff)) + 
  geom_line(aes(y = negative, colour = "Negative")) + 
  geom_line(aes(y = positive, colour = "Positive")) + 
  geom_line(aes(y = diff, colour = "Difference"))

# plot total difference
changes_total + ggtitle("Difference in sentiments over time") + ylab("Difference in sentiment values") + xlab("Date") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
