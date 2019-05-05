library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(cld3)

##Reading the data from the csv
tweets <- read.csv("./dat/FullMontreal_data.csv")

## Processing the Tweet data and Metadat (tweet id, retweets, likes, username)
text <- data.frame(date = tweets$date, tweets = tweets$tweet, retweets = tweets$retweets_count, favs = tweets$likes_count, id = tweets$id)

# These ones reformat the date, tweets, favourites and retweets field to characters and numeric
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
tidy_tweets <- text %>% unnest_tokens(word, tweets) #separates table of tweets into words
cleaned_tweets <- tidy_tweets %>% anti_join(get_stopwords()) #removes stopwords


########
######## Sentiment Analysis is possible below this line
########

# gets the most common words
tidy_tweets %>% count(word, sort = TRUE)

# bing analysis by date (+ive/-ive)
bing <- get_sentiments("bing")
bing
tweetsent <- tidy_tweets %>% 
    inner_join(bing, by = "word") %>%
    mutate(date = lubridate::ymd(date)) %>% 
    mutate(score = retweets + favs/2 + 1) %>%
    group_by(sentiment, date) %>%
    summarize(mood = sum(score)) %>%
    ggplot(aes(x=as.Date(date), y=mood, color=sentiment)) + geom_point() + geom_line(aes(group = sentiment))
tweetsent + ggtitle("Time series of variation in sentiments using Binary (+ive/-ive) scale") + xlab("Time") + ylab("Aggregate Score") +
  theme_bw()+
  theme(strip.background =element_rect(fill="White"))+
  theme(strip.text = element_text(colour = 'Black'))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold")) +
  theme(axis.text=element_text(size=10, face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=10,face="bold"),
        legend.title=element_text(size=14, face="bold"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1, size = 10))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+ 
  scale_color_discrete(name = "Sentiment")

# Sentiment total difference
changes_w <- tidy_tweets %>% 
  inner_join(bing, by = "word") %>%
  mutate(date = lubridate::ymd(date)) %>% 
  mutate(score = retweets + favs/2 + 1) %>%
  group_by(sentiment, date) %>%
  summarize(mood = sum(score))

changes_total <- changes_w %>% 
  spread(key = sentiment, value = mood) %>%
  mutate(diff = positive - negative) %>%
  ggplot(aes(x = as.Date(date), y = diff)) + geom_point() + geom_line()
changes_total + ggtitle("Time series of variation in sentiments using Binary (+ive/-ive) scale") + xlab("Time") + ylab("Aggregate Score") +
  theme_bw()+
  theme(strip.background =element_rect(fill="White"))+
  theme(strip.text = element_text(colour = 'Black'))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold")) +
  theme(axis.text=element_text(size=10, face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=10,face="bold"),
        legend.title=element_text(size=14, face="bold"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1, size = 10))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+ 
  scale_color_discrete(name = "Sentiment")



#tidy_tweets %>% count(word, sort = TRUE) 

##Afinn analysis (score)
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
  ggplot(aes(x=date, y=mean_emotion, size = log(popularity_index + 1))) +
  geom_point(alpha = 0.08)+
  ggtitle("Time series of variation in sentiments using the Afinn Spectrum") + xlab("Time") + ylab("Afinn Spectrum") +
  theme_bw()+
  theme(strip.background =element_rect(fill="White"))+
  theme(strip.text = element_text(colour = 'Black'))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold")) +
  theme(axis.text=element_text(size=10, face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=10,face="bold"),
        legend.title=element_text(size=14, face="bold"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1, size = 10))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + 
  scale_size_continuous(name = "Popularity index")


#nrc - Category analysis
#Filters and reveals how many joy words are used in tidy tweets
nrc <- get_sentiments("nrc")
nrc
tweetsent <- tidy_tweets %>% 
  ungroup %>% 
  inner_join(nrc %>% ungroup, by = "word") %>%
  mutate(date = lubridate::ymd(date)) %>% 
  group_by(sentiment, date) %>%
  tally %>% 
  ggplot(aes(x=date, y=n, color=sentiment)) + geom_point() +geom_path(aes(group=sentiment))

tweetsent+ ggtitle("Time series of variation in sentiments using the NRC Categories") + xlab("Time") + ylab("Frequency") +
  theme_bw()+
  theme(strip.background =element_rect(fill="White"))+
  theme(strip.text = element_text(colour = 'Black'))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold")) +
  theme(axis.text=element_text(size=10, face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=10,face="bold"),
        legend.title=element_text(size=14, face="bold"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1, size = 10))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + 
  scale_color_discrete(name = "Sentiment")
