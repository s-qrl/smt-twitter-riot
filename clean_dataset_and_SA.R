library(dplyr)
library(tidyverse)
library(lubridate)
library(syuzhet)
library(ggplot2)

riot_tweets_biden = read.csv("resources/riot_tweets_snscrape_biden_raw.csv")
riot_tweets_trump = read.csv("resources/riot_tweets_snscrape_trump_raw.csv")  

##### Cleaning Dataset
########################

# duplicate ID removal
riot_tweets_biden = riot_tweets_biden %>% distinct(ID, .keep_all = TRUE)
riot_tweets_trump = riot_tweets_trump %>% distinct(ID, .keep_all = TRUE)

# remove text duplicates
riot_tweets_biden = riot_tweets_biden %>% distinct(content, .keep_all = TRUE)
riot_tweets_trump = riot_tweets_trump %>% distinct(content, .keep_all = TRUE)

# remove columns "location" and "coordinates"
riot_tweets_biden = select(riot_tweets_biden, -c(location, coordinates))
riot_tweets_trump = select(riot_tweets_trump, -c(location, coordinates))


##### Analysis for Biden
########################

# perform SA
riot_tweets_biden$sentiment <- get_sentiment(riot_tweets_biden$content, method = "bing")
riot_tweets_biden$sentiment_syuzhet <- syuzhet::get_sentiment(riot_tweets_biden$content, method = "syuzhet")

write.csv(riot_tweets_biden, file = "resources/riot_tweets_biden.csv", row.names = FALSE)

##### Analysis for Trump
########################

# perform SA
riot_tweets_trump$sentiment <- get_sentiment(riot_tweets_trump$content, method = "bing")
riot_tweets_trump$sentiment_syuzhet <- syuzhet::get_sentiment(riot_tweets_trump$content, method = "syuzhet")

write.csv(riot_tweets_trump, file = "resources/riot_tweets_trump.csv", row.names = FALSE)


##### Aggregation

# convert timestamp to datetime 
riot_tweets_trump$timestamp <- ymd_hms(riot_tweets_trump$date)
riot_tweets_biden$timestamp <- ymd_hms(riot_tweets_biden$date)

# aggregate sentiment scores per minute
biden_sentiment <- riot_tweets_biden %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment))

biden_sentiment_syuzhet <- riot_tweets_biden %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment_syuzhet))

trump_sentiment <- riot_tweets_trump %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment))

trump_sentiment_syuzhet <- riot_tweets_trump %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment_syuzhet))


##### Analysis
########################


# adding rolling means (k=10 has been chosen arbitrarily)
biden_sentiment$rollmean = zoo::rollmean(biden_sentiment$avg_sentiment,10, fill = NA)
biden_sentiment_syuzhet$rollmean = zoo::rollmean(biden_sentiment_syuzhet$avg_sentiment,10, fill = NA)

trump_sentiment$rollmean = zoo::rollmean(trump_sentiment$avg_sentiment,10, fill = NA)
trump_sentiment_syuzhet$rollmean = zoo::rollmean(trump_sentiment_syuzhet$avg_sentiment,10, fill = NA)

##### Biden
########################

# visualize biden
ggplot(biden_sentiment, aes(x = minute, y = avg_sentiment)) +
  geom_line(color = "blue") +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiments of Tweets containing 'Biden'")
ggsave('plots/biden_average.png', last_plot(), device = "png")

# visualize biden (syuzhet)
ggplot(biden_sentiment_syuzhet, aes(x = minute, y = avg_sentiment)) +
  geom_line(color = "blue") +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiments of Tweets containing 'Biden'")
ggsave('plots/biden_average_syuzhet.png', last_plot(), device = "png")

# visualize biden (rolling mean)
ggplot(biden_sentiment, aes(x = minute, y = rollmean)) +
  geom_line(color = "blue") +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiments of Tweets containing 'Biden'")
ggsave('plots/biden_rolling_mean.png', last_plot(), device = "png")

# visualize biden (syuzhet rolling mean)
ggplot(biden_sentiment_syuzhet, aes(x = minute, y = rollmean)) +
  geom_line(color = "blue") +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiments of Tweets containing 'Biden'")
ggsave('plots/biden_syuzhet_rolling mean.png', last_plot(), device = "png")

##### Trump
########################

# visualize trump
ggplot(trump_sentiment, aes(x = minute, y = avg_sentiment)) +
  geom_line(color = "red") +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiments of Tweets containing 'Trump'")
ggsave('plots/trump_average.png', last_plot(), device = "png")

# visualize trump (syuzhet)
ggplot(trump_sentiment_syuzhet, aes(x = minute, y = avg_sentiment)) +
  geom_line(color = "red") +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiments of Tweets containing 'Trump'")
ggsave('plots/trump_average_syuzhet.png', last_plot(), device = "png")

# visualize trump (rolling mean)
ggplot(trump_sentiment, aes(x = minute, y = rollmean)) +
  geom_line(color = "red") +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiments of Tweets containing 'Trump'")
ggsave('plots/trump_rolling_mean.png', last_plot(), device = "png")

# visualize trump (syuzhet rolling mean)
ggplot(trump_sentiment_syuzhet, aes(x = minute, y = rollmean)) +
  geom_line(color = "red") +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiments of Tweets containing 'Trump'")
ggsave('plots/trump_syuzhet_rolling mean.png', last_plot(), device = "png")


# comparison

trump_sentiment['dataset'] <- "Trump" 
biden_sentiment['dataset'] <- "Biden"
merged_data <- rbind(trump_sentiment, biden_sentiment)

trump_sentiment_syuzhet['dataset'] <- "Trump" 
biden_sentiment_syuzhet['dataset'] <- "Biden"
merged_data_syuzhet <- rbind(trump_sentiment_syuzhet, biden_sentiment_syuzhet)

### method = "bing"
ggplot(merged_data, aes(x = minute, y = avg_sentiment, color = dataset)) +
  geom_smooth() +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiment Analysis Timeline") +
  scale_color_manual(values = c("Trump" = "red", "Biden" = "blue"))
ggsave('plots/both_smooth.png', last_plot(), device = "png")

### method = "syuzhet"
ggplot(merged_data_syuzhet, aes(x = minute, y = avg_sentiment, color = dataset)) +
  geom_smooth() +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiment Analysis Timeline") +
  scale_color_manual(values = c("Trump" = "red", "Biden" = "blue"))
ggsave('plots/both_syuzhet_smooth.png', last_plot(), device = "png")

