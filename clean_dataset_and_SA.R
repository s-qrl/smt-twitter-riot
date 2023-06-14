library(dplyr)
library(tidyverse)
library(lubridate)
library(syuzhet)
library(ggplot2)

riot_tweets_biden = read.csv("resources/riot_tweets_snscrape_biden.csv")
riot_tweets_trump = read.csv("resources/riot_tweets_snscrape_trump.csv")  

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

# Write to file
write.csv(riot_tweets_trump, file = "resources/riot_tweets_trump.csv", row.names = FALSE)
write.csv(riot_tweets_biden, file = "resources/riot_tweets_biden.csv", row.names = FALSE)

##### Analysis for Biden
########################

# convert timestamp to datetime
riot_tweets_biden$timestamp <- ymd_hms(riot_tweets_biden$date)

# perform SA
riot_tweets_biden$sentiment <- get_sentiment(riot_tweets_biden$content, method = "bing")

# aggregate sentiment scores per minute
biden_sentiment <- riot_tweets_biden %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment))

# visualize
ggplot(biden_sentiment, aes(x = minute, y = avg_sentiment)) +
  geom_line() +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiment Analysis Timeline")


##### Analysis for Trump
########################

# convert timestamp to datetime (there was an issue with one date, so I had to use try-catch)
tryCatch(
  parse_date_time(riot_tweets_trump$date, orders = c("ymd_hms", "dmy_hms", "mdy_hms")),
  error = function(e) e
)

# perform SA
riot_tweets_trump$sentiment <- get_sentiment(riot_tweets_trump$content, method = "bing")

# aggregate sentiment scores per minute
trump_sentiment <- riot_tweets_trump %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment))

# visualize
ggplot(trump_sentiment, aes(x = minute, y = avg_sentiment)) +
  geom_line() +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiment Analysis Timeline")

##### Comparison
########################

trump_sentiment['dataset'] <- "Trump" 
biden_sentiment['dataset'] <- "Biden"
merged_data <- rbind(trump_sentiment, biden_sentiment)

ggplot(merged_data, aes(x = minute, y = avg_sentiment, color = dataset)) +
  geom_line() +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiment Analysis Timeline") +
  scale_color_manual(values = c("Trump" = "red", "Biden" = "blue"))
