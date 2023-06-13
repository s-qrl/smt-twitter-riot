library(dplyr)

full_tweets = read.csv("resources/tweets.csv")  # path to dataset
head(full_tweets)

# no garbage values found
# duplicate ID removal
riot_tweets = full_tweets %>% distinct(tweet_id, .keep_all = TRUE)

# remove text duplicates
riot_tweets = riot_tweets %>% distinct(text, .keep_all = TRUE)

# "trump" data set
riot_tweets_trump <- riot_tweets[grep('trump', riot_tweets$text, ignore.case = TRUE), ]

# "biden" data set
riot_tweets_biden <- riot_tweets[grep('biden', riot_tweets$text, ignore.case = TRUE), ]

# data set that includes both "trump" and "biden"
riot_tweets_both <- intersect(riot_tweets_biden, riot_tweets_trump)

# Write to dataset
write.csv(riot_tweets_trump, file = "resources/riot_tweets_trump.csv", row.names = FALSE)
write.csv(riot_tweets_biden, file = "resources/riot_tweets_biden.csv", row.names = FALSE)
write.csv(riot_tweets_both, file = "resources/riot_tweets_both.csv", row.names = FALSE)
