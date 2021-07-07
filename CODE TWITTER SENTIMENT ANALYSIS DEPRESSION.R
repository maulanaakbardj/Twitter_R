library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(purrr)

#Search for tweets on topics of your choice,
#narrow down the number of desired tweets and decide whether to include retweets or not.
kata <- search_tweets("#Depression", n=1000, include_rts = FALSE)
kata


#Process each set of tweets into a neat text or corpus object.
tweet.Kata = kata %>% select(screen_name, text)
tweet.Kata
head(tweet.Kata$text)


#removing http elements
tweet.Kata$stripped_text1 <- gsub("http\\S+","",tweet.Kata$text)

#use the unnest_tokens() function to convert to lowercase
#remove punctuation, and id for each tweet
tweet.Kata_stem <- tweet.Kata %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)
head(tweet.Kata_stem)

#remove stopwords words from words list
cleaned_tweets.Kata <- tweet.Kata_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Kata)

head(tweet.Kata$text)

#Top 20 words in #Depression tweets
cleaned_tweets.Kata %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y = n)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x="Count",
       y="Unique word",
       tittle="Unique word counts found in #Depression Tweets")


#To do a sentiment analysis using Bing on a depression tweet,
#The following command returns a tibble.
bing_kata = cleaned_tweets.Kata %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()


#visualization of word count,
#You can filter and plot words next to each other to compare
#positive and negative emotions.
bing_kata %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% 
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment))+ 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(title = "Tweets containing '#Depression'", y = "Contribution to sentiment", 
       x = NULL) + coord_flip() + theme_bw()

#Function to get sentiment score for each tweet
sentiment_bing = function(twt){
  twt_tbl = tibble(text = twt) %>%
    mutate(
      stripped_text = gsub("http\\S+","",text)
    )%>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment, sort = TRUE) %>%
    ungroup() %>%
    #create a "score" column, which assigns -1 to all negative words,
    #and 1 for positive words
    mutate(
      score = case_when(
        sentiment == 'negative'~n*(-1),
        sentiment == 'positive'~n*1)
    )
  #calculate the total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, #if there is no word, the score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, the number of positives and negatives
  )
  #to track which tweets contain no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", #Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" #Type 2: zero means word count = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}


#apply function
#The lapply function returns a list of all sentiment scores, types, and tweet tables
kata_sent = lapply(kata$text, function(x){sentiment_bing(x)})   
kata_sent


#make a tibble that determines the word, score and type jenis
kata_sentiment = bind_rows(tibble(kata = '#Depression', 
                                  score = unlist(map(kata_sent, 'score')), 
                                  type = unlist(map(kata_sent, 'type'))))

#we can see some characteristics of sentiment in each group.
#Here is a tweet sentiment histogram.
ggplot(kata_sentiment, aes(x=score, fill = kata)) + 
  geom_histogram(bins = 15, alpha= .6) + facet_grid(~kata) + theme_bw()
















