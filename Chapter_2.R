##---------------------------------------------------------------
##                          Chapter 2                           -
##---------------------------------------------------------------



#################################################################
##              Sentiment Analysis with tidy data              ##
#################################################################

library(tidytext)
# 3 lexicons for sentiment analysis: nrc, bing, afinn.

get_sentiments("afinn")

get_sentiments("nrc") #NRC can be better used as an add on to text analysis that if we analyze
                      # a text to be positive/negative, then what sentiment as a whole does the text convey?

get_sentiments("bing")

#get_sentiments(lexicon = c("bing", "afinn", "loughran", "nrc"))

#NOTE: these lexicons are for unigrams: i.e single words.


library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",ignore_case = TRUE)))) %>% 
  ungroup() %>% 
  unnest_tokens(word,text)


nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_books %>% 
  filter(book == "Emma") %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = TRUE)


library(tidyr)

jane_austen_sentiment <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80,sentiment) %>% 
  spread(sentiment, n, fill = 0 ) %>% 
  mutate(sentiment = positive - negative)

#Now we can plot these sentiment scores across the plot trajectory of each novel. 
#Notice that we are plotting against the index on the x-axis that keeps track of narrative time in sections of text

library(ggplot2)

ggplot(jane_austen_sentiment,aes(x = index, y =sentiment, fill = book))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~book, ncol = 2, scales = "free_x")

##################################################################
##          Comparing the three sentiment dictionaries          ##
##################################################################


##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##  Let’s use all three sentiment lexicons and examine how the sentiment changes across the narrative arc of Pride and Prejudice. 
##  First, let’s use filter() to choose only the words from the one novel we are interested in.   -
##                                                                                                                                                                                                                        -
##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "Afinn")
  

bing_and_nrc <- bind_rows(pride_prejudice %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          pride_prejudice %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


# Why is, for example, the result for the NRC lexicon biased so high
# in sentiment compared to the Bing et al. result? Let’s look briefly
# at how many positive and negative words are in these lexicons.


get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)
#################################################################
##           Most Common Positive and Negative Words           ##
#################################################################

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()



bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


##################################################################
##                          word cloud                          ##
##################################################################

library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))



library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment,value.var =  "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)



##---------------------------------------------------------------
##                  Using Sentences as Tokens                   -
##---------------------------------------------------------------


PandP_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())



bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()