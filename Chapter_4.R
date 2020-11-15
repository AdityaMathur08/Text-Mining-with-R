#################################################################
##                          Chapter 4                          ##
#################################################################
## 4.0 N grams and Correlations----

 
# many interesting text analyses are based on the relationships between words, 
#whether examining which words tend to follow others immediately, or that tend to co-occur within the same documents.

## 4.1  Tokenizing by N gram----

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams # Still a tidy Text, a token per row

# 4.1.1 Examining the most common bigrams----
austen_bigrams %>% 
  count(bigram, sort =TRUE)

#Separating the bigrams to weed out the common words
library(tidyr)

bigrams_seperated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2"),sep = " ")

bigram_filtered <- bigrams_seperated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)  

#Q:Why filter and not antijoin?


# New Bigrams count

bigram_counts<- bigram_filtered %>% 
  count(word1, word2, sort = TRUE)

#We can see that names (whether first and last or with a salutation)
#are the most common pairs in Jane Austen books.

#Combining the most frequent bigrams devoid of stop words tidyr's unite function

bigram_united<- bigram_filtered %>% 
  unite(bigram, word1, word2, sep = " ")


austen_books() %>% 
  filter(book == "Sense & Sensibility") %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram , c("word1","word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
  !word2 %in% stop_words$word,
  !word3 %in% stop_words$word) %>% 
  count(word1,word2,word3, sort = TRUE)



# 4.1.2 Analyzing Bigrams----
#finding out the most important streets in the  Jane Austen's books, when we say most important, we are emphasizing on most mentions

bigram_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)



  # Now we are using the stop words removed bigrams to compute TF-IDF, tf-idf output will tell us how important is that bigram
# with respect to the document interpretation.

bigram_plot<- bigram_united %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram,book,n) %>% 
  arrange(desc(tf_idf)) 
 

bigram_plot %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# 4.1.3 Using bigrams to provide context in sentiment analysis----

# When performing sentiment analysis "Context" is very important and bigrams are somewhat
# better in analyzing the words along with context.
##----------------------------------------------------------------------------------------------------------------------------------------------------------------
##  Example: `I am not Happy, and I don't like it`, can be interpreted as a positive document, however the 'not' before the words Happy and Like negates that    -
##----------------------------------------------------------------------------------------------------------------------------------------------------------------


bigrams_seperated %>% 
  filter(word1 == "not") %>% 
  count(word1, word2, sort = TRUE)

#By performing sentiment analysis on the Bigram data, we can examine how often sentiment-asssociated words are
#preceded by "not" or other negative words

AFINN<- get_sentiments("afinn")

# We can examine the most frequent words that were preceded by "not" and were associated with a sentiment

not_words<- bigrams_seperated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, value, sort = TRUE)



library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences = Contibution") +
  coord_flip()


# Not” isn’t the only term that provides some context for the following word.
#We could pick four common words (or more) that negate the subsequent term,



negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_seperated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)


library(forcats)

  
  negated_words %>% 
  mutate(word1 = factor(word1, levels = negation_words)) %>% 
  mutate(contribution = n * value) %>% 
  arrange(desc(abs(n * value)))%>%
  group_by(word1) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  top_n(20,abs(contribution)) %>% 
  ggplot(aes(word2, contribution, fill = contribution > 0))+
  geom_col(show.legend = FALSE)+
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences = Contibution")+
  facet_wrap(~word1, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()


#Q: how do I  reverse the AFINN values of each word that follows a negation?
  
  
  
  
# 4.1.4 Visualizing Network of Bigrams with ggraph ----
  
##################################################################
##         Visualizing a network of Bigrams with ggraph         ##
##################################################################

  library(igraph)
  
  bigram_counts
  
  
# filter for any relatively common combinations
  
  bigram_graph <- bigram_counts %>%
    filter(n > 20) %>%
    graph_from_data_frame()


library(ggraph)
library(grid)

set.seed(2017)

#Fruchterman and Reingold.

# 
# ggraph(bigram_graph,layout = "fr")+
#   geom_edge_link()+
#   geom_node_point() +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#network structure is useful and flexible way to visualize relational tidy data.


# Creating the Functions count bigrams and vizualizing graphs

# 4.1.5 Vizualizing bigrams in other texts----

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
   
}


# the King James version is book 10 on Project Gutenberg:
library(gutenbergr)
kjv <- gutenberg_download(10)


library(stringr)

kjv_bigrams <- kjv %>% 
  count_bigrams()

# filtering out rare combinations, as well as digits

kjv_bigrams %>% filter(n > 40,
                       !str_detect(word1,"\\d"),
                       !str_detect(word2,"\\d")) %>% 
  visualize_bigrams() 


# 4.2  Counting and correlating pairs of words with wider Package----

##################################################################
##  Counting and correlating pairs of words with wider Package  ##
##################################################################

###  The widyr package makes operations such as computing counts and correlations 
# easy, by simplifying the pattern of “widen data, perform an operation, 
# then re-tidy data”  We’ll focus on a set of functions that make pairwise
# comparisons between groups of observations 
#(for example, between documents, or sections of text).   

# 4.2.1 Counting and correlating among sections ----

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

library(widyr)

# One useful function from widyr is the pairwise_count() function. 
# The prefix pairwise_ means it will result in one row for each pair of words
#in the word variable. 

# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

word_pairs %>%
  filter(item1 == "darcy")

# 4.2.2 Pairwise Coreelation ----



# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)



# We can find the most correlated words with Pounds
word_cors %>%
  filter(item1 == "pounds")




# WE can also find correlated words to some interesting words
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()