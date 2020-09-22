#Analysing word and document frequency: tfidf:
# A list of stop words is not a very sophisticated approch to remobve the most common wors in a document.
# So does this mean we can do away with stop words, when using td-idf?

#################################################################
##             Term Frequency in Jane Austin Novels             ##
##################################################################
library(janeaustenr)
library(dplyr)
library(tidytext)



book_words<- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book,word,sort = TRUE)


total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n)) 
  
book_words <- left_join(book_words,total_words)

# the number of times a word appears in a novel divided by the
#total number of terms (words) in that novel. This is exactly what term frequency is.

library(ggplot2)

ggplot(book_words,aes(x= n/total, fill = book))+
  geom_histogram(show.legend = FALSE)+
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")


##################################################################
##                          Zipf's Law                          ##
##################################################################

##--------------------------------------------------------------
##  Zipf’s law states that the frequency that a word appears is
##inversely proportional to its rank.   -
##--------------------------------------------------------------



freq_by_rank<- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+
  ggtitle ("Zipf's Law")


# Binding tf-idf


book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Let’s look at a visualization for these high tf-idf words

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()


# Most of these words are nouns


##--------------------------------------------------------------------------------------------------------------
##                            What measuring tf-idf has done here is show us that                              -
##                           Jane Austen used similar language across her six novels,                          -
##   and what distinguishes one novel from the rest within the collection of her works are the proper nouns,   -
##                                        the names of people and places                                       -
##--------------------------------------------------------------------------------------------------------------



##################################################################
##                   A Corpus of Physics Text                   ##
##################################################################


library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")



physics_words <- physics %>% 
  unnest_tokens(word, text) %>% 
  count(author,word, sort = TRUE)



#Here we see just the raw counts; we need to remember that these documents 
#are all different lengths. Let’s go ahead and calculate tf-idf,

library(forcats)


physics_plot <- physics_words %>% 
  bind_tf_idf(word,author,n) %>% 
  mutate(word = fct_reorder(word,tf_idf)) %>% 
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))



physics_plot %>% 
  group_by(author) %>% 
  top_n(15,tf_idf) %>% 
  ungroup() %>% 
  mutate(word= reorder(word,tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = author))+
  geom_col(legend = FALSE)+
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

#Very interesting indeed. One thing we see here is “k” in the Einstein text?


library(stringr)

physics %>% 
  filter(str_detect(text, "_k_")) %>% 
  select(text)


mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))


physics_words <- anti_join(physics_words,mystopwords, by = "word")



plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, author)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))




ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()