##----------------------------------------------------------------
##                      Text Mining with R                       -
##----------------------------------------------------------------
#################################################################
##                          Chapter 1                          ##
#################################################################
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(line_number = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>% 
  ungroup()

original_books


library(tidytext)

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)


tidy_books %>%
  count(word, sort = TRUE)

library(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#################################################################
##                     GutenburgR Package                      ##
#################################################################

library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# Checking which were the most common words HG Wells used in these Novels
tidy_hgwells %>% 
  count(word, sort = TRUE)

#Plotting the words
tidy_hgwells %>%
  count(word, sort = TRUE) %>%
  filter(n > 150) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

bronte %>% head(10)

tidy_bronte <- bronte %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

#What are the most common words in these novels of the Bronte sisters

tidy_bronte %>% 
  count(word, sort = TRUE)

# Interesting that “time”, “eyes”, and “hand” are in the top 10 for both H.G. Wells and the Brontë sisters.

####################################################################################################################################################################################################################################################################################################################
##  Now, let’s calculate the frequency for each word for the works of Jane Austen, the Brontë sisters, and H.G.
##  Wells by binding the data frames together. We can use spread and gather from tidyr to reshape our dataframe 
##  so that it is just what we need for plotting and comparing the three sets of novels.  ##
####################################################################################################################################################################################################################################################################################################################

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "HG Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author,word) %>% 
  group_by(author) %>% 
  mutate(proportion = n / sum(n) ) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, "Bronte Sisters","HG Wells")

# Plotting the data

library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)




#Let’s quantify how similar and different these sets of word frequencies 
#are using a correlation test. How correlated are the word frequencies between
#Austen and the Brontë sisters, and between Austen and Wells?



cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)


cor.test(data = frequency[frequency$author == "HG Wells",],
         ~ proportion + `Jane Austen`)

#Just as we saw in the plots, the word frequencies are 
#more correlated between the Austen and Brontë novels than between Austen and H.G. Wells.
