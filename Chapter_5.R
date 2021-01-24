
##################################################################
##     Chapter 5. Converting to and from non-tidy formats ----     
##################################################################
#Tidy text format: a table with one-token-per-document-per-row, 
#such as is constructed by the unnest_tokens() function.
library(tidytext)
library(dplyr)
library(ggplot2)
# There are many packages used in R for Text analysis like quanteda & tm whose input and
# output may not necessarily be in Tidy format.hence in this chapter we are dealing with
#converting back and forth between Tidy Data format(one token per document row ) and 
# non tidy data structures(Ex. tidying Documenr Term Matrix and casting a tidy dataframe into Sparse Matrix)

# Tidying a Document Term Matrix----
# DTMs are usually implemented as sparse matrices---> Row corresponds to document and Columns are the text and the value is the frequency of the word in column

#DTM cannot be directly used with tidy tools(dplyr,tidy text, ggplot2) and Tidy tables cant be used
# for many computational text analysis, hence we have 2 functions tidy() and cast() for this conversion 
#to and from tidy text and DTM/corpus datastructures

#tidy() turns a document-term matrix into a tidy data frame. 
#cast() turns a tidy one-term-per-row data frame into a matrix. 

library(tm)




