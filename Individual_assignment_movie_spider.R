library(magrittr)
library(rvest)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(tidyverse)
library(tidyr)



Joker <- xml2::read_html("https://www.imdb.com/title/tt7286456/reviews?ref_=tt_urv") 
Joker_review <- Joker %>%
  html_nodes('.text') %>%
  html_text()
#View(Joker_review) 

The_Dark_Knight <-xml2::read_html("https://www.imdb.com/title/tt0468569/reviews?ref_=tt_urv")
The_Dark_Knight_review <- The_Dark_Knight %>%
  html_nodes('.text') %>%
  html_text()
#View(The_Dark_Knight_review)

df_Joker <- data_frame(id=1:25, text=Joker_review)
#View(df_Joker)

df_The_Dark_Knight <- data_frame(id=1:25, text= The_Dark_Knight_review)
#View(df_The_Dark_Knight)

# cust_stop <- data_frame(
#   word=c("movie","film","movies"),
#   lexicon=rep("custom",each=3) 
# )

# data(stop_words)
# afinn <- get_sentiments("afinn")
# nrc <- get_sentiments("nrc")
# bing <- get_sentiments("bing")

bigrams_separated_Joker <- df_Joker %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_separated_Knight <- df_The_Dark_Knight %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_counts_Joker <- bigrams_separated_Joker %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

bigram_counts_Knight <- bigrams_separated_Knight %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

# Joker_review_frequencies <- df_Joker %>%
#   unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
#   separate(bigram, c("word1", "word2"), sep = " ") %>%
#   filter(!word1 %in% stop_words$word) %>%
#   filter(!word2 %in% stop_words$word) %>%
#   unite(bigram, word1, word2, sep=" ") %>%
#   count(id, bigram) %>%
#   bind_tf_idf(bigram, id, n) %>%
#   arrange(desc(tf_idf))
  
# The_Dark_Knight_review_frequencies <- df_The_Dark_Knight %>%
#   unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
#   separate(bigram, c("word1", "word2"), sep = " ") %>%
#   filter(!word1 %in% stop_words$word) %>%
#   filter(!word2 %in% stop_words$word) %>%
#   unite(bigram, word1, word2, sep=" ") %>%
#   count(id, bigram) %>%
#   bind_tf_idf(bigram, id, n) %>%
#   arrange(desc(tf_idf)) 

negation_tokens <- c("no","never","without","not")

negated_words_Joker <- bigrams_separated_Joker %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words_Knight <- bigrams_separated_Knight %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

library(igraph)
#install.packages("ggraph")
library(ggraph)
bigram_graph <- bind_rows(bigrams_separated_Knight,
                          bigrams_separated_Joker)%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n>1) %>%
  graph_from_data_frame ()

bigram_graph

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
