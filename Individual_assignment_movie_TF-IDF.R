library(magrittr)
library(rvest)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(dplyr)
library(tidyverse)



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

df_Joker <- tibble(id=1:25, text=Joker_review)
#View(df_Joker)

df_The_Dark_Knight <- tibble(id=1:25, text= The_Dark_Knight_review)
#View(df_The_Dark_Knight)

cust_stop <- data_frame(
  word=c("movie","film","movies"),
  lexicon=rep("custom",each=3) 
)

Joker_review_frequencies <- df_Joker %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE)
  

The_Dark_Knight_review_frequencies <- df_The_Dark_Knight %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE) 

df <- bind_rows(
  mutate(Joker_review_frequencies,movie="Joker"),
  mutate(The_Dark_Knight_review_frequencies,movie="The Dark Knight")
)

movies_words <- df %>%
  bind_tf_idf(word, movie, n) #book is location info

movies_words # we get all the zeors because we are looking at stop words ... too common
tail(movies_words)

movies_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
movies_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(movie) %>%
  top_n(12) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=movie))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~movie, ncol=2, scales="free")+
  coord_flip()


