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
library(wordcloud)



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

cust_stop <- data_frame(
  word=c("movie","film","movies"),
  lexicon=rep("custom",each=3) 
)

afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

# Joker_review_frequencies <- df_Joker %>%
#   unnest_tokens(word, text)%>%
#   anti_join(stop_words) %>%
#   anti_join(cust_stop) %>%
#   inner_join(get_sentiments("nrc")) %>%  #pizza flavor word cloud
#   count(word, sentiment, sort=TRUE) %>%
#   acast(word ~sentiment, value.var="n", fill=0) %>%
#   comparison.cloud(colors = c("black", "red","pink","yellow","orange","grey","blue","green"),
#                    max.words=100,
#                    scale = c(0.8,0.8),
#                    fixed.asp=TRUE,   #True将长宽比例固定
#                    title.size=1
#   )


The_Dark_Knight_review_frequencies <- df_The_Dark_Knight %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop)%>%
  inner_join(get_sentiments("nrc")) %>%  #pizza flavor word cloud
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>% 
  comparison.cloud(colors = c("black", "red","pink","yellow","orange","grey","blue","green"),
                   max.words=100,
                   scale = c(0.8,0.8),
                   fixed.asp=TRUE,   #True将长宽比例固定
                   title.size=1
  ) 



