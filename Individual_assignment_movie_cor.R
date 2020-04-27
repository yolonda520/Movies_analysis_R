library(magrittr)
library(rvest)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)
library(scales)

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
  word=c("movie","film"),
  lexicon=rep("custom",each=2) 
)

Joker_review_frequencies <- df_Joker %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop)
  

The_Dark_Knight_review_frequencies <- df_The_Dark_Knight %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) 

frequency <- bind_rows(mutate(Joker_review_frequencies, movie="Joker"),
                     mutate(The_Dark_Knight_review_frequencies, movie="Knight")
                     )  %>%  #closing bind_rows 
  mutate(word=str_extract(word, "[a-z']+")) %>% 
  count(movie, word) %>%   
  group_by(movie) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(movie, proportion) %>%
  gather(movie, proportion, `Joker`)

cor.test(data=frequency[frequency$movie == 'Joker',],
         ~proportion + `Knight`) 

ggplot(frequency, aes(x=proportion, y=`Knight`, 
                      color = abs(`Knight`- proportion))) +
  geom_abline(color="grey40", lty=2) +
  geom_jitter(alpha=.1, size=5.5, width=0.3, height=0.3) +
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.0001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~movie, ncol=2) +
  theme(legend.position = "none") +
  labs(y="Knight", x=NULL)

