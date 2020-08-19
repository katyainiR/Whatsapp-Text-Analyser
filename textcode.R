
library(readr)
library(SnowballC)
library(tidyverse)
library(tidytext)
library(RColorBrewer)

x = 7 #top n number of participants to be analysed

chat <- read_delim("path", "]", escape_double = FALSE,
                     col_names = FALSE, trim_ws = TRUE) %>%
  select(-X1) %>%
  separate(
    X2, into = c("sender", "said"), sep = ":"
  ) %>%
  filter(!is.na(sender))
  mutate(
    said = str_to_lower(said)
  ) %>%
    mutate(
      sender = fct_lump(sender, x)
    ) 
  

#Alternate text processing strategy. Depends on the type of your text file.
# chat <- read_csv("path", col_names = FALSE) %>%
#   separate(X2, into = c("X3", "X4"), sep = 10) %>%
#   separate(X4, into = c("sender", "said"), sep = ":") %>%
#   select(sender, said) %>%
#   mutate(
#     said = str_to_lower(said)
#   ) %>%
#   filter(!is.na(sender)) %>%
#   mutate(
#     sender = fct_lump(sender, x)
#   ) 


#Most active participant.
chat %>%
  count(sender, sort = T)  %>%
  mutate(sender = fct_reorder(sender,n)) %>%
  ggplot(aes(sender, n, fill=sender))+
  geom_bar(stat = "identity")+ 
  coord_flip()

chart = chat%>%
  unnest_tokens(words, said) %>% 
  #In case you want to club stemmed words.
  #mutate(words = wordStem(words)) %>% 
  filter (!words %in% c(stop_words$word, "omitted", "media", "image", "deleted"), str_detect(words, "^[a-z]"), !is.na(words)) 



# top words that account for around 15% of all the words that were sent.
top_words = chart %>%
  count(words, sort = T) %>%
  mutate(cumu = cumsum(n),
         quartile = cumu/sum(n)) %>%
  filter(quartile <= 0.15)


chart %>%
  filter(words %in% top_words$words) %>%
  group_by(words, sender) %>%
  count() %>%
  ungroup() %>%
  group_by(words) %>%
  mutate(count = sum(n)) %>%
  ungroup() %>%
  mutate(
    words = fct_reorder(words, count)  
  ) %>%
  ggplot(aes(words, n)) +
  geom_bar(stat = "identity", aes(fill = sender)) +
  coord_flip() + 
  scale_fill_manual(values = brewer.pal(x+1, "Spectral")) +
  labs(y = "# of messages sent", x = "Most used words", title = "Top 15% Words categorised by senders", subtitle = "Top words calculated using most frequent words arranged by their percentile.")
  
  
 
