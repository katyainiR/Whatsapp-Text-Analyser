
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
# chat <- read_csv("C:/Users/user/Desktop/_chat.txt", col_names = FALSE) %>%
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

# Most frequently used words.
chart %>%
  add_count(words) %>%
  filter(n>=quantile(n,0.85)) %>%
  mutate( words = fct_reorder(words, n))%>% 
  ggplot(aes(words, n, fill = sender)) +
  geom_col(show.legend = F) + 
  coord_flip() + 
  scale_fill_manual(values = brewer.pal(x+1, "Spectral")) + #In case you see error in this section, use a different colour scale or lower your x.
  labs(y = "# of messages sent", x = "Most used words", title = "Categorizing top 15% used words in chat by senders", subtitle = "....")
  
 
