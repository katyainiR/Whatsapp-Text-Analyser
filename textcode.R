
library(readr)
library(SnowballC)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(ggrepel)


path = "C:/Users/user/Desktop/_chat.txt"

chat <- read_delim(path, "]", escape_double = FALSE,
                     col_names = FALSE, trim_ws = TRUE) %>%
  select(-X1) %>%
  separate(
    X2, into = c("sender", "said"), sep = ":"
  ) %>%
  filter(!is.na(sender))
  mutate(
    sender = str_squish(sender),
    said = str_to_lower(said)
  ) %>%
    mutate(
      sender = fct_lump(sender, n)
    ) 
  

#Alternate text processor. Depends on the type of your text file.
# chat <- read_csv(path, col_names = FALSE) %>%
#   separate(X2, into = c("X3", "X4"), sep = 10) %>%
#   separate(X4, into = c("sender", "said"), sep = ":") %>%
#   select(sender, said) %>%
#   mutate(
#     said = str_to_lower(said),
#     sender = str_squish(sender)
#   ) %>%
#   filter(!is.na(sender)) %>%
#   mutate(
#     sender = fct_lump(sender, n)
#   )


n = 7
#MOst active participant.
q = chat %>%
  count(sender, sort = T)  %>%
  mutate(sender = fct_reorder(sender,n)) %>%
  ggplot(aes(sender, n, fill=sender))+
  geom_bar(stat = "identity")+ 
  coord_flip() + 
  labs(x = "Sender", y = "# of messages sent", title = "Most active participants in the group") + 
  scale_fill_manual(values = brewer.pal(8, "Spectral")) 


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


data = chart %>%
  filter(words %in% top_words$words) %>%
  group_by(words, sender) %>%
  count() %>%
  ungroup() %>%
  group_by(words) %>%
  mutate(count = sum(n),
         pct = (round(n/count,2))) %>%
  arrange(count, desc(sender)) %>%
  mutate(
    cumu = cumsum(n),
    y.pos = lag(cumu, default = 0) + n*0.5 
  ) %>%
  ungroup() %>%
  mutate(
    words = fct_reorder(words, count)
  )

# filtering data for considerably large percentage points.
filt = data %>%
  filter(pct>0.15)

g = ggplot(NULL, aes(words, n)) +
    geom_bar(data = data, stat = "identity", aes(fill = sender)) +
    coord_flip() + 
    geom_text(data = filt, aes(x = words, label = scales::percent(pct), y = y.pos)) + 
    scale_fill_manual(values = brewer.pal(8, "Spectral")) +
    labs(y = "# of messages sent", x = "Most used words", title = "Top 15% Words categorised by senders", subtitle = "Top words calculated using most frequent words arranged by their percentile.")


print(q)
print(g)




