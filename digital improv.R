library(tidytext)
library(tm)
library(ggrepel)
library(wordcloud2)

#KT

kt %>% 
  select(one_improv_translate) %>% 
  drop_na()

kt %>% 
  select(id, one_improv_translate) %>% 
  drop_na() %>% 
  unnest_tokens(word, one_improv_translate) %>% 
  anti_join(stop_words) %>% 
  mutate(word = stemDocument(word)) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(word = recode(word, "advertis" = "ad")) %>% 
  group_by(word) %>%
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(n >= 2) %>% 
  wordcloud2(maxRotation = 0, minRotation = 0, fontFamily = "sans-serif", color = google_cols2)


  correio %>% 
    select(one_improv_translate) %>% 
    drop_na()

correio %>% 
  select(id, one_improv_translate) %>% 
  drop_na() %>% 
  unnest_tokens(word, one_improv_translate) %>% 
  anti_join(stop_words) %>% 
  mutate(word = stemDocument(word)) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(word = recode(word, "advertis" = "ad")) %>% 
  group_by(word) %>%
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(n >= 2) %>% 
  wordcloud2(maxRotation = 0, minRotation = 0, fontFamily = "sans-serif", color = google_cols2)


ara %>% 
  select(id, one_improv_translate) %>% 
  drop_na() %>% 
  unnest_tokens(word, one_improv_translate) %>% 
  anti_join(stop_words) %>% 
  mutate(word = stemDocument(word)) %>% 
  distinct(id, .keep_all = T) %>% 
  group_by(word) %>%
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(n >= 2) %>% 
  wordcloud2(maxRotation = 0, minRotation = 0, fontFamily = "sans-serif", color = google_cols2)


sud %>% 
  select(id, one_improv_translate) %>% 
  drop_na() %>% 
  unnest_tokens(word, one_improv_translate) %>% 
  anti_join(stop_words) %>% 
  mutate(word = stemDocument(word)) %>% 
  distinct(id, .keep_all = T) %>% 
  group_by(word) %>%
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(n >= 2) %>% 
  wordcloud2(maxRotation = 0, minRotation = 0, fontFamily = "sans-serif", color = google_cols2)

