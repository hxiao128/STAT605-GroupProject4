library(dplyr)
library(tidytext)
library(SnowballC)

# read one file, it should be generalized
data <- read_tsv('amazon_reviews_us_Apparel_v1_00.tsv')

# for demenstration, take partial data
data2 <- data %>% tail(100)

new_stop_words <- append(stop_words$word, "br")
words <- data2 %>%
  select(review_id, star_rating, review_body) %>%
  unnest_tokens(word, review_body) %>%
  filter(!word %in% new_stop_words, str_detect(word, "^[a-z']+$"))

afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))

reviews.afinn <- words %>%
  inner_join(afinn, by = "word")

word_summary <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(star_rating), score = max(value), count_word = n()) %>%
  arrange(desc(count_word)) %>%
  head(10)
