library(tidyverse)
library(tidytext)
library(SnowballC)

files <- list.files(pattern=".*(tsv)$", 
                    recursive=TRUE,
                    full.names=TRUE)

d <- lapply(files, read_tsv)

# read one file, it should be generalized
data <- d[[1]]

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

write_csv(word_summary, "word_summary.csv")
