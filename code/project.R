library(readr)
library(dplyr)
library(stringi)
library(tm)
library(tidytext)
library(tidyr)
library(mgsub)
print("successfully load the packages")

args = (commandArgs(trailingOnly=TRUE))
file = args[1]
#if(length(args) == 2){
#  process = as.numeric(args[1])
#  directory = args[2]
#} else {
#  cat('usage: Rscript project.R <process> <directory>\n', file=stderr())
#  stop()
#}

#no <- 0

# Read file ------
#files <- list.files(arg[2])
#files <- list(file)
#for (file in files){
  #no <- no + 1 
  #path <- paste(args[2], file, sep = "/")
  #print(paste(no, "% ... Processing ", file, sep = ""))
  df <- read_tsv(file)
  #df <- read_tsv('archive/amazon_reviews_us_Gift_Card_v1_00.tsv')
  df <- filter(df,verified_purchase=="Y")
  
  # Load "Stop Words" from the tidytext package
  data("stop_words")
  new_stop_words <- stop_words%>% add_row(word = "br", lexicon = "SMART")
  
  df.text <- df %>% select(review_body)
  
  # Encoding
  # Check Encoding and Make it consistent
  stri_enc_mark(df.text$review_body)
  df.text$review_body <- sapply(df.text$review_body,function(row) iconv(row,"latin1","ASCII",sub = " "))
  
  # Lowecase all text
  df.text$review_body <- tolower(df.text$review_body)
  
  # make wasn't=was not, can't=can not, etc..
  df.text$review_body <- gsub("wasn[\u2019']t", "was not", df.text$review_body)
  df.text$review_body <- gsub("won[\u2019']t", "will not", df.text$review_body)
  df.text$review_body <- gsub("can[\u2019']t", "can not", df.text$review_body)
  df.text$review_body <- gsub("didn[\u2019']t", "did not", df.text$review_body)
  df.text$review_body <- gsub("don[\u2019']t", "do not", df.text$review_body)
  df.text$review_body <- gsub("I[\u2019']m", "I am", df.text$review_body)
  df.text$review_body <- gsub("[\u2019']ve", " have", df.text$review_body) 
  df.text$review_body <- gsub("[\u2019|']s", "", df.text$review_body)
  df.text$review_body <- gsub("[\u2019']re", " are", df.text$review_body)
  df.text$review_body <- gsub("[\u2019']ll", " will", df.text$review_body)
  
  # If you view common typos during your analysis, fix them here.
  df.text$review_body<- gsub("canceling", "cancelling", df.text$review_body)
  df.text$review_body <- gsub("cancellation", "cancelling", df.text$review_body)
  
  # Fix Negations
  # Create a list to identify the sentiment shifters in the text
  negation.words <- c("not",
                      "no",
                      "without",
                      "never",
                      "bad",
                      "none",
                      "never",
                      "nobody",
                      "nowhere",
                      "neither",
                      "nothing"
  )
  
  # Run the following to view Shifted sentiments sorted by polarity point
  shifted.words <- df.text %>% 
    unnest_tokens(bigram, review_body, token = "ngrams", n = 2) %>%
    count(bigram, sort = TRUE) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(word1 %in% negation.words & !word2 %in% new_stop_words$word)%>%
    inner_join(get_sentiments("bing"), by = c(word2 = "word")) %>%
    mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
    mutate(score = sentiment * n) %>%
    mutate(word2 = reorder(word2, score))
  
  # Pick the most effective sentiment shifters
  negated.phrases <- c("not worth", 
                       "not noise",
                       "no issues",
                       "no complaints",
                       "not disappoint",
                       "not disappointed",
                       "not cheap",
                       "no regrets"
                       
  )
  
  # Find synonyms for the phrases above to replace
  synonyms <- c("expensive",
                "functional",
                "cool",
                "satisfied",
                "satisfied",
                "satisfied",
                "expensive",
                "satisfied"
  )
  
  # Replace the negations with their synonyms.
  df.text <- mgsub(df.text$review_body, negated.phrases, synonyms) %>%
    dplyr::as_data_frame() %>%
    rename(review_body = value)
  
  
  # if you want to ignore words that are frequent but doesn't help, add them to this list.
  ignore.words <- data_frame(word = c("sound", "bose", "headphones","noise", "soundlink"))
  
  # create the words freq table
  word.freq.table<- df.text %>% 
    unnest_tokens(word, review_body) %>%
    anti_join(new_stop_words) %>%
    anti_join(ignore.words) %>%
    count(word, sort = TRUE)
  
  csvname <- gsub("tsv", "csv", file)
  write.csv(word.freq.table[0:10,], csvname, row.names = FALSE)
#}