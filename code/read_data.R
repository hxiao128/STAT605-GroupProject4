library(tidyverse)

files <- list.files(pattern=".*(tsv)$", 
                    recursive=TRUE,
                    full.names=TRUE)

d <- lapply(files, read_tsv)
