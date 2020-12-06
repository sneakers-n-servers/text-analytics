library(here)
library(tidyverse)
library(stringr)

source("text-functions.R")

tarzan.file <- here("data", "TarzanOfTheApes.txt")
tarzan.lines <- book_to_vector(tarzan.file, remove_last = 3)
tarzan.chapters <- book_to_chapters(tarzan.lines)
chapter_names <- get_chapter_names(tarzan.lines)

lecuture_functions(tarzan.lines)

tarzan.frame <- data.frame(chapter=chapter_names, text=tarzan.chapters)
str(tarzan.frame)

tarzan.frame %>%
  unnest_tokens(word, text) %>%
  mutate(word_size=nchar(word)) %>%
  arrange(desc(word_size)) %>%
  top_n(5)

tarzan.frame %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_len=nchar(sentence)) %>%
  arrange(desc(sentence_len)) %>%
  top_n(5)

