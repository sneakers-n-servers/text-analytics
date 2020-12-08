library(here)
library(tm)
library(tidyverse)

book_to_vector <- function(path, encoding = "UTF-8", display_count = 10, remove_last = 0){
  stopifnot(file.exists(path))
  # Read file as vector remove empty lines
  book.lines <- readLines(
    tarzan.file, 
    encoding="UTF-8"
  )
  book.lines <- book.lines[which(book.lines != "")]
  
  # Grab content from first chapter to end of book
  start_index <- grep("^chapter", book.lines, ignore.case = TRUE)[1]
  end_index <- grep("^end of project gutenberg's", book.lines, ignore.case = TRUE) - 1 - remove_last
  book.lines <- book.lines[start_index:end_index]
  
  message(sprintf("Displaying first %d lines", display_count))
  print(head(book.lines, display_count))
  message(sprintf("Displaying last %d lines", display_count))
  print(tail(book.lines, display_count))
  book.lines
}

get_chapter_names <- function(book.lines, chapter_regex = "Chapter [IVXLCDM]+"){
  chapter_lines <- grep(chapter_regex, book.lines)
  titles <- chapter_lines + 1
  book.lines[titles]
}

book_to_chapters <- function(book.lines, chapter_regex = "Chapter [IVXLCDM]+"){
  #book.lines <- book.lines
  
  # Get the index of each chapter, the next line is the title
  chapter_lines <- grep(chapter_regex, book.lines)
  titles <- chapter_lines + 1
  # Remove titles
  book.lines <- book.lines[-titles]
  book.lines <- book.lines[-1]
  
  # Split by chapter duplicate white space
  single_line <- paste(book.lines, collapse = ' ')
  chapters <- strsplit(single_line, "Chapter [IVXLCDM]+", fixed = FALSE, perl = TRUE)
  chapters <- lapply(chapters, trimws)[[1]]
  for(i in 1:length(chapters)){
    chapters[i] <- str_replace_all(chapters[i], "[ ]+", " ")
  }
  message("Displaying book by chapters")
  print(chapters)
  chapters
}

lecuture_functions <- function(book.vector){
  vcorpus <- VCorpus(VectorSource(book.vector))
  dtm <- DocumentTermMatrix(vcorpus)
  print(vcorpus)
  print(inspect(dtm))
}

longest <- function(book.frame, n = 10){
  longest.words <- book.frame %>%
    unnest_tokens(word, text) %>%
    mutate(word_size=nchar(word)) %>%
    arrange(desc(word_size)) %>%
    top_n(n)
  
  longest.sentences <- book.frame %>%
    unnest_tokens(sentence, text, token = "sentences") %>%
    mutate(sentence_len=nchar(sentence)) %>%
    arrange(desc(sentence_len)) %>%
    top_n(n)
  
  longest.words
  longest.sentences
}

remove_num_punc<- function(x){
  #"[!\"#$%&'*+,./)(:;<=>?@\][\\^`{|}~]"
  gsub("[^[:alpha:][:space:]]*", "", x)
}

clean_data <- function(book.vector){
  vcorpus <- VCorpus(VectorSource(book.vector))
  lower <- tm_map(vcorpus, content_transformer(tolower))
  cleaned <- tm_map(lower, content_transformer(remove_num_punc))
  
  stopWords <- c(tm::stopwords('english'))
  cleaned_nostop <- tm::tm_map(cleaned, tm::removeWords, stopWords)
  
  cleaned_nostop
}

remove_words_under_len_five <- function(sentence){
  gsub('\\b\\w{1,5}\\s','',sentence)
}

clean_data_over_five <- function(sentences){
  vcorpus <- VCorpus(VectorSource(sentences))
  just_sentences <- vcorpus[["2"]][["content"]]
  just_sentences_over_five <- tm_map(just_sentences, content_transformer(remove_words_under_len_five))
  just_sentences_over_five
}

get_nouns <- function(word){
  filter <- getTermFilter("ExactMatchFilter", word, TRUE)
  nouns <- getIndexTerms("NOUN", 10, filter)
  nouns <- sapply(nouns, getLemma)
  nouns
}

get_verbs <- function(word){
  filter <- getTermFilter("ExactMatchFilter", word, TRUE)
  verbs <- getIndexTerms("VERB", 10, filter)
  verbs <- sapply(verbs, getLemma)
  verbs
}

filter_nouns <- function(words){
  result <- lapply(words, get_nouns)
  result
}

filter_verbs <- function(words){
  result <- lapply(words, get_verbs)
  result
}

get_words<- function(sentence){
  words <- strsplit(sentence, " ")
  words <- unlist(words, recursive = FALSE)
  words
  # freq<-table(words)
  # plot(sort(words.freq,decreasing = TRUE), ylab="Frequency", ylim=c(0,20), 
  # main="Frequency of words")
}
