library(here)
library(tm)
#library(tidyverse)

book_to_vector <- function(path, encoding = "UTF-8", display_count = 10, remove_last = 0){
  stopifnot(file.exists(path))
  # Read file as vector remove empty lines
  book.lines <- readLines(
    tarzan.file, 
    encoding="UTF-8"
  )
  book.lines <- book.lines[which(book.lines != "")]
  
  # Grab content from first chapter to end of book
  start_index <- grep("^chapter", book.lines, ignore.case = TRUE)[1] + 1
  end_index <- grep("^end of project gutenberg's", book.lines, ignore.case = TRUE) - 1 - remove_last
  book.lines <- book.lines[start_index:end_index]
  
  message(sprintf("Displaying first %d lines", display_count))
  print(head(book.lines, display_count))
  message(sprintf("Displaying last %d lines", display_count))
  print(tail(book.lines, display_count))
  book.lines
}

book_to_chapters <- function(book.lines){
  # Get the index of each chapter, the next line is the title
  chapter_lines <- grep("Chapter [IVXLCDM]+", book.lines)
  titles <- chapter_lines + 1
  # Remove titles
  book.lines <- book.lines[-titles]
  book.lines <- book.lines[-1]
  
  single_line <- paste(book.lines, collapse = ' ')
  chapters <- strsplit(single_line, "Chapter [IVXLCDM]+", fixed = FALSE, perl = TRUE)
  chapters <- lapply(chapters, trimws)[[1]]
  message("Displaying book by chapters")
  print(chapters)
  chapters
}

lecuture_functions <- function(book.vector){
  vcorpus <- VCorpus(VectorSource(book.vector))
}