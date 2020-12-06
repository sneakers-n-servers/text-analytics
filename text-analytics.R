library(here)
library(tidyverse)
library(stringr)

source("text-functions.R")

tarzan.file <- here("data", "TarzanOfTheApes.txt")
tarzan.lines <- book_to_vector(tarzan.file, remove_last = 3)
tarzan.lines <- book_to_chapters(tarzan.lines)

vcorpus <- VCorpus(VectorSource(tarzan.lines))
dtm <- DocumentTermMatrix(vcorpus)
vcorpus
inspect(dtm)
