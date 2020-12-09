library(here)
library(tidyverse)
library(tidytext)
library(stringr)

library(textreuse)
# Sys.setenv(WNHOME = "/Library/Frameworks/R.framework/Versions/4.0/Resources/library/wordnet")
library(wordnet)
library(zipfR)
library(corpustools)
library(quanteda)

source("text-functions.R")

tarzan.file <- here("data", "TarzanOfTheApes.txt")
tarzan.lines <- book_to_vector(tarzan.file, remove_last = 3)
tarzan.chapters <- book_to_chapters(tarzan.lines)
chapter_names <- get_chapter_names(tarzan.lines)

lecuture_functions(tarzan.lines)

tarzan.frame <- data.frame(chapter=chapter_names, text=tarzan.chapters)
str(tarzan.frame)

#10 longest words and sentences
longest_words <- tarzan.frame %>%
  unnest_tokens(word, text) %>%
  mutate(word_size=nchar(word)) %>%
  arrange(desc(word_size)) %>%
  top_n(10)

longest_sentences <- tarzan.frame %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_len=nchar(sentence)) %>%
  arrange(desc(sentence_len)) %>%
  top_n(10)

longest_words
longest_sentences

#clean data --> filter out stop words, remove numbers and punctuation, and (possibly) removing sparse words
cleaned_data <- clean_data(tarzan.lines)
cleaned_data

#Dendrogram
cleaned_data_tdm <- tm::TermDocumentMatrix(cleaned_data)
freqTerms <- tm::findFreqTerms(cleaned_data_tdm)

cleaned_df <- as.data.frame(cleaned_data_tdm[[1]])
cleaned_dist <- dist(cleaned_df)
dendrogram <- hclust(cleaned_dist, method="ward.D2")

#WordNet to mark the parts of speech for the 10 longest sentences 
#found in part b for nouns and verbs having a length of 5 or greater.
source("text-functions.R")
vcorpus <- VCorpus(VectorSource(longest_sentences))
just_sentences <- list(vcorpus[["2"]][["content"]])
just_sentences_over_five <- lapply(just_sentences, remove_words_under_len_five)
just_sentences_over_five

#Get list of all words over length 5
words_over_five <- lapply(just_sentences_over_five, get_words)
words_over_five

#get all nouns
result <- lapply(words_over_five, filter_nouns)

#remove nulls and compress
nouns <- unlist(result, recursive = FALSE)
nouns[sapply(nouns, is.list)] <- NULL
nouns

#and verbs
result <- lapply(words_over_five, filter_verbs)
verbs <- unlist(result, recursive = FALSE)
verbs[sapply(verbs, is.list)] <- NULL
verbs


#Analyze word frequency using functions from package zipfR.
all_words <- lapply(just_sentences, get_words)
all_words

tdmblog <- TermDocumentMatrix(cleaned_data, control = list(removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE))
dtmblog <- DocumentTermMatrix(cleaned_data)
m <- as.matrix(tdmblog)
v <- sort(rowSums(m), decreasing=TRUE)
freq <- sort(colSums(as.matrix(dtmblog)), decreasing=TRUE)   
wfblog <- data.frame(word=names(freq), freq=freq)

#Do analysis on frequencies
wfblog <- na.omit(wfblog)
summary(wfblog)

wfblog_table <- table(wfblog$freq)
length(wfblog$freq)
wfblog$word
barplot(wfblog$freq, names.arg = wfblog$word, main = "Frequency of Words", xlab = "Word", ylab = "# Times used",)


## zipfr work
wfblog_list <- data.frame(as.list(wfblog))

numeric_word_data <- data.matrix(wfblog$word)
numeric_word_data

indexs <- seq(from = 1, to = length(numeric_word_data), by = 1)
wfblog_tf <- tfl(wfblog$freq, k=indexs)
wfblog_spc <- tfl2spc(wfblog_tf)

# compute Zipf-Mandelbrot model from data and look at model summary
zm <- lnre("zm", wfblog_spc)
zm

## plot observed and expected spectrum
#TODO: Add words to numbers
zm.spc <- lnre.spc(zm,N(wfblog_spc))
plot(wfblog_spc, zm.spc, xlab="Most common words", ylab="Frequency",
     ylim=c(0,4500))
legend(27,16000,c("Observed Frequency", "Expected Frequency"),
       col=c("black", "red"),pch= 15,box.col="white", cex=1)

#TODO: Another zipfr visualization?



#Only do for words of length 6
vcorpus <- VCorpus(VectorSource(longest_sentences))
just_sentences <- list(vcorpus[["2"]][["content"]])
just_sentences_over_six <- lapply(just_sentences, remove_words_under_len_six)
just_sentences_over_six

#Get list of all words over length 5
words_over_six <- lapply(just_sentences_over_six, get_words)
words_over_six

#Generate bigrams and trigrams for all words whose length is greater than 6 characters in the 10 longest sentences
bigrams <- words_over_six %>%
  unnest_tokens(bigram, words_over_six, token = "ngrams", n = 2) 

trigrams <- words_over_six %>%
  unnest_tokens(trigram, words_over_six, token = "ngrams", n = 3) 

bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE)

trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE)

# bigrams
# trigrams
# bigram_counts
# trigram_counts

#Process the text from the document using quanteda 
#Describe the methods you use, the results you get, and what you understand about the theme of the book.
dfm_inaug <- corpus(tarzan.lines) %>% 
  dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 10, verbose = FALSE)

set.seed(100)
textplot_wordcloud(dfm_inaug)


#Process the text from the document using corpustools
tc = create_tcorpus(tarzan.lines, doc_column = 'doc_id', text_columns='tokens')
tc$preprocess(use_stemming = T, remove_stopwords=T)
tc$tokens

#search for certain terms
dfm = get_dfm(tc, 'feature')
hits = search_features(tc, query = c('Savage# savage*','Apes# apes*', 'Man# man*', 'Jungle# jungle*', 'Good# good*', 'Bad# bad*'))
summary(hits)

#get relationships between words
g = semnet(hits, measure = 'con_prob')
igraph::get.adjacency(g, attr = 'weight')
plot(hits)


#TODO: Not sure what to do with this
# Process the text from the document using stringi
library(stringi)




