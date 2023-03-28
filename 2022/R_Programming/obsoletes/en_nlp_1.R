library(stringr)
library(dplyr)
library(tidytext)
library(stopwords)

load('en_nlp.RData')

en_texts = en_nlp[str_detect(en_nlp$word, "\\[Text="), ]
en_words_vector = en_texts$word
en_words_vector = en_words_vector %>% str_split(' ')
en_words_vector = en_words_vector %>% str_split('=')
en_words_vector = sapply(en_words_vector, "[[", 2)
en_words_vector = str_split(en_words_vector, ' ')
en_words_vector = sapply(en_words_vector, "[[", 1)
en_words_vector = tolower(en_words_vector)

en_only_noun_tibble = tibble(word = en_words_vector)
en_only_noun_tibble %>% anti_join(stop_words)
View(count(en_only_noun_tibble, word, sort = T))
