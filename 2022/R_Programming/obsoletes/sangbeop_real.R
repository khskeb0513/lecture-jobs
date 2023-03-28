library(tidytext)
library(stopwords)
library(RcppMeCab)
library(dplyr)

tidy_c1 = c1 %>% unnest_tokens(word, text, token = pos)
tidy_c1$word = sapply(strsplit(tidy_c1$word, '/'), function(charVector) {
  charVector[1]
})
stop_words_ko = tibble(word = stopwords('ko', 'marimo'))
without_stopwords_tidy_c1 = tidy_c1 %>% anti_join(stop_words_ko) %>% unnest_tokens(pos_word, word, token = pos)
only_noun_tidy_c1 = without_stopwords_tidy_c1[grep('nnp', without_stopwords_tidy_c1$pos_word),]
tf_idf_tidy_c1 = only_noun_tidy_c1 %>% count(book, pos_word, sort = T) %>% bind_tf_idf(pos_word, book, n)
