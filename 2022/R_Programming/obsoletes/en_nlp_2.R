library(readr)
library(tidytext)
library(dplyr)
library(stopwords)
library(RcppMeCab)
library(SnowballC)
library(stringr)
library(coreNLP)

downloadCoreNLP(outputLoc = './coreNLP', type = 'base')
initCoreNLP(libLoc = '/usr/local/Cellar/stanford-corenlp/4.4.0/libexec/', type = 'english')
annotateString('Hi!', format = 'text')

en = tibble(text = read_file('54525-2.txt'))
ko = tibble(text = read_file('54525-3.txt', locale = locale(encoding = 'euc-kr')))

en_tidy = en %>%
  unnest_tokens(text, output = word) %>%
  mutate(pos = wordStem(word)) %>%
  anti_join(get_stopwords()) %>%
  filter(str_detect(word, '\\D')) %>%
  count(word, sort = TRUE)

spacy_parse(en_tidy$word)

en_tidy$book = rep('sangbeop', nrow(en_tidy))


ko_tidy = ko %>% unnest_tokens(text, output = word, token = posParallel)

View(en_tidy %>%
       count(book, word, sort = TRUE) %>%
       bind_tf_idf(word,
                   book,
                   n = n))

without_stopword_ko_tidy = ko_tidy %>% anti_join(get_stopwords('ko', 'marimo'))

tibble(text = text) %>% unnest_tokens(input = text,
                                      output = word,
                                      token = pos(join = F))