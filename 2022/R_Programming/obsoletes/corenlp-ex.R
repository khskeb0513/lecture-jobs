library(tidytext)
library(dplyr)
library(stringr)
library(readr)
library(RCoreNLP)
library(stopwords)
library(wordcloud2)
library(tm)

nlp = RCoreNLP(domain = '0.tcp.jp.ngrok.io', port = 19776)
# text_tibble = tibble(text = str_split(read_file('stic.txt'), '\n')[[1]])
text_tibble = tibble(text = read_file('stic_ch17.txt'))
text_pos = text_tibble %>% unnest_tokens(
  input = text,
  output = word,
  token = function(sentences) {
    nlp$pos(sentences, verbose = T)
  }
)
text_pos$pos = sapply(strsplit(text_pos$word, '/'), function(charVector) {
  charVector[2]
})
text_pos$word = sapply(strsplit(text_pos$word, '/'), function(charVector) {
  charVector[1]
})
text_without_stopwords = text_pos %>% anti_join(get_stopwords())
text_detect_pos = text_without_stopwords %>% filter(str_detect(pos, 'vb'))
text_count = text_detect_pos %>% count(word, sort = T)
# text_count = text_count %>% anti_join(tibble(word = c('dawkins')))
text_count
wordcloud2(text_count)
