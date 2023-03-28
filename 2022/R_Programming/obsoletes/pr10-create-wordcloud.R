library(wordcloud2)
library(RcppMeCab)
library(tidytext)
library(dplyr)
library(stopwords)
library(openxlsx)
library(stringr)

panre_df = read.xlsx("panre_df.xlsx")

set.seed(1234)
panre_pos = panre_df %>%
  unnest_tokens(word, panre_text, token = posParallel) %>%
  filter(str_detect(word, '/nnp')) # 고유명사
  # filter(str_detect(word, '/nng')) # 일반명사
  # filter(str_detect(word, '/nng|/nnp')) # 일반명사와 고유명사

panre_pos$word = sapply(str_split(panre_pos$word, '/'), "[[", 1)

panre_pos = panre_pos %>%
  anti_join(tibble(word = stopwords('ko', 'marimo'))) %>% # 패키지 활용 불용어 삭제
  anti_join(tibble(word = c(
    '경우', '원고', '피고', '사건', '판결', '항', '법', '위', '기준', '법원'
  ))) %>% # 직접 불용어 삭제
  count(word, sort = T)

panre_pos %>% wordcloud2(shape = 'cardioid')
