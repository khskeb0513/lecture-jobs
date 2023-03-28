tbl = tibble(document = c("경기1", "경기2", "경기3"),
             text = c("대한민국 우루과이", "대한민국 가나", "대한민국 포르투칼"))
token_tbl = tbl %>%
  unnest_tokens(word, text) %>%
  count(document, word, sort = T) %>%
  bind_tf_idf(word, document, n)
