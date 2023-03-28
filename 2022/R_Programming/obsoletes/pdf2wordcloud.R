library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(readr)
library(RCoreNLP)
library(stopwords)
library(wordcloud2)
library(httr)
library(purrr)
library(jsonlite)

# ================= Retrieve Document Information =================

document_id = 12
warning('MUST CHECK IS DOCUMENT SELECTED CORRECTLY')
unlist(content(GET(
  paste0('http://localhost:1337/api/parsed-documents/', document_id)
))$data$attributes)

# ================= Analyze PDF File =================

local_file_name = 'MW12S2.pdf'

warning('MUST CHECK FILENAME AND ITS CONTENTS ARE CORRECT')
print(local_file_name)
doc = readPDF(control = list(text = "-layout"))(
  elem = list(uri = local_file_name),
  language = "en",
  id = "id1"
)
text_tibble = tibble(text = NLP::content(doc))
nlp = RCoreNLP(domain = '0.tcp.jp.ngrok.io', port = 19776)
text_lemma = text_tibble %>%
  unnest_tokens(
    input = text,
    output = word,
    token = function(col) {
      nlp$tokenize(col, annotators = 'lemma')
    }
  )
text_lemma$word = sapply(strsplit(text_lemma$word, '/'), function(charVector) {
  charVector[2]
})
text_count = text_lemma %>% count(word, sort = T)
text_pos = tibble(text = paste(text_count$word, collapse = ' ')) %>%
  unnest_tokens(
    input = text,
    output = word,
    token = function(col) {
      nlp$tokenize(col, annotators = 'pos', verbose = T)
    }
  )
text_pos$pos = sapply(strsplit(text_pos$word, '/'), function(charVector) {
  charVector[2]
})
text_pos$word = sapply(strsplit(text_pos$word, '/'), function(charVector) {
  charVector[1]
})
text_pos$n = 0
for (i in 1:nrow(text_pos)) {
  if (sum(text_count$word == as.character(text_pos[i, 1])) == 1) {
    text_pos[i, 3] = text_count[text_count$word == as.character(text_pos[i, 1]), 2]
  }
}
text_pos = text_pos %>% filter(n != 0)
text_without_stopwords = text_pos %>%
  anti_join(stop_words)
text_filter_pos = text_without_stopwords %>%
  filter(str_detect(pos, 'nn|vb')) %>%
  filter(str_detect(word, '\\D'))
text_filter_pos$find_count = text_filter_pos$n
text_filter_pos$find_count_normed =
  ((text_filter_pos$n - min(text_filter_pos$n)) / (max(text_filter_pos$n) -
                                                     min(text_filter_pos$n)))
text_filter_pos$n = text_filter_pos$find_count_normed
text_filter_pos = text_filter_pos %>%
  filter(find_count_normed != 0)
warning('MUST CHECK WORDCLOUD IS GENERATED CORRECTLY')
wordcloud2(text_filter_pos[, c(1, 3)])

# ================= Post Analyze Result by Document =================
for (i in 1:nrow(text_filter_pos)) {
  row = text_filter_pos[i,]
  row = c(row, list('parsed_document' = document_id))
  POST(
    'http://localhost:1337/api/document-tokenizeds',
    body = rjson::toJSON(list(data = row)),
    config = add_headers(`content-type` = 'application/json')
  )
}
