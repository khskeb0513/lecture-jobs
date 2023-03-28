
  data.frame(
    text = "
Actor
@DanielNDurant
 is teaming up with
@DeafWest
 to advocate for authentic Deaf representation in arts and entertainment — proving that communication doesn’t have to be a barrier. Learn how tools like #Pixel's Live Caption for Calls help bridge the gap. http://android.com/accessibility"
  ) %>% unnest_sentences(word, text)


tibble(text = "I'm going school.") %>% unnest_tokens(word, text)
tibble(text = "I'm going school.") %>% unnest_ptb(word, text)
tibble(text = "I am going school.") %>% unnest_tokens(word, text)
tibble(text = "I am going school.") %>% unnest_ptb(word, text)

book_words <- austen_books() %>%
  unnest_ptb(word, text)

parts_of_speech %>% filter(str_detect(word, '\\.'))
tibble(word = c('.', 'asdf', 'sdaf.')) %>% filter(str_detect(word, '\\.'))

tibble(text = "I'm Ajou student, in business college.") %>%
  unnest_ptb(word, text)
# unnest_tokens(word, text)

library(RcppMeCab)

tibble(text = c('asdf', 'hi.', '안녕하세요?')) %>%
  unnest_tokens(
    word,
    text,
    token = function(charactar_vector) {
      list(a = c('asdf', 'asdf'),
           'asdf',
           'asdf')
    }
  )

tibble(text = '
       모든 인간은 태어날 때부터 자유로우며 그 존엄과 권리에 있어 동등하다.
       인간은 천부적으로 이성과 양심을 부여받았으며 서로 형제애의 정신으로 행동하여야 한다.
       ') %>%
  unnest_tokens(word,
                text,
                token = RcppMeCab::pos)

pos(c(
  '모든 인간은 태어날 때부터 자유로우며 그 존엄과 권리에 있어 동등하다.',
  '인간은 천부적으로 이성과 양심을 부여받았으며 서로 형제애의 정신으로 행동하여야 한다.'
))
