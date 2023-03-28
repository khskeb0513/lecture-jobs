library(tidyverse)
library(tidytext)

tb = tibble(
  text = c(
    'I have a dream that one day this nation will rise up and live out the true meaning of its creed. We hold these truths to be self-evident that all men are created equal.',
    'I have a dream that one day out in the red hills of Georgia the sons of former slaves and the sons of former slaveowners will be able to sit down together at the table of brotherhood.',
    'I have a dream that one day even the state of Mississippi, a state sweltering with the heat of oppression, will be transformed into an oasis of freedom and justice.',
    'I have a dream that my four little children will one day live in a nation where they will not be judged by the color of their skin but by their character.',
    'I have a dream today.',
    'I have a dream that one day down in Alabama, with its vicious racists, with its governor having his lips dripping with the words of interposition and nullification; that one day right down in Alabama little black boys and black girls will be able to join hands with little white boys and white girls as sisters and brothers.'
  )
)
tb
tb %>% count(text, sort = T)

library(tidytext)

View(
  tb %>%
    mutate(book = 1) %>%
    unnest_tokens(output = word, input = text) %>%
    count(book, word, sort = T) %>%
    bind_tf_idf(word, book, n)
)
View(tidytext::unnest_tokens(
  tibble(
    text = "I have a dream today."
  ),
  word,
  text)
)

tb_1 = tibble(tb, id = 1)

tb %>% tidytext::bind_tf_idf(text, id, n)

library(janeaustenr)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)
