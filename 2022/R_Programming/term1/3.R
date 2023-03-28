View(tidytext::nma_words %>%
  group_by(modifier) %>%
  summarise(head = head(word, 3)))

mylist = list()

parts_of_speech %>%
  filter(pos == '')



tidytext::stop_words %>%
  group_by(lexicon) %>%
  summarise(head = head(word, 3))

tidytext::stop_words %>%
  group_by(lexicon) %>%
  summarise(n = n())




tidytext::sentiments %>%
       group_by(sentiment) %>%
       summarise(head = head(word))

tidytext::sentiments %>%
  group_by(sentiment) %>%
  summarise(n = n())

tidytext::parts_of_speech
tidytext::parts_of_speech %>%
  group_by(pos) %>%
  summarise(n = n())
tidytext::parts_of_speech %>%
  filter()

str_match(parts_of_speech$pos, '.')
