library(dplyr)
library(readr)

toFormedChars = function(filename, title) {
  contents = paste0(str_split(read_file(filename), ' ')[[1]], ' ')
  splited_contents = c('')
  for (value in contents) {
    if (length(strsplit(splited_contents[length(splited_contents)], '')[[1]]) + length(strsplit(value, '')) > 30) {
      splited_contents = append(splited_contents, value)
    } else {
      splited_contents[length(splited_contents)] = paste0(splited_contents[length(splited_contents)], value)
    }
  }
  return(tibble(text = splited_contents, book = rep(title, length(splited_contents))))
}