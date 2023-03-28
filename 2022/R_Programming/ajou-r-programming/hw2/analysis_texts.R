setwd('~/RProjects/ajou-r-programming/hw2/')
source('fetch-ajou-wish.R')

if (!require('tidyverse')) {
  install.packages('tidyverse')
  library(tidyverse)
}
if (!require('tidytext')) {
  install.packages('tidytext')
  library(tidytext)
}
if (!require('devtools')) {
  install.packages('devtools')
  library(devtools)
}
if (!require('RcppMeCab')) {
  install_github("junhewk/RcppMeCab")
  library(RcppMeCab)
}

ajou_wish_articles = fetch_ajou_wish_articles(ajou_ac_kr_cookie = 'kr-30min=Y; kr-day=Y; kr-month=Y; _ga=GA1.3.1891144128.1667481229; _ga_1WWMRKJP34=GS1.1.1667646205.5.1.1667646211.0.0.0; _ga_B05H7FL8XH=GS1.1.1667646205.5.1.1667646211.0.0.0; _gid=GA1.3.374649084.1667591009; locale=ko; JSESSIONID=135B642F52AC2A099845FE181EB442B3; _gat_gtag_UA_160627624_1=1; USER_KEY=ea111ce3bc443456337989b98a6df34b; apt.uid=AP-PQQY5YJEHTTA-2-1667380394538-49237655.0.2.4999d9c2-2044-47fa-a83c-9b66affa2bd1; JSESSIONID=f3SRGDz5XdH61JBRTFyWHyQetUxsJ1vjxIAz6C1E6M9bPOmlbdhAoBc62zU1vDTY.yonam_servlet_IPSI02; PHAROSVISITOR=0000652b01843da04ba678f00a960a7b', 1000)

tokenized = ajou_wish_articles$article_pages %>%
  unnest_tokens(word, content, 'words')
tokenized$tagged = posParallel(tokenized$word)
frequency = table(grep('NNP', unlist(tokenized$tagged), value = T))
frequency = sort(frequency, decreasing = T)
head(frequency, n = 30)
