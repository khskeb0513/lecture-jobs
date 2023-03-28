if (!require('purrr')) {
  install.packages('purrr')
  library(purrr)
}
if (!require('xml2')) {
  install.packages('xml2')
  library(xml2)
}
if (!require('rvest')) {
  install.packages('rvest')
  library(rvest)
}
if (!require('httr')) {
  install.packages('httr')
  library(httr)
}

fetch_ajou_wish_articles = function(ajou_ac_kr_cookie, fetch_count = 10) {
  wish_base_url = 'https://www.ajou.ac.kr/kr/ajou/wish.do'
  wish_webpage = read_html(paste0(wish_base_url, '?mode=list&&articleLimit=', fetch_count))
  article_response = NULL
  article_pages = rvest::html_attr(html_nodes(wish_webpage, '.b-title-box > a'), 'href') %>% reduce(function(previousValue, currentValue) {
    if (nrow(previousValue) %% 10 == 0) {
      print(nrow(previousValue))
    }
    handle_reset(wish_base_url)
    article_response <<- GET(paste0(wish_base_url, currentValue),
                             add_headers(Cookie = ajou_ac_kr_cookie))
    if (status_code(article_response) > 399) {
      stop('unauthorized cookie')
    }
    article_webpage = read_html(content(article_response,
                                        as = 'raw'))
    rbind(
      previousValue,
      list(
        title = html_text(html_node(article_webpage, '.b-title')),
        author = html_text(html_node(article_webpage, '.b-writer-box')),
        date = html_text(html_node(article_webpage, '.b-date-box')),
        hit = as.numeric(html_text(
          html_node(article_webpage, '.b-hit-box')
        )),
        content = html_text(html_node(article_webpage, '.b-content-box'))
      )
    )
  }, .init = data.frame())
  return(list(
    article_pages = article_pages,
    last_article_response = article_response
  ))
}