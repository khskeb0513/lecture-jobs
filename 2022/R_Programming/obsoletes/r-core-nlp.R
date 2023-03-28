RCoreNLP = function(scheme = 'http',
                    domain = 'localhost',
                    port = 9000) {
  port = as.numeric(port)
  url = paste0(scheme, '://', domain, ':', port, '?properties=')
  healthCheck = function() {
    url = paste0(url, urltools::url_encode(rjson::toJSON(
      list(outputFormat = 'json', annotators = 'tokenize,pos')
    )))
    tryCatch(
      expr = {
        if (sum(names(httr::content(httr::POST(url))) == 'sentences') == 1) {
          return(list(ping = 'pong'))
        } else {
          return(list(ping = NA))
        }
      },
      error = function(e) {
        print.warnings(e)
        return(list(ping = NA))
      }
    )
  }
  pos = function(sentences, verbose = F) {
    url = paste0(url, urltools::url_encode(rjson::toJSON(
      list(outputFormat = 'json', annotators = 'tokenize,pos')
    )))
    results = list()
    acc_count = 1
    for (sentence in sentences) {
      if (verbose) {
        print(paste0('request sentence: ', acc_count, ' / ', length(sentences)))
        acc_count = acc_count + 1
      }
      if (sentence == '') {
        results <- append(results, '')
        next
      }
      response = httr::content(httr::POST(url = url, body = sentence))
      parsed_list = list(unlist(lapply(response$sentences, function(value) {
        return(sapply(value$tokens, function(value) {
          return(paste0(value$word, '/', value$pos))
        }))
      })))
      names(parsed_list) = c(sentence)
      results <- append(results, parsed_list)
    }
    return(results)
  }
  return(list(
    url = url,
    healthCheck = healthCheck,
    pos = pos
  ))
}
