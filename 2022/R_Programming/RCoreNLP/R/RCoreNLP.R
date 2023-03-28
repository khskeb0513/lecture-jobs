#' Create object to connect CoreNLP Server
#'
#' This function creates a object includes connection values and functinos to request to CoreNLP Server.
#'
#' @param scheme Scheme to connect server
#' @param domain Domain to connect server
#' @param port Port to connect server
#' @return A list of connection
#' @examples
#' # Use default server connection values
#' nlp <- RCoreNLP()
#'
#' # Return request base url
#' nlp$url
#'
#' # Check connection available
#' nlp$healthCheck()
#'
#' # part-of-speech tagger
#' nlp$pos('All human beings are born free and equal in dignity and rights.')
#' @export
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
  parsable_annotators = c('lemma', 'pos')
  tokenize = function(sentences,
                      annotators = parsable_annotators,
                      separator = '/',
                      verbose = F) {
    annotators = parsable_annotators[stringr::str_detect(annotators, parsable_annotators)]
    url = paste0(url, urltools::url_encode(rjson::toJSON(
      list(
        outputFormat = 'json',
        annotators = paste(append(annotators, 'tokenize'),
                           collapse = ',')
      )
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
          combined_value = value$word
          for (annotator in annotators) {
            combined_value = paste0(combined_value, separator, value[[annotator]])
          }
          return(combined_value)
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
    tokenize = tokenize
  ))
}
