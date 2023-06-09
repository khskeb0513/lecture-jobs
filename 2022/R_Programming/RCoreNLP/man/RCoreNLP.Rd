% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RCoreNLP.R
\name{RCoreNLP}
\alias{RCoreNLP}
\title{Create object to connect CoreNLP Server}
\usage{
RCoreNLP(scheme = "http", domain = "localhost", port = 9000)
}
\arguments{
\item{scheme}{Scheme to connect server}

\item{domain}{Domain to connect server}

\item{port}{Port to connect server}
}
\value{
A list
}
\description{
This function creates a object includes connection values and functinos to request to CoreNLP Server.
}
\examples{
# Use default server connection values
nlp <- RCoreNLP()

# Return request base url
nlp$url

# Check connection available
nlp$healthCheck()

# part-of-speech tagger
nlp$pos('All human beings are born free and equal in dignity and rights.')
nlp$pos(
    c('All human beings are born free and equal in dignity and rights.')
)
}
