% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vecTxt.R
\name{vecTxt}
\alias{vecTxt}
\alias{vecTxtQ}
\title{Easily parse a vector into a character value}
\usage{
vecTxt(
  vector,
  delimiter = ", ",
  useQuote = "",
  firstDelimiter = NULL,
  lastDelimiter = " & ",
  firstElements = 0,
  lastElements = 1,
  lastHasPrecedence = TRUE
)

vecTxtQ(vector, useQuote = "'", ...)
}
\arguments{
\item{vector}{The vector to process.}

\item{delimiter, firstDelimiter, lastDelimiter}{The delimiters
to use for respectively the middle, first
\code{firstElements}, and last \code{lastElements} elements.}

\item{useQuote}{This character string is pre- and appended to all elements;
so use this to quote all elements (\code{useQuote="'"}), doublequote all
elements (\code{useQuote='"'}), or anything else (e.g. \code{useQuote='|'}).
The only difference between \code{vecTxt} and \code{vecTxtQ} is that the
latter by default quotes the elements.}

\item{firstElements, lastElements}{The number of elements for which to use
the first respective last delimiters}

\item{lastHasPrecedence}{If the vector is very short, it's possible that the
sum of firstElements and lastElements is larger than the vector length. In
that case, downwardly adjust the number of elements to separate with the
first delimiter (\code{TRUE}) or the number of elements to separate with the
last delimiter (\code{FALSE})?}

\item{...}{Any addition arguments to \code{vecTxtQ} are passed on to
\code{vecTxt}.}
}
\value{
A character vector of length 1.
}
\description{
Easily parse a vector into a character value
}
\examples{
vecTxtQ(names(mtcars));
}
