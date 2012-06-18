\name{symbolToString}
\alias{sym2str}
\alias{symbolToString}
\title{Character and String Manipulation.}
\usage{
  symbolToString(x)

  sym2str(x)
}
\arguments{
  \item{x}{any object or even undefined symbol.}
}
\value{
  a string by coercing an object, a call or even an
  undefined symbol to characters.
}
\description{
  Convert objects, calls or even undefined symbols to
  characters if possible. It's used to make input of
  character more convenient (no need to type double or
  single quotation mark).
}
\note{
  This function works well with undefined simple symbols
  (which doen't have spaces in it), strings, calls that
  return strings. For other kind of objects, calls, symbols
  and so on, the result of this function is unpredictable.
  So it should be used cautiously.
}
\examples{
\dontrun{
#remove objects
try(rm(x),silent=TRUE)
#convert symbol x into string
sym2str(x)
}
}
\author{
  Chuanlong Benjamin Du
}
\seealso{
  \code{\link[base]{as.character}},
  \code{\link[base]{substitute}} and
  \code{\link[base]{deparse}}.
}
\keyword{character}
\keyword{convert}
\keyword{string}

