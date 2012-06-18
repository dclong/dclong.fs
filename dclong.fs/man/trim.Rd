\name{leftTrim}
\alias{leftTrim}
\alias{ltrim}
\alias{rightTrim}
\alias{rtrim}
\alias{trim}
\title{Character and String Manipulation.}
\usage{
  leftTrim(x, ws = TRUE)

  ltrim(x, ws = TRUE)

  rightTrim(x, ws = TRUE)

  rtrim(x, ws = TRUE)

  trim(x, ws = TRUE)
}
\arguments{
  \item{x}{a character vector, or an object that can be
  coerced to character by \code{as.character}.}

  \item{ws}{logical; If \code{TRUE} then get rid of all
  leading white space, o.w. only leading spaces will be
  removed.}
}
\value{
  A subtring without leading or training (white) spaces.
}
\description{
  Get rid of leading and/or trailing white spaces.
}
\note{
  \code{ltrim} and \code{rtrim} are short for
  \code{leftTrim} \code{rightTrim} respectively.
}
\examples{
\dontrun{
#define a string
txt=" good\\n !"
#get rid of leading white spaces
ltrim(txt)}
\dontrun{
#get rid of trailing white spaces
rtrim(txt)}
\dontrun{
#get rid of both leading and trailing white spaces
trim(txt)}
}
\author{
  Chuanlong Benjamin Du
}
\keyword{character}
\keyword{space}
\keyword{string}
\keyword{trim}

