\name{capitalize}
\alias{cap}
\alias{capitalize}
\title{Character and String Manipulation}
\usage{
  capitalize(x)

  cap(x)
}
\arguments{
  \item{x}{a character vector.}
}
\value{
  A character vector with the same length and the same
  attribute but with the first letter of each word
  capitalized.
}
\description{
  Capitalize a given character vector.
}
\examples{
\dontrun{
#capitalization
cap("ab cd")}
}
\author{
  Chuanlong Benjamin Du
}
\seealso{
  \code{\link[base]{tolower}} and
  \code{\link[base]{toupper}}.
}
\keyword{capitalize}
\keyword{character}
\keyword{string}

