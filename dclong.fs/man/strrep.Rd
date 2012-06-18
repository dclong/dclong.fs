\name{strReplace}
\alias{strrep}
\alias{strReplace}
\title{Character and String Manipulation}
\usage{
  strReplace(x, first, last, replacement)

  strrep(x, first, last, replacement)
}
\arguments{
  \item{x}{a character vector.}

  \item{first}{integer. The first element to be replaced.}

  \item{last}{integer. the last element to be replace.}

  \item{replacement}{a replacement of the substring.}
}
\description{
  Replace a substring by index.
}
\examples{
\dontrun{
tempStr = "abcRdlk$...$"
strReplace(tempStr,8,12,"GOOD")}
}
\author{
  Chuanlong Benjamin Du
}

