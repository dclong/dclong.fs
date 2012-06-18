\name{h}
\alias{h}
\title{R History}
\usage{
  h(pattern, max.show = 25, reverse = FALSE, ...)
}
\arguments{
  \item{pattern}{a regular expression.}

  \item{max.show}{the maximum history command to show.}

  \item{reverse}{logical; if \code{TRUE}, commands are
  displayed in reverse order.}

  \item{...}{other arguments that be passed to function
  \code{history}. TODO make it possible to index history
  command directly, and run it directly, also substitution
  ...}
}
\description{
  Function \code{h} wraps \code{history} to make it more
  convenient to check R history.
}

