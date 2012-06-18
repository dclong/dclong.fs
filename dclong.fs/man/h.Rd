\name{h}
\alias{h}
\title{R History}
\usage{
  h(pattern, select, old, new, preview = FALSE,
    max.show = 25, reverse = FALSE, lazy.input = TRUE, ...)
}
\arguments{
  \item{pattern}{a regular expression.}

  \item{select}{an integer indexing the command to be
  selected. If negative, the indexing is reversed. That is
  -1 stands for the last command, -2 stands for the last
  but 1 command, etc.}

  \item{old}{a pattern of strings to be substituted.}

  \item{new}{the new string to replace the old pattern.}

  \item{preview}{logical; If TRUE, the command is printed
  to the console; otherwise it is executed.}

  \item{max.show}{the maximum history command to show.}

  \item{reverse}{logical; if \code{TRUE}, commands are
  displayed in reverse order.}

  \item{lazy.input}{logical; If TRUE, single/double quotes
  can be omitted for arguments \code{pattern}, \code{old}
  and \code{new} if they do not contain white spaces.}

  \item{...}{other arguments that be passed to function
  \code{history}. TODO make it possible to index history
  command directly, and run it directly, also substitution
  ...}
}
\description{
  Function \code{h} wraps \code{history} to make it more
  convenient to check R history.
}

