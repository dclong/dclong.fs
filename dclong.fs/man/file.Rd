\name{is.file}
\alias{is.bin}
\alias{is.cel}
\alias{is.dir}
\alias{is.extension}
\alias{is.file}
\alias{is.folder}
\alias{is.pdf}
\alias{is.r}
\alias{is.R}
\alias{is.rdata}
\alias{is.Rdata}
\alias{is.RData}
\alias{is.txt}
\title{File and Directory Manipulation.}
\usage{
  is.file(path, lazy.input = TRUE)

  is.dir(path, lazy.input = TRUE)

  is.folder(path, lazy.input = TRUE)

  is.extension(extension, path, ...)

  is.pdf(path, ...)

  is.txt(path, ...)

  is.bin(path, ...)

  is.cel(path, ...)

  is.r(path, ...)

  is.R(path, ...)

  is.rdata(path, ...)

  is.Rdata(path, ...)

  is.RData(path, ...)
}
\arguments{
  \item{path}{a string vector.}

  \item{lazy.input}{logical; If true, then simple string
  (without white space and other special characters) can be
  used without double or single quotation.}

  \item{extension}{string; a file extension.}

  \item{\dots}{arguments that can be passed to function
  \code{grepl}.}
}
\value{
  a character vector.
}
\description{
  Check whether a path is a directory, file or file with
  specified extension or not.
}
\author{
  Chuanlong Benjamin Du
}
\keyword{extension}
\keyword{file}
\keyword{name}

