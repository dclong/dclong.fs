\name{dir.extension}
\alias{dir.aux}
\alias{dir.bak}
\alias{dir.bin}
\alias{dir.cel}
\alias{dir.chp}
\alias{dir.class}
\alias{dir.csv}
\alias{dir.dat}
\alias{dir.dir}
\alias{dir.dll}
\alias{dir.doc}
\alias{dir.docx}
\alias{dir.dvi}
\alias{dir.dylib}
\alias{dir.eps}
\alias{dir.excel}
\alias{dir.extension}
\alias{dir.file}
\alias{dir.folder}
\alias{dir.gz}
\alias{dir.h}
\alias{dir.hidden}
\alias{dir.hpp}
\alias{dir.inf}
\alias{dir.java}
\alias{dir.jpeg}
\alias{dir.jpg}
\alias{dir.log}
\alias{dir.missing}
\alias{dir.pdf}
\alias{dir.png}
\alias{dir.ppt}
\alias{dir.r}
\alias{dir.R}
\alias{dir.rdata}
\alias{dir.Rdata}
\alias{dir.RData}
\alias{dir.rhistory}
\alias{dir.Rhistory}
\alias{dir.RHistory}
\alias{dir.rpt}
\alias{dir.sl}
\alias{dir.so}
\alias{dir.sys}
\alias{dir.tex}
\alias{dir.txt}
\alias{dir.word}
\alias{dir.xls}
\alias{dir.xlsx}
\title{File and Directory Manipulation.}
\usage{
  dir.extension(extension, path = ".", dd, pattern,
    exact = FALSE, all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.hidden(path = ".", dd, pattern, full.names,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.missing(path = ".", dd, pattern, all.files = FALSE,
    full.names = FALSE, recursive = FALSE,
    include.dirs = FALSE, lazy.input = TRUE, ...)

  dir.file(path = ".", dd, pattern, all.files = FALSE,
    full.names = FALSE, recursive = FALSE,
    include.dirs = FALSE, lazy.input = TRUE, ...)

  dir.dir(path = ".", dd, pattern, all.files = FALSE,
    full.names = FALSE, recursive = FALSE,
    include.dirs = FALSE, lazy.input = TRUE, ...)

  dir.folder(path = ".", dd, pattern, all.files = FALSE,
    full.names = FALSE, recursive = FALSE,
    include.dirs = FALSE, lazy.input = TRUE, ...)

  dir.word(path = ".", dd, pattern, all.files = FALSE,
    full.names = FALSE, recursive = FALSE,
    include.dirs = FALSE, lazy.input = TRUE, ...)

  dir.sys(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.inf(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.bin(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.ppt(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.excel(path = ".", dd, pattern, all.files = FALSE,
    full.names = FALSE, recursive = FALSE,
    include.dirs = FALSE, lazy.input = TRUE, ...)

  dir.xlsx(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.xls(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.txt(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.tex(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.class(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.java(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.rhistory(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.Rhistory(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.RHistory(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.rdata(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.Rdata(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.RData(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.r(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.R(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.png(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.pdf(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.log(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.jpg(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.jpeg(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.gz(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.eps(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.dvi(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.docx(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.doc(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.dat(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.aux(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.bak(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.csv(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.cel(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.rpt(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.chp(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.h(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.hpp(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.dll(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.so(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.sl(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)

  dir.dylib(path = ".", dd, pattern, exact = FALSE,
    all.files = FALSE, full.names = FALSE,
    recursive = FALSE, include.dirs = FALSE,
    lazy.input = TRUE, ...)
}
\arguments{
  \item{extension}{character; extension of a file.}

  \item{path}{path of a directory.}

  \item{dd}{a positive integer which is the index of some
  directory defined. You can use function
  \code{\link{showDirDef}} to check the index of
  directories defined.}

  \item{pattern}{character string containing a regular
  expression (or character string for fixed = TRUE) to be
  matched in the given character vector. Coerced by
  \code{\link[base]{as.character}} to a character string if
  possible. If a character vector of length 2 or more is
  supplied, the first element is used with a warning.
  Missing values are allowed except for regexpr and
  gregexpr.}

  \item{exact}{a logical value. If \code{TRUE}, files with
  the exact file extension are returned, otherwise, files
  with extensions containing the specified extension are
  returned.}

  \item{all.files}{a logical value. If \code{FALSE}, only
  the names of visible files are returned. If \code{TRUE},
  all file names will be returned.}

  \item{full.names}{a logical value. If \code{TRUE}, the
  directory path is prepended to the file names. If
  \code{FALSE}, only the file names are returned.}

  \item{recursive}{logical. If \code{TRUE} the listing
  recurse into directories.}

  \item{include.dirs}{logical. If TRUE subdirectories names
  will be included in recursive listing, vice versa.}

  \item{lazy.input}{logical; If \code{TRUE}, then simple
  string (without white space and other special characters)
  can be used without double or single quotation.}

  \item{...}{extra parameters that can be passed to
  function \code{\link[base]{grep}}.}
}
\value{
  a character vector of ".aux" files in the given
  directory.
}
\description{
  Displays files with a given extension in a given
  directory.
}
\examples{
\dontrun{
dir.aux()}
}
\author{
  Chuanlong Benjamin Du
}
\keyword{display}
\keyword{extension}

