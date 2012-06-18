\name{combinePath}
\alias{combinePath}
\alias{fext}
\alias{fileExtension}
\alias{fileName}
\alias{fname}
\title{File and Directory Manipulation.}
\usage{
  combinePath(base, ...)

  fileName(path, extension = TRUE, full = FALSE)

  fname(path, extension = TRUE, full = FALSE)

  fileExtension(path, keep.dot = TRUE)

  fext(path, keep.dot = TRUE)
}
\arguments{
  \item{base}{the base path.}

  \item{\dots}{relavtive paths.}

  \item{path}{a string vector.}

  \item{full}{logical; whether to return full path or not.}

  \item{extension}{logical; whether to keep extension or
  not.}

  \item{keep.dot}{logical; if true then the dot is kept in
  the file extensions, o.w., it's removed from the file
  extensions.}
}
\value{
  the full path by combining the base and relative path.
}
\description{
  The following functions offers ways to manipulate paths.
  Function \code{combinePath} combines paths without
  worrying about the annoying trailing and leading "/";
  function \code{fileName} gets the file name from the path
  of a file; function \code{fileExtension} gets the file
  extension from the path of the a file.
}
\examples{
\dontrun{
combinePath("D:/Study/","/hello.txt")}
\dontrun{
fileName("C:/hello.txt",FALSE)}
}
\author{
  Chuanlong Benjamin Du
}
\seealso{
  \code{\link{fileName}}, \code{\link{fileExtension}}.
}
\keyword{combine}
\keyword{path}

