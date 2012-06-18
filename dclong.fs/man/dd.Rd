\name{updateDirDef}
\alias{adddd}
\alias{addDirDef}
\alias{deletedd}
\alias{deleteDirDef}
\alias{editdd}
\alias{editDirDef}
\alias{getdd}
\alias{getDirDef}
\alias{movedd}
\alias{moveDirDef}
\alias{showdd}
\alias{showDirDef}
\alias{swapdd}
\alias{swapDirDef}
\alias{updatedd}
\alias{updateDirDef}
\title{File/Direcotry Manipulation}
\usage{
  updateDirDef(path, index, warning = TRUE)

  updatedd(path, index, warning = TRUE)

  swapDirDef(index1, index2)

  swapdd(index1, index2)

  showDirDef()

  showdd()

  moveDirDef(index, up, down = -up)

  movedd(index, up, down = -up)

  getDirDef(index)

  getdd(index)

  editDirDef()

  editdd()

  deleteDirDef(index)

  deletedd(index)

  addDirDef(path, index = NULL, warning = TRUE)

  adddd(path, index = NULL, warning = TRUE)
}
\arguments{
  \item{path}{path of a directory.}

  \item{index}{a positive integer.}

  \item{warning}{logical; if ture warning information is
  display if any, o.w., warning information is supressed.}

  \item{index1}{a positive integer.}

  \item{index2}{a positive integer.}

  \item{up}{an integer indicate how many position you want
  to move a directory up. It doesn't have to be positive.
  If it's negative, then the selected directory will be
  moved down.}

  \item{down}{an integer indicate how many position you
  want to move a directory down. It doesn't have to be
  positive. If it's negative, then the selected directory
  will be moved up.}
}
\description{
  Add, delete, update, move, extract and show directories
  defined by user. You can also edit the file containing
  these directories directly. Function \code{addDirDef} or
  \code{adddd} add new defined directories; function
  \code{deleteDirDef} or \code{deletedd} delete defined
  directories; function \code{updateDirDef} or
  \code{updatedd} update existing defined directories;
  function \code{moveDirDef} or \code{movedd} move existing
  defined directories up or down; function
  \code{swapDirDef} or \code{swapdd} swap the position of
  two existing defined directories; function
  \code{getDirDef} or \code{getdd} return the path of
  defined directories; function \code{showDirDef} or
  \code{showdd} print all defined directories; function
  \code{editDirDef} or \code{editdd} allows you open the
  file containing defined directories and edit them
  directly.
}
\examples{
\dontrun{
#adddd(getwd(),1)}
}
\author{
  Chuanlong Benjamin Du
}
\keyword{directory}
\keyword{manipulation}

