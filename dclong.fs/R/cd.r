
#' @title Get and Set Working Directory

#' @description Change work directory to the given one.
#'
#'If the name of the directory that you want to change to is a simple one 
#' (without space and other special symbols), 
#' and it's not an object in the current workspace, 
#' then you don't have to use double or single quotation marks. 
#' Function \code{cd} also support a quick way for changing working direcotries, 
#' which is backed by a bunch of functions in this package. 
#' Specifically, 
#' if you have defined directories using function \code{addDirDef} or \code{editDirDef}, 
#' you can change working directory to these defined directories quickly by using \code{cd(dd=n)}, 
#' where n is the index of a directory defined.
#' @param path a new working directory.
#' @param dd index of a default path (refer to \code{\link{showDirDef}}).
#' @param lazy.input logical; 
#' If \code{TRUE}, then simple string (without white space and other special characters) 
#' can be used without double or single quotation.
#' @author Chuanlong (Ben) Du
#' @seealso \code{setwd}.
#' @keywords working directory set
#' @examples
#' currentDir = getwd()
#' cd(..)
#' cd(currentDir)
#' @export cd
cd <- function(path,dd=NULL,lazy.input=TRUE)
{
  if(!(is.null(dd))){
    return(setwd(getDirDef(dd)))
  }
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  setwd(path)
}

