#' @title File and Directory Manipulation.
#' @description Check whether a path is a directory, file or file with specified extension or not.

#' @return a character vector.
#' @author Chuanlong Benjamin Du
#' @keywords file name extension


#' @param path a string vector.
#' @param lazy.input logical; If true, 
#' then simple string (without white space and other special characters) can be used without double or single quotation.

#' @export 
#' @rdname file
is.file=function(path,lazy.input=TRUE)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  file_test(op="-f",path)
}
#' @export 
#' @rdname file
is.dir <-
function(path,lazy.input=TRUE)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  file_test(op="-d",path)
}

#' @export 
#' @rdname file
is.folder = is.dir

#' @export 
#' @rdname file
#' @param extension string; a file extension.
#' @param \dots arguments that can be passed to function \code{grepl}.
is.extension = function(extension,path,...){
    grepl(pattern=paste("\\",extension,"$",sep=""),x=path,...)
}
#' @export 
#' @rdname file
is.pdf = function(path,...){
    is.extension(extension=".pdf",path=path,...)
}
#' @export 
#' @rdname file
is.txt = function(path,...){
    is.extension(extension=".txt",path=path,...)
}
#' @export 
#' @rdname file
is.bin = function(path,...){
    is.extension(extension=".bin",path=path,...)
}
#' @export 
#' @rdname file
is.cel = function(path,...){
    is.extension(extension=".cel",path=path,...)
}
#' @export 
#' @rdname file
is.r = function(path,...){
    is.extension(extension=".r",path=path,...)
}
#' @export 
#' @rdname file
is.rdata = function(path,...){
    is.extension(extension=".rdata",path=path,...)
}
#' @export 
#' @rdname file
is.Rdata = is.rdata
#' @export 
#' @rdname file
is.RData = is.rdata
