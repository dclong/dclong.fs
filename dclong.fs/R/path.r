
#' @title File and Directory Manipulation.
#' @description The following functions offers ways to manipulate paths. 
#' Function \code{join_path} combines paths without worrying about the annoying trailing and leading "/";
#' function \code{file_name} gets the file name from the path of a file;
#' function \code{file_extension} gets the file extension from the path of the a file.
#' @author Chuanlong Benjamin Du


#' @param base the base path.
#' @param \dots relavtive paths.
#' @return the full path by combining the base and relative path.
#' @seealso \code{\link{fileName}}, \code{\link{fileExtension}}.
#' @keywords combine join path file name extension
#' @examples
#' \dontrun{
#' join_path("D:/Study/", "/hello.txt")}
#' @export
#' @rdname path
join_path <-
function(base, ...)
{
  #dots=match.call(expand.dots = FALSE)$...
  dots=list(...)
  if(length(dots))
  {
    if(!all(sapply(dots, FUN=function(x) is.symbol(x)||is.character(x))))
      stop("... must contain names or character strings")
  }
  else
    return(base)
  names <- sapply(dots, as.character)
  names=gsub("^\\\\","",names)
  names=gsub("^/","",names)
  names=gsub("\\\\$","",names)
  names=gsub("/$","",names)
  names=paste(names,collapse="/")
  #------------------------------------------
  base=gsub("\\\\$","",base)
  base=gsub("/$","",base)
  #------------------------------------------
  paste(base,names,sep="/")
}

#' @examples
#' \dontrun{
#' file_name("C:/hello.txt",FALSE)}
#' @export 
#' @rdname path
#' @param path a string vector.
#' @param full logical; whether to return full path or not.
#' @param extension logical; whether to keep extension or not.
file_name <-
function(path, extension=TRUE, full=FALSE)
{
  if(extension){
    if(full){
      return(path)
    }
    return(basename(path))
  }
  path = gsub("\\.[^.]*$", "", path)
  if(full){
    return(path)
  }
  return(basename(path))
}

#' @export 
#' @rdname path

#' @export 
#' @rdname path
#' @param keep_dot logical; if true then the dot is kept in the file extensions,
#' o.w., it's removed from the file extensions.
file_extension <-
function(path, keep_dot=TRUE)
{
  file.name = file_name(path=path, extension=TRUE, full=FALSE)
  if(keep_dot){
    ifelse(regexpr(pattern=".", text=file.name, fixed=TRUE)>0, paste(".", gsub("^.*\\.", "", file), sep=""), "")
  }else{
    ifelse(regexpr(pattern=".", text=file.name, fixed=TRUE)>0, gsub("^.*\\.", "", file), "")
  }
}
#' @export 
#' @rdname path

