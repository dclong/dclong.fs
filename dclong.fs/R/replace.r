
#' @title Character and String Manipulation
#' @description Replace a substring by index.
#' @param x a character vector.
#' @param first integer. The first element to be replaced.
#' @param last integer. the last element to be replace.
#' @param replacement a replacement of the substring.
#' @author Chuanlong Benjamin Du
#' @examples
#' \dontrun{
#' tempStr = "abcRdlk$...$"
#' strReplace(tempStr,8,12,"GOOD")}

#' @export
#' @rdname strrep
strReplace <-
function(x, first, last, replacement){
  subString1 = substr(x,1,first-1)
  subString2 = substr(x,last+1,nchar(x))
  return(paste(subString1,replacement,subString2,sep=""))
}
#' @export
#' @rdname strrep
strrep = strReplace
