
#' @title Character and String Manipulation
#' @description Capitalize a given character vector.
#' @param x a character vector.
#' @return A character vector with the same length and the same attribute but with the first letter of each word capitalized.
#' @author Chuanlong Benjamin Du
#' @seealso \code{\link[base]{tolower}} and \code{\link[base]{toupper}}.
#' @keywords character capitalize string
#' @examples
#' \dontrun{
#' #capitalization
#' cap("ab cd")}

#' @export 
#' @rdname cap 
capitalize <-
function(x)
{
  x=symbolToString(substitute(x))
  if(!is.character(x))
    x=as.character(x)
  gsub("\\b(\\w)","\\U\\1",x,perl=TRUE)
}

#' @export
#' @rdname cap
cap=capitalize
