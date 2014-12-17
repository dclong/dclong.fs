#' File and Directory Manipulation.
#' Put the content of an object (vector, matrix or data frame) to the clipboard.
#' @param x the object whose content is to be copied to the clipboard.
#' @author Chuanlong Ben Du
#' @keywords copy clipboard
#' @examples
#' \dontrun{
#' # put a vecto 1:10 to the clipboard
#' copy(1:10)
#' }
#' @export copy
copy <- function(x){ 
    write.table(x=x, file="clipboard", row.names=FALSE, colnames=FALSE)
}
