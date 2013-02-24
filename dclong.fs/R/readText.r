
#' File and Directory Manipulation.
#' Reads text from a file.
#' @param title the title of the file to be read in.
#' @param path the path of the parent directory of the file to be read in.
#' @param file the path (either short or long) of the file.
#' @param lazy.input logical; If true, then simple string (without white spaceand other special characters) can be used without double or single quotation.
#' @return the content of the text file.
#' @author Chuanlong Benjamin Du
#' @seealso \code{\link[base]{readLines}}, \code{\link{readMatrix}}.
#' @keywords read file text
#' @examples
#' \dontrun{
#' #create a temp file
#' fileName = "TEMP_TEMP.txt"
#' file.create(fileName)
#' #write something into the file
#' cat("adf;f;",file=fileName)
#' #read the text out
#' readText(fileName)
#' }
#' @export readText
readText <-
function(title, path=getwd(), file=NULL,lazy.input=TRUE)
{
  if(lazy.input){
    if(is.null(file)){#don't get rid of substitute
      title=symbolToString(substitute(title))
      path=symbolToString(substitute(path))
      file=combinePath(path,title)
    }
    else{
      file=symbolToString(substitute(file))
    }
  }
  paste(readLines(file), collapse = "\n")
}
