
#' @title Character and String Manipulation.
#' @description Convert objects, calls or even undefined symbols to characters if possible. 
#' It's used to make input of character more convenient (no need to type double or single quotation mark).
#' @param x any object or even undefined symbol.
#' @param eval logical; If TRUE, function and calls are evaluated; otherwise, they are preserved.
#' @return a string by coercing an object, a call or even an undefined symbol to characters.
#' @note This function works well with undefined simple symbols (which doen't have spaces in it), strings, calls that return strings. 
#' For other kind of objects, calls, symbols and so on, the result of this function is unpredictable. So it should be used cautiously.
#' @author Chuanlong Benjamin Du
#' @seealso \code{\link[base]{as.character}}, \code{\link[base]{substitute}} and \code{\link[base]{deparse}}.
#' @keywords string convert character
#' @examples
#' \dontrun{
#' #remove objects
#' try(rm(x),silent=TRUE)
#' #convert symbol x into string
#' sym2str(x)
#' }

#' @export symbolToString
#' @rdname sym2str
symbolToString <-
function(x,eval=TRUE)
{
  #result=try(exists(x,envir<-envir),silent=TRUE)
  result<-try(class(x),silent=TRUE)
  if(result=='name'){
    if(eval){
        result=try(eval(x),silent=TRUE)
        if(class(result)=='try-error'){
          return(deparse(x))
        }
        else{
          return(as.character(result))
        }
    }
    return(as.character(x))
  }
  if(class(result)=='try-error'){
    return(deparse(substitute(x)))
  }
  if(result=='call'){
      if(eval){
        return(as.character(eval(x)))
      }
      return(as.character(x))
  }
  if(result=="numeric" || result=="NULL" || result=="logical"){
    return(as.character(x))
  }
  if(result=="function"){
    return(deparse(substitute(x)))
  }
  if(result=="character"){
    return(x)
  }
  stop("argument x cannot be coerced to character.")
}

#' @export
#' @rdname sym2str
sym2str = symbolToString

