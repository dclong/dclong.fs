
#' @title Character and String Manipulation.
#' @description Get rid of leading and/or trailing white spaces.
#' @param x a character vector, or an object that can be coerced to character by \code{as.character}.
#' @param ws logical; If \code{TRUE} then get rid of all leading white space, o.w. only leading spaces will be removed.
#' @return A subtring without leading or training (white) spaces.
#' @author Chuanlong Benjamin Du
#' @note \code{ltrim} and \code{rtrim} are short for \code{leftTrim} \code{rightTrim} respectively.
#' @keywords string character trim space
#' @examples
#' \dontrun{
#' #define a string
#' txt=" good\n !"
#' #get rid of leading white spaces
#' ltrim(txt)}

#' @export
#' @rdname trim
ltrim = function(x, ws=TRUE) {
    if(ws){
        x=gsub("^[[:space:]]+", "", x)
        return(x)
    }
    gsub("^ +", "", x)
}

#' @export
#' @rdname trim
#' @examples
#' \dontrun{
#' #get rid of trailing white spaces
#' rtrim(txt)}
rtrim = function(x, ws=TRUE){
    if(ws){
        x=gsub("[[:space:]]+$", "", x)
        return(x)
    }
    gsub(" +$", "", x)
}

#' @export
#' @rdname trim
#' @examples
#' \dontrun{
#' #get rid of both leading and trailing white spaces
#' trim(txt)}
trim = function(x, ws=TRUE){
    rtrim(ltrim(x, ws), ws)
}
