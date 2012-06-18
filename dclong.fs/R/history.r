#' R History
#' @description Function \code{h} wraps \code{history} to make it more 
#' convenient to check R history.
#' @param pattern a regular expression.
#' @param select an integer indexing the command to be selected. If negative, the indexing is reversed. 
#' That is -1 stands for the last command, -2 stands for the last but 1 command, etc.
#' @param old a pattern of strings to be substituted.
#' @param new the new string to replace the old pattern.
#' @param preview logical; If TRUE, the command is printed to the console; otherwise it is executed.
#' @param max.show the maximum history command to show.
#' @param reverse logical; if \code{TRUE}, commands are displayed in reverse order.
#' @param lazy.input logical; If TRUE, single/double quotes can be omitted for arguments
#' \code{pattern}, \code{old} and \code{new} if they do not contain white spaces.
#' @param ... other arguments that be passed to function \code{history}.
#' TODO make it possible to index history command directly, and run it directly, also 
#' substitution ...
#' @export
h = function(pattern,select,old,new,preview=FALSE,max.show=25,reverse = FALSE,lazy.input=TRUE,...){
    if(lazy.input){
        pattern=symbolToString(substitute(pattern),eval=FALSE)
    }
    file1 <- tempfile("Rrawhist")
    savehistory(file1)
    rawhist <- readLines(file1)
    unlink(file1)
    if (!missing(pattern)){
        rawhist <- unique(grep(pattern, rawhist, value = TRUE, ignore.case=TRUE))
    }
    nlines <- length(rawhist)
    if (nlines) {
        inds <- max(1, nlines - max.show):nlines
        if(reverse){ 
            inds <- rev(inds)
        }
    } else {
        inds <- integer()
    }
    if(missing(select)){
        return(cat(paste(inds,": ",rawhist[inds],collapse="\n",sep=""),"\n"))
    }
    if(select<0){
        select = length(inds) + select
    }
    command = rawhist[inds][select]
    if(!missing(old)&&!missing(new)){
        if(lazy.input){
            old = symbolToString(substitute(old),eval=FALSE)
            new = symbolToString(substitute(new),eval=FALSE)
        }
        gsub(old,new,command,...) -> command
    }
    if(preview){
        return(cat(command,"\n"))
    }
    eval(parse(text=command))
}
