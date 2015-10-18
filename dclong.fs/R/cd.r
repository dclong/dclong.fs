
#' @title Get and Set Working Directory

#' @description Linux style of getting work directoy
#' and changing work directory.
#'
#' If the name of the directory that you want to change to is a simple one 
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
#' @export 
#' @rdname cd
cd = function(path, dd=NULL, lazy.input=TRUE) {
    if(!(is.null(dd))){
        path = getDirDef(dd)
        setwd(path)
        return(path)
    }
    if(lazy.input){
        path = symbol_to_string(substitute(path))
    }
    setwd(path)
    return(path)
}

cd.1 = function(){
    setwd("..")
    return("..")
}

cd.2 = function(){
    setwd("../..")
    return("../..")
}

cd.3 = function(){
    setwd("../../..")
    return("../../..")
}

cd.4 = function(){
    setwd("../../../..")
    return("../../../..")
}


#' @export 
#' @rdname cd
cs = function(path, dd=NULL, lazy.input=TRUE){
    path = cd(path=path, dd=dd, lazy.input=lazy.input)
    dir(path)
}

#' Linux style function for getting current work directory.
#' @export 
#' @rdname cd
pwd = getwd
