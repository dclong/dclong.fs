
#' Input and Output.
#' Read data into a matrix if possible, which is a mutation of \code{\link[utils]{read.table}}.
#' @param title the title of the file to be read in.
#' @param path the parent directory of the file.
#' @param file path (either short or long) of the file.
#' @param header refer to \code{read.table}.
#' @param sep refer to \code{read.table}.
#' @param quote refer to \code{read.table}.
#' @param dec refer to \code{read.table}.
#' @param lazy.input logical; If true, 
#' then simple string (without white spaceand other special characters) can be used 
#' without double or single quotation.
#' @return a matrix read from the specified file.
#' @author Chuanlong Benjamin Du
#' @seealso \code{\link[utils]{read.table}}, \code{\link{read_text}}.
#' @keywords data matrix read
#' @examples
#' \dontrun{
#' #create a temp file
#' fileName= "TEMP_TEMP.TXT"
#' file.create(fileName)
#' write.table(matrix(rnorm(9),nrow=3),file=fileName,row.names=FALSE,col.names=FALSE)
#' #read matrix out of the file
#' readMatrix(fileName)
#' }
#' @export 
#' @rdname read_matrix
read_matrix <- function(title, path=getwd(), file=NULL, header=FALSE, sep="", quote="\"'", dec=".", lazy.input=TRUE) {
    if(lazy.input){
        if(is.null(file)){#don't get rid of substitute
            title = symbol_to_string(substitute(title))
            path = symbol_to_string(substitute(path))
            file = join_path(path, title)
        } else{
            file = symbol_to_string(substitute(file))
        }
        sep = symbol_to_string(substitute(sep))
        quote = symbol_to_string(substitute(quote))
        dec = symbol_to_string(substitute(dec))
    }else{
        if(is.null(file)){
            file = join_path(path, title)
        }
    }
    as.matrix(read.table(file=file, header=header, sep=sep, quote=quote))
}
