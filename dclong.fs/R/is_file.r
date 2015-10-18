#' @title File and Directory Manipulation.
#' @description Check whether a path is a directory, file or file with specified extension or not.

#' @return a character vector.
#' @author Chuanlong Benjamin Du
#' @keywords file name extension


#' @param path a string vector.
#' @param lazy.input logical; If true, 
#' then simple string (without white space and other special characters) can be used without double or single quotation.

#' @export 
#' @rdname file
is_file = function(path, lazy.input = TRUE) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    file_test(op = "-f", path)
}

#' @export 
#' @rdname file
is.file = is_file

#' @export 
#' @rdname file
is_dir <- function(path, lazy.input = TRUE) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    file_test(op = "-d", path)
}

#' @export 
#' @rdname file
is_folder = is_dir

#' @export 
#' @rdname file
is.dir = is_dir

#' @export 
#' @rdname file
is.folder = is_dir

#' @export 
#' @rdname file
#' @param extension string; a file extension.
#' @param \dots arguments that can be passed to function \code{grepl}.
is_extension = function(extension, path, ...) {
    grepl(pattern = paste("\\", extension, "$", sep = ""), x = path, ...)
}

#' @export 
#' @rdname file
is.extension = is_extension

#' @export 
#' @rdname file
is_pdf = function(path, ...) {
    is_extension(extension = ".pdf", path = path, ...)
}

#' @export 
#' @rdname file
is.pdf = is_pdf

#' @export 
#' @rdname file
is_txt = function(path, ...) {
    is_extension(extension = ".txt", path = path, ...)
}

#' @export 
#' @rdname file
is.txt = is_txt

#' @export 
#' @rdname file
is_bin = function(path, ...) {
    is_extension(extension = ".bin", path = path, ...)
}

#' @export 
#' @rdname file
is.bin = is_bin

#' @export 
#' @rdname file
is_cel = function(path, ...) {
    is_extension(extension = ".cel", path = path, ...)
}

#' @export 
#' @rdname file
is.cel = is_cel

#' @export 
#' @rdname file
is_r = function(path, ...) {
    is_extension(extension = ".r", path = path, ...)
}

#' @export 
#' @rdname file
is.r = is_r

#' @export 
#' @rdname file
is_rdata = function(path, ...) {
    is_extension(extension = ".rdata", path = path, ...)
}

#' @export 
#' @rdname file
is_Rdata = is_rdata

#' @export 
#' @rdname file
is_RData = is_rdata 

#' @export 
#' @rdname file
is.rdata = is_rdata

#' @export 
#' @rdname file
is.Rdata = is_rdata

#' @export 
#' @rdname file
is.RData = is_rdata

