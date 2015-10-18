
#' @title File and Directory Manipulation.
#' @description Displays files with a given extension in a given directory.
#' @param extension character; extension of a file.
#' @param path path of a directory.
#' @param dd a positive integer which is the index of some directory defined. 
#' You can use function \code{\link{showDirDef}} to check the index of directories defined.
#' @param pattern character string containing a regular expression (or character string for fixed = TRUE) 
#' to be matched in the given character vector. 
#' Coerced by \code{\link[base]{as.character}} to a character string if possible. 
#' If a character vector of length 2 or more is supplied, the first element is used with a warning. 
#' Missing values are allowed except for regexpr and gregexpr.
#' @param exact a logical value. If \code{TRUE}, files with the exact file extension are returned, otherwise, 
#' files with extensions containing the specified extension are returned. 
#' @param all.files a logical value. If \code{FALSE}, only the names of visible files are returned. 
#' If \code{TRUE}, all file names will be returned.
#' @param full.names a logical value. If \code{TRUE}, the directory path is prepended to the file names. 
#' If \code{FALSE}, only the file names are returned.
#' @param recursive logical. If \code{TRUE} the listing recurse into directories.
#' @param include.dirs logical. If TRUE subdirectories names will be included in recursive listing, vice versa.
#' @param lazy.input logical; If \code{TRUE}, then simple string (without white space and other special characters) 
#' can be used without double or single quotation.
#' @param ... extra parameters that can be passed to function \code{\link[base]{grep}}.
#' @return a character vector of '.aux' files in the given directory.
#' @author Chuanlong Benjamin Du
#' @keywords display extension
#' @examples
#' \dontrun{
#' dir.aux()}

#' @export 
#' @rdname dir
dir_extension <- function(extension, path = ".", dd, pattern, exact = FALSE, all.files = FALSE, 
    full.names = FALSE, recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        extension = symbol_to_string(substitute(extension))
        path = symbol_to_string(substitute(path))
    }
    if (!missing(dd)) {
        path = getDirDef(dd)
    }
    if (exact) {
        files = dir(path = path, pattern = paste(".*\\", extension, "$", sep = ""), all.files = all.files, 
            full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
            ignore.case = TRUE)
    } else {
        files = dir(path = path, pattern = paste(".*\\", extension, sep = ""), all.files = all.files, 
            full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
            ignore.case = TRUE)
    }
    if (!missing(pattern)) {
        files = grep(pattern = pattern, x = files, value = TRUE, ...)
    }
    return(files)
}

#' @export 
#' @rdname dir
dir.extension = dir_extension

#' @export 
#' @rdname dir
dir_hidden = function(path = ".", dd, pattern, full.names, recursive = FALSE, include.dirs = FALSE, 
    lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    if (!missing(dd)) {
        path = getDirDef(dd)
    }
    allFiles = dir(path = path, all.files = TRUE, full.names = full.names, recursive = recursive, 
        include.dirs = include.dirs)
    visibleFiles = dir(path = path, all.files = FALSE, full.names = full.names, recursive = recursive, 
        include.dirs = include.dirs)
    hiddenFiles = setdiff(allFiles, visibleFiles)
    if (!missing(pattern)) {
        hiddenFiles = grep(pattern = pattern, x = hiddenFiles, value = TRUE, ...)
    }
    return(hiddenFiles)
}
#' @export 
#' @rdname dir
dir.hidden = dir_hidden
#' @export 
#' @rdname dir
dir_missing <- function(path = ".", dd, pattern, all.files = FALSE, full.names = FALSE, recursive = FALSE, 
    include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    if (!missing(dd)) {
        path = getDirDef(dd)
    }
    files = dir(path = path, all.files = all.files, full.names = full.names, recursive = recursive, 
        include.dirs = include.dirs)
    files = files[is.file(files, FALSE)]
    fileNames = file_name(path = files, full = FALSE, extension = TRUE)
    hasExtension <- grepl(pattern = ".", x = fileNames, fixed = TRUE)
    files = files[!hasExtension]
    if (!missing(pattern)) {
        files = grep(pattern = pattern, x = files, value = TRUE, ...)
    }
    return(files)
}

#' @export 
#' @rdname dir
dir.missing = dir_missing

#' @export 
#' @rdname dir
dir_file <- function(path = ".", dd, pattern, all.files = FALSE, full.names = FALSE, recursive = FALSE, 
    include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    if (!missing(dd)) {
        path = getDirDef(dd)
    }
    files = dir(path = path, all.files = all.files, full.names = full.names, recursive = recursive, 
        include.dirs = include.dirs)
    files = files[is.file(files, FALSE)]
    if (!missing(pattern)) {
        files = grep(pattern = pattern, x = files, value = TRUE, ...)
    }
    return(files)
}

#' @export 
#' @rdname dir
dir.file = dir_file

#' @export 
#' @rdname dir
dir_dir <- function(path = ".", dd, pattern, all.files = FALSE, full.names = FALSE, recursive = FALSE, 
    include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    if (!missing(dd)) {
        path = getDirDef(dd)
    }
    dirs = dir(path = path, all.files = all.files, full.names = full.names, recursive = recursive, 
        include.dirs = include.dirs)
    dirs = dirs[is.dir(dirs, FALSE)]
    if (!missing(pattern)) {
        dirs = grep(pattern = pattern, x = dirs, value = TRUE, ...)
    }
    return(dirs)
}

#' @export 
#' @rdname dir
dir_folder = dir_dir

#' @export 
#' @rdname dir
dir.dir = dir_dir

#' @export 
#' @rdname dir
dir.folder = dir_dir

#' @export 
#' @rdname dir
dir_word <- function(path = ".", dd, pattern, all.files = FALSE, full.names = FALSE, recursive = FALSE, 
    include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    c(dir_doc(path = path, dd = dd, pattern = pattern, all.files = all.files, full.names = full.names, 
            recursive = recursive, include.dirs = include.dirs, lazy.input = FALSE), 
      dir_docx(path = path, dd = dd, pattern = pattern, all.files = all.files, full.names = full.names, 
            recursive = recursive, include.dirs = include.dirs, lazy.input = FALSE))
}

#' @export 
#' @rdname dir
dir.word = dir_word

#' @export 
#' @rdname dir
dir_sys <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".sys", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.sys = dir_sys

#' @export 
#' @rdname dir
dir_inf <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".inf", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.inf = dir_inf

#' @export 
#' @rdname dir
dir_cpp <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".cpp", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.cpp = dir_cpp

#' @export 
#' @rdname dir
dir_c <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".c", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.c = dir_c

#' @export 
#' @rdname dir
dir_bin <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".bin", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.bin = dir_bin

#' @export 
#' @rdname dir
dir_ppt <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".ppt", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.ppt = dir_ppt

#' @export 
#' @rdname dir
dir_excel <- function(path = ".", dd, pattern, all.files = FALSE, full.names = FALSE, recursive = FALSE, 
    include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    c(dir_xls(path = path, dd = dd, pattern = pattern, all.files = all.files, full.names = full.names, 
            recursive = recursive, include.dirs = include.dirs, lazy.input = FALSE), 
      dir_xlsx(path = path, dd = dd, pattern = pattern, all.files = all.files, full.names = full.names, 
            recursive = recursive, include.dirs = include.dirs, lazy.input = FALSE))
}

#' @export 
#' @rdname dir
dir.excel = dir_excel

#' @export 
#' @rdname dir
dir_xlsx <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".xlsx", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.xlsx = dir_xlsx

#' @export 
#' @rdname dir
dir_xls <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".xls", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.xls = dir_xls

#' @export 
#' @rdname dir
dir_txt <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".txt", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.txt = dir_txt

#' @export 
#' @rdname dir
dir_tex <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".tex", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.tex = dir_tex

#' @export 
#' @rdname dir
dir_class <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".class", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.class = dir_class

#' @export 
#' @rdname dir
dir_java <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".java", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.java = dir_java

#' @export 
#' @rdname dir
dir_rhistory <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".rhistory", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir_Rhistory = dir_rhistory

#' @export 
#' @rdname dir
dir_RHistory = dir_rhistory

#' @export 
#' @rdname dir
dir.rhistory = dir_rhistory

#' @export 
#' @rdname dir
dir.Rhistory = dir_rhistory

#' @export 
#' @rdname dir
dir.RHistory = dir_rhistory

#' @export 
#' @rdname dir
dir_rdata <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".rdata", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir_Rdata = dir_rdata

#' @export 
#' @rdname dir
dir_RData = dir_rdata

#' @export 
#' @rdname dir
dir.rdata = dir_rdata

#' @export 
#' @rdname dir
dir.Rdata = dir_rdata

#' @export 
#' @rdname dir
dir.RData = dir_rdata

#' @export 
#' @rdname dir
dir_r <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".r", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir_R = dir_r

#' @export 
#' @rdname dir
dir.r = dir_r

#' @export 
#' @rdname dir
dir.R = dir_r

#' @export 
#' @rdname dir
dir_png <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".png", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.png = dir_png

#' @export 
#' @rdname dir
dir_pdf <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".pdf", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.pdf = dir_pdf

#' @export 
#' @rdname dir
dir_log <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".log", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.log = dir_log

#' @export 
#' @rdname dir
dir_jpg <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".jpg", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.jpg = dir_jpg

#' @export 
#' @rdname dir
dir_jpeg <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".jpeg", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.jpeg = dir_jpeg

#' @export 
#' @rdname dir
dir_gz <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".gz", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.gz = dir_gz

#' @export 
#' @rdname dir
dir_eps <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".eps", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.eps = dir_eps

#' @export 
#' @rdname dir
dir_dvi <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".dvi", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.dvi = dir_dvi

#' @export 
#' @rdname dir
dir_docx <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".docx", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.docx = dir_docx

#' @export 
#' @rdname dir
dir_doc <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".doc", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.doc = dir_doc

#' @export 
#' @rdname dir
dir_dat <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".dat", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.dat = dir_dat

#' @export 
#' @rdname dir
dir_aux <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".aux", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.aux = dir_aux

#' @export 
#' @rdname dir
dir_bak <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".bak", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.bak = dir_bak

#' @export 
#' @rdname dir
dir_csv <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".csv", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.csv = dir_csv

#' @export 
#' @rdname dir
dir_cel <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".cel", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.cel = dir_cel

#' @export 
#' @rdname dir
dir_rpt <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".rpt", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.rpt = dir_rpt

#' @export 
#' @rdname dir
dir_chp <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".chp", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.chp = dir_chp

#' @export 
#' @rdname dir
dir_h <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".h", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.h = dir_h

#' @export 
#' @rdname dir
dir_hpp <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".hpp", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.hpp = dir_hpp

#' @export 
#' @rdname dir
dir_dll <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".dll", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.dll = dir_dll

#' @export 
#' @rdname dir
dir_so <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".so", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.so = dir_so

#' @export 
#' @rdname dir
dir_sl <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".sl", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.sl = dir_sl

#' @export 
#' @rdname dir
dir_dbf <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".dbf", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.dbf = dir_dbf

#' @export 
#' @rdname dir
dir_dylib <- function(path = ".", dd, pattern, exact = FALSE, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, include.dirs = FALSE, lazy.input = TRUE, ...) {
    if (lazy.input) {
        path = symbol_to_string(substitute(path))
    }
    dir_extension(extension = ".dylib", path = path, dd = dd, pattern = pattern, exact = exact, 
        all.files = all.files, full.names = full.names, recursive = recursive, include.dirs = include.dirs, 
        lazy.input = FALSE)
}

#' @export 
#' @rdname dir
dir.dylib = dir_dylib


#' @export 
#' @rdname dir
dir_1 = function() {
    dir("..")
}

#' @export 
#' @rdname dir
dir_2 = function() {
    dir("../..")
}

#' @export 
#' @rdname dir
dir_3 = function() {
    dir("../../..")
}

#' @export 
#' @rdname dir
dir_4 = function() {
    dir("../../../..")
}
 
