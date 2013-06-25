
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
#' @return a character vector of ".aux" files in the given directory.
#' @author Chuanlong Benjamin Du
#' @keywords display extension
#' @examples
#' \dontrun{
#' dir.aux()}

#' @export 
#' @rdname dir
dir.extension <-
function(extension,path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    extension=symbolToString(substitute(extension))
    path=symbolToString(substitute(path))
  }
  if(!missing(dd)){
    path=getDirDef(dd)
  }
  if(exact){
      dir(path=path,pattern=paste(".*\\",extension,"$",sep=""),all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,ignore.case=TRUE)->files
  }else{
      dir(path=path,pattern=paste(".*\\",extension,sep=""),all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,ignore.case=TRUE)->files
  }
  if(!missing(pattern)){
    files = grep(pattern=pattern,x=files,value=TRUE,...)
  }
  return(files)
}
#' @export 
#' @rdname dir
dir.hidden=function(path=".",dd,pattern,full.names,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...){
  if(lazy.input){
    path = symbolToString(substitute(path))
  }
  if(!missing(dd)){
    path = getDirDef(dd)
  }
  allFiles = dir(path=path,all.files=TRUE,full.names=full.names,recursive=recursive,include.dirs=include.dirs)
  visibleFiles = dir(path=path,all.files=FALSE,full.names=full.names,recursive=recursive,include.dirs=include.dirs)
  hiddenFiles = setdiff(allFiles,visibleFiles)
  if(!missing(pattern)){
    hiddenFiles = grep(pattern=pattern,x=hiddenFiles,value=TRUE,...)
  }
  return(hiddenFiles)
}
#' @export 
#' @rdname dir
dir.missing <-
function(path=".",dd,pattern,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...){
  if(lazy.input){
    path = symbolToString(substitute(path))
  }
  if(!missing(dd)){
    path = getDirDef(dd)
  }
  files = dir(path=path,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs)
  files = files[is.file(files,FALSE)]
  fileNames = fileName(path=files,full=FALSE,extension=TRUE)
  grepl(pattern=".",x=fileNames,fixed=TRUE)->hasExtension
  files = files[!hasExtension]
  if(!missing(pattern)){
    files = grep(pattern=pattern,x=files,value=TRUE,...)
  }
  return(files)
}

#' @export 
#' @rdname dir
dir.file <-
function(path=".",dd,pattern,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...){
  if(lazy.input){
    path = symbolToString(substitute(path))
  }
  if(!missing(dd)){
    path = getDirDef(dd)
  }
  files = dir(path=path,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs)
  files = files[is.file(files,FALSE)]
  if(!missing(pattern)){
    files = grep(pattern=pattern,x=files,value=TRUE,...)
  }
  return(files)
}
#' @export 
#' @rdname dir
dir.dir <-
function(path=".",dd,pattern,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  if(!missing(dd)){
    path=getDirDef(dd)
  }
  dirs=dir(path=path,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs)
  dirs=dirs[is.dir(dirs,FALSE)]
  if(!missing(pattern)){
    dirs=grep(pattern=pattern,x=dirs,value=TRUE,...)
  }
  return(dirs)
}
#' @export 
#' @rdname dir
dir.folder=dir.dir
#' @export 
#' @rdname dir
dir.word <-
function(path=".",dd,pattern,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  c(dir.doc(path=path,dd=dd,pattern=pattern,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE),dir.docx(path=path,dd=dd,pattern=pattern,all.files=all.files,names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE))
}
#' @export 
#' @rdname dir
dir.sys <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".sys",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.inf <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".inf",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.cpp <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".cpp",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.c <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".c",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.bin <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".bin",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.ppt <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".ppt",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.excel <-
function(path=".",dd,pattern,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  c(dir.xls(path=path,dd=dd,pattern=pattern,all.files=all.files,
            full.names=full.names,recursive=recursive,include.dirs=include.dirs,
            lazy.input=FALSE),dir.xlsx(path=path,dd=dd,pattern=pattern,
                                       all.files=all.files,names=full.names,recursive=recursive,
                                       include.dirs=include.dirs,lazy.input=FALSE))
}
#' @export 
#' @rdname dir
dir.xlsx <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".xlsx",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.xls <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".xls",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.txt <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".txt",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.tex <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".tex",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.class <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".class",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.java <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".java",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.rhistory <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".rhistory",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.Rhistory = dir.rhistory
#' @export 
#' @rdname dir
dir.RHistory = dir.rhistory
#' @export 
#' @rdname dir
dir.rdata <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".rdata",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.Rdata = dir.rdata
#' @export 
#' @rdname dir
dir.RData = dir.rdata
#' @export 
#' @rdname dir
dir.r <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".r",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.R = dir.r
#' @export 
#' @rdname dir
dir.png <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".png",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.pdf <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".pdf",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.log <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".log",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.jpg <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".jpg",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.jpeg <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".jpeg",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.gz <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".gz",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.eps <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".eps",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.dvi <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".dvi",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.docx <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".docx",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.doc <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".doc",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.dat <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".dat",path=path,dd=dd,pattern=pattern,exact=exact,
                all.files=all.files,full.names=full.names,recursive=recursive,
                include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.aux <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".aux",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.bak <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".bak",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.csv <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".csv",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.cel <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".cel",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.rpt <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".rpt",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.chp <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".chp",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,
                full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.h <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".h",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,
                full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.hpp <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".hpp",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,
                full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.dll <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".dll",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,
                full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.so <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".so",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,
                full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.sl <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".sl",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,
                full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.dbf <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".dbf",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,
                full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
#' @export 
#' @rdname dir
dir.dylib <-
function(path=".",dd,pattern,exact=FALSE,all.files=FALSE,full.names=FALSE,recursive=FALSE,include.dirs=FALSE,lazy.input=TRUE,...)
{
  if(lazy.input){
    path=symbolToString(substitute(path))
  }
  dir.extension(extension=".dylib",path=path,dd=dd,pattern=pattern,exact=exact,all.files=all.files,
                full.names=full.names,recursive=recursive,include.dirs=include.dirs,lazy.input=FALSE)
}
