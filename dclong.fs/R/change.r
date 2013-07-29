
#' @title File and Directory Manipulation.
                                                                              #' 
#' @description Function \code{changeFileContent} (or \code{cfc}) changes content of multiple files at the same time; 
#' function \code{changeFileName} (or \code{cfn} changes names of multiple files at the same time; 
#' function \code{changeDirName} changes names of multiple directories at the same time.
#' 
#' @param old a regular expression indicates the pattern of old content.
#' 
#' @param new a string which replaces the old pattern.
#' 
#' @param fpattern a regular expression specifies which files will be changed. 
#' If NULL, then no file is specified by this argument.
#' 
#' @param flist a character vector which specifies a list of files/directories to be changed.
#' 
#' @param cfixed logical; Indicate when changing file content, 
#' file name or directory name, whether regular expression be used or not. 
#' If true then \code{cpattern} is a string to be matched as it is. 
#' Overrides all conflicting arguments.
#'
#' @param cignore.case logical; 
#' Indicate whether to ignore case when changing file content, 
#' file names or directory names.
#'
#' @param fignore.case logical; 
#' Indicate whether to ignore case when finding files that match \code{fpattern}.
#'
#' @param path path of a directory in which to search files or directories.
#'
#' @param dd a positive integer which is an index of some directory defined. 
#' You can use \code{\link{showdd}} to check the index of directories defined.
#'
#' @param all logical; 
#' Indicate whether to change all occurences that matches patteern specified by \code{old}.
#'
#' @param interact logical; 
#' Indicate whether allow user interaction while changing file content, 
#' file names or directory names. 
#' If TRUE, then user's interaction is required.
#'
#' @param lazy.input logical; 
#' If true, then simple string (without white space and other special characters) 
#' can be used without double or single quotation.
#'
#' @param ... extra parameters that can be passed to function \code{\link{grep}} and \code{link{gsub}}. 
#' They only affect the changing process, 
#' i.e. the name-finding process won't be affected by these extra parameters.
#' 
#' @note Functions here can be dangerous since they make changes of file content, 
#' file names and directory names, so be careful when you use them.
#' 
#' @author Chuanlong (Ben) Du
#'
#' @keywords change file content name directory
#'
#' @examples
#' #create a temp file
#' fileName = "TEMP_TEMP.TXT"
#' #write something into the file
#' cat("Just TRY\n", file = fileName)
#' #changefc(old = "TRY", new = "try", flist = fileName)
#'
#' @export changeFileContent changefc changeFileName changefn changeDirName changedn
#' @importFrom dclong.String symbolToString

changeFileContent = function(old, new, fpattern = NULL, flist = NULL, cfixed = FALSE, 
cignore.case = FALSE, fignore.case = TRUE, path = getwd(), dd = NULL, all = TRUE, interact = TRUE, lazy.input = TRUE, ...) {
    if(lazy.input){
        old = dclong.String::symbolToString(substitute(old))
        new = dclong.String::symbolToString(substitute(new))
        fpattern = dclong.String::symbolToString(substitute(fpattern))
        flist = dclong.String::symbolToString(substitute(flist))
    }
    if(!is.null(dd)){
        path = getDirDef(dd)
    }
    #get the files to be changed
    if(!is.null(fpattern) && length(fpattern)){
        files = dir(path = path, pattern = fpattern, full.names = TRUE, ignore.case = fignore.case)
    } else{
        files = NULL
    }
    files = c(files, flist)
    files = files[is.file(files, FALSE)]
    #warn user about the risk of the operation
    if(interact){
        #print out the files to be changed
        cat(paste(files, collapse = "\n"), "\n")
        cat("File(s) listed above will be changed. Making copies first can avoid unnecessary troubles.\n")
        userIntent = askUserIntent()
        if(userIntent = = 'o'){
          #process one by one      !you'd better make it interactive when changing file content
          return(changeFileContentOneByOne(old, new, files, cfixed, cignore.case, all, ...)
        }
        if(userIntent = = 'a'){
            #process all files
            return(changeMultipleFileContent(old, new, files, cfixed, cignore.case, all, ...))
        }
        #cancel operation
        return(cancelOperation())
    }
    #process all file silently
    return(changeMultipleFileContent(old, new, files, cfixed, cignore.case, all, ...))
}

cfc = changeFileContent

#' @param dpattern a regular expression specifies which directories will be changed. 
#' If NULL, then no file is specified by this argument.
#' @param dlist a character vector which specifies a list of directories to be changed.
changeDirName = function(old, new, dpattern = NULL, dlist = NULL, cfixed = FALSE, cignore.case = TRUE, 
fignore.case = TRUE, path = getwd(), dd = NULL, all = TRUE, interact = TRUE, lazy.input = TRUE, ...) {
    if(lazy.input){
        old = dclong.String::symbolToString(substitute(old))
        new = dclong.String::symbolToString(substitute(new))
        dpattern = dclong.String::symbolToString(substitute(dpattern))
        dlist = dclong.String::symbolToString(substitute(dlist))
    }
    if(!is.null(dd)){
        path = getDirDef(dd)
    }
    dirs = NULL
    if(!is.null(dpattern) && length(dpattern)){
        dirs = dir(path = path, pattern = dpattern, full.names = TRUE, ignore.case = fignore.case)
    }
    dirs = c(dirs, dlist)
    dirs = dirs[is.dir(dirs, FALSE)]
    #dir names, parent names and so on
    if(interact){
        #check which files are to be changed
        dirs = grep(pattern = old, x = dirs, ignore.case = cignore.case, value = TRUE, fixed = cfixed, ...)
        if(length(dirs) = = 0){
            return(cat("No directory names will be changed.\n"))
        }
        newDirs = newFileName(old = old, new = new, flist = dirs, cfixed = cfixed, cignore.case = cignore.case, all = all, ...)
        dirTable = cbind(dirs, newDirs)
        colnames(dirTable) = c("Old", "New")
        #print out the directories to be changed
        print(dirTable)
        cat("The above renaming will be Done.\n")
        userIntent = askUserIntent()
        if(userIntent = = 'o'){
            #process one by one
            return(changeNameOneByOne(dirTable, "directory", path, cfixed, cignore.case))
        }
        if(userIntent = = 'a'){
            #process all files
            return(changeMultipleName(dirTable, path, cfixed, cignore.case))
        }
        #cancel operation
        return(cancelOperation())
    }
    #process all file silently
    return(changeMultipleName(dirTable, path, cfixed, cignore.case))
}

cdn = changeDirName

changeFileName = function(old, new, fpattern = NULL, flist = NULL, cfixed = FALSE, cignore.case = TRUE, 
fignore.case = TRUE, path = getwd(), dd = NULL, all = TRUE, interact = TRUE, lazy.input = TRUE, ...) {
    if(lazy.input){
        old = dclong.String::symbolToString(substitute(old))
        new = dclong.String::symbolToString(substitute(new))
        fpattern = dclong.String::symbolToString(substitute(fpattern))
        flist = dclong.String::symbolToString(substitute(flist))
    }
    if(!is.null(dd)){
        path = getDirDef(dd)
    }
    #get the files
    files = NULL
    if(!is.null(fpattern) && length(fpattern)){
        files = dir(path = path, pattern = fpattern, full.names = TRUE, ignore.case = fignore.case)
    }
    files = c(files, flist)
    files = files[is.file(files, FALSE)]
    if(interact){
        #check which files are to be changed
        file.names = fileName(file = files, full = FALSE, extension = TRUE)
        file.index = grep(pattern = old, x = file.names, ignore.case = cignore.case, value = FALSE, fixed = cfixed, ...)
        if(length(file.index) = = 0){
            return(cat("No file names will be changed.\n"))
        }
        newFiles = newFileName(old = old, new = new, flist = file.names, cfixed = cfixed, cignore.case = cignore.case, all = all, ...)
        parent.folders = parentFolder(files)
        fileTable = cbind(files, combinePath(parent.folders, newFiles))
        colnames(fileTable) = c("Old", "New")
        #print out the directories to be changed
        print(fileTable)
        cat("The above renaming will be Done.\n")
        userIntent = askUserIntent()
        if(userIntent = = 'o'){
            #process one by one
            return(changeNameOneByOne(old, new, fileTable, "file", path, cfixed, cignore.case))
        }
        if(userIntent = = 'a'){
            #process all files
            return(changeMultipleName(old, new, files, path, cfixed, cignore.case, all, ...))
        }
        #cancel operation
        cancelOperation()
    }
    #process all file silently
    return(changeMultipleName(old, new, files, path, cfixed, cignore.case, all, ...))
}

cfn = changeFileName

askUserIntent = function(){
    repeat{
        cat("O/o: Process one by one.\nA/a: Process all.\nC/c: Cancel operation.\n")
        flush.console()
        userIntent = scan(what = character(0), n = 1)
        tolower(userIntent)
        if(userIntent = = 'o' || userIntent = = 'a' || userIntent = = 'c'){
            return(userIntent)
        }
    }
}

askUserIntent2 = function(){
    repeat{
        cat("Y/y: Yes to this one.\nA/a: Yes to all.\nS/s: Skip this one.\nP/p: Skip all.\nC/c: Cancel Operation.\n")
        scan(what = character(), n = 1)->userIntent
        userIntent = tolower(userIntent)
        if(userIntent = = 'y' || userIntent = = 'a' || userIntent = = 'p' || userIntent = = 'c' || userIntent = = 's'){
            return(userIntent)
        }
    }
}

changeNameOneByOne = function(old, new, dlist, type, path, cfixed, cignore.case, all, ...){
    if(length(dlist)){
        cat(paste("Change the name of the following ", type, "?\n", sep = ""), dlist[1], "\n")
        userIntent = askUserIntent2()
        if(userIntent = = 'y'){
            changeOneName(old, new, dlist[1], path, cfixed, cignore.case, all, ...)
            return(changeNameOneByOne(old, new, dlist[-1], type, path, cfixed, cignore.case, all, ...))
        }
        if(userIntent = = 'a'){
            return(changeMultipleName(old, new, dlist, path, cfixed, cignore.case, all, ...))
        }
        if(userIntent = = 's'){
            return(changeNameOneByOne(old, new, dlist[-1], type, path, cfixed, cignore.case, all, ...))
        }
        if(userIntent = = 'p'){
            return()
        }
        cancelOperation()
    }
}

changeFileContentOneByOne = function(old, new, flist, cfixed, cignore.case, all, ...){
    if(length(flist)){
        cat("Process the following file?\n", flist[1], "\n")
        userIntent = askUserIntent2()
        if(userIntent = = 'y'){
            changeOneFileContent(old, new, flist[1], cfixed, cignore.case, all, ...)
            return(changeFileContentOneByOne(old, new, flist[-1], cfixed, cignore.case, all, ...))
        }
        if(userIntent = = 'a'){
            return(changeMultipleFileContent(old, new, flist, cfixed, cignore.case, all, ...))
        }
        if(userIntent = = 's'){
            return(changeFileContentOneByOne(old, new, flist[-1], cfixed, cignore.case, all, ...))
        }
        if(userIntent = = 'p'){
            return()
        }
        cancelOperation()
    }
}

changeOneFileContent = function(old, new, flist, cfixed, cignore.case, all, ...){
    #read the file content
    fileContent = readText(file = flist, lazy.input = FALSE)
    #replace old string with new string
    if(all){
        fileContent = gsub(pattern = old, replacement = new, fileContent, ignore.case = cignore.case, fixed = cfixed, ...)
    }else{
        fileContent = sub(pattern = old, replacement = new, fileContent, ignore.case = cignore.case, fixed = cfixed, ...)
    }
    #rewrite the file
    cat(fileContent, file = flist, append = FALSE)
}

changeMultipleFileContent = function(old, new, flist, cfixed, cignore.case, all, ...){
    for(file in flist){
        changeOneFileContent(old, new, file, cfixed, cignore.case, all, ...)
    }
}

cancelOperation = function(){
    stop("operation is canceled by user.")
}

newFileName = function(old, new, flist, cfixed, cignore.case, all, ...){
    if(all){
        return(gsub(pattern = old, replacement = new, x = flist, ignore.case = cignore.case, fixed = cfixed, ...))
    }
    return(sub(pattern = old, replacement = new, x = flist, ignore.case = cignore.case, fixed = cfixed, ...))
}

changeOneName = function(old, new, flist, path, cfixed, cignore.case, all, ...){
    newName = newFileName(old = old, new = new, flist = flist, cfixed = cfixed, cignore.case = cignore.case, all = all, ...)
    newFullPath = combinePath(path, newName)
    oldFullPath = combinePath(path, flist)
    file.rename(from = oldFullPath, to = newFullPath)
}

changeMultipleName = function(old, new, flist, path, cfixed, cignore.case, all, ...){
    for(f in flist){
        changeOneName(old, new, f, path, cfixed, cignore.case, all, ...)
    }
}
