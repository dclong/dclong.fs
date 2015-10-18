
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
#' #cfc(old = "TRY", new = "try", fpattern = '*.txt')
#'
#' @export 
#' @rdname change
changeFileContent = function(old, new, fpattern = NULL, cfixed = FALSE, 
cignore.case = FALSE, fignore.case = TRUE, path = getwd(), 
dd = NULL, all = TRUE, interact = TRUE, lazy.input = TRUE, ...) {
    if(lazy.input){
        old = symbol_to_string(substitute(old))
        new = symbol_to_string(substitute(new))
        fpattern = symbol_to_string(substitute(fpattern))
    }
    if(!is.null(dd)){
        path = getDirDef(dd)
    }
    #get the files to be changed
    files = character()
    if(!is.null(fpattern) && length(fpattern)){
        files = dir(path = path, pattern = fpattern, full.names = TRUE, ignore.case = fignore.case)
    }
    files = files[is.file(files, FALSE)]
    # warn user about the risk of the operation
    if(interact){
        #print out the files to be changed
        cat(paste(files, collapse = "\n"), "\n")
        cat("Content of files listed above are to be changed. Backup them to avoid problems!\n")
        userIntent = askUserIntent()
        if(userIntent == 'o'){
          #process one by one      
          return(changeFileContentOneByOne(old, new, files, cfixed, cignore.case, all, ...))
        }
        if(userIntent == 'a'){
            #process all files
            return(changeMultipleFileContent(old, new, files, cfixed, cignore.case, all, ...))
        }
        #cancel operation
        return(cancelOperation())
    }
    #process all file silently
    return(changeMultipleFileContent(old, new, files, cfixed, cignore.case, all, ...))
}
#' @export
#' @rdname change
cfc = changeFileContent
#' @rdname change
#' @export
changeFileName = function(old, new, fpattern = NULL, cfixed = FALSE, cignore.case = TRUE, 
fignore.case = TRUE, path = getwd(), dd = NULL, all = TRUE, interact = TRUE, lazy.input = TRUE, ...) {
    if(lazy.input){
        old = symbol_to_string(substitute(old))
        new = symbol_to_string(substitute(new))
        fpattern = symbol_to_string(substitute(fpattern))
    }
    if(!is.null(dd)){
        path = getDirDef(dd)
    }
    #get the files
    files = character()
    if(!is.null(fpattern) && length(fpattern)){
        files = dir(path = path, pattern = fpattern, full.names = TRUE, ignore.case = fignore.case)
    }
    if(interact){
        #check which files are to be changed
        file.names = file_name(path = files, extension = TRUE, full = FALSE)
        file.names = grep(pattern = old, x = file.names, 
                          ignore.case = cignore.case, value = TRUE, fixed = cfixed, ...)
        if(length(file.names) == 0){
            return(cat("No files to be renamed\n"))
        }
        newFiles = newFileName(old = old, new = new, files = file.names, 
            cfixed = cfixed, cignore.case = cignore.case, all = all, ...)
        ftable = cbind(file.names, newFiles)
        colnames(ftable) = c("From", "To")
        #print out the files to be changed
        print(ftable)
        cat("The above renaming is to be done.\n")
        userIntent = askUserIntent()
        if(userIntent == 'o'){
            #process one by one
            return(changeNameOneByOne(ftable=ftable, path=path)) 
        }
        if(userIntent == 'a'){
            #process all files
            return(changeMultipleName(ftable=ftable, path=path)) 
        }
        #cancel operation
        cancelOperation()
    }
    #process all file silently
    return(changeMultipleName(ftable=ftable, path))
}
#' @export
#' @rdname change
cfn = changeFileName

askUserIntent = function(){
    repeat{
        cat("O/o: Process one by one.\nA/a: Process all.\nC/c: Cancel operation.\n")
        flush.console()
        userIntent = scan(what = character(0), n = 1)
        tolower(userIntent)
        if(userIntent == 'o' || userIntent == 'a' || userIntent == 'c'){
            return(userIntent)
        }
    }
}

askUserIntent2 = function(){
    repeat{
        cat("Y/y: Yes to this one.\nA/a: Yes to all.\nS/s: Skip this one.\nP/p: Skip all.\nC/c: Cancel Operation.\n")
        scan(what = character(), n = 1)->userIntent
        userIntent = tolower(userIntent)
        if(userIntent == 'y' || userIntent == 'a' || userIntent == 'p' || userIntent == 'c' || userIntent == 's'){
            return(userIntent)
        }
    }
}

changeNameOneByOne = function(ftable, path){
    if(nrow(ftable)){
        cat("Change \"", ftable[1, 1], "\" to \"", ftable[1, 2], "\"?\n")
        userIntent = askUserIntent2()
        if(userIntent == 'y'){
            changeOneName(ftable[1, ,drop=FALSE], path=path)
            return(changeNameOneByOne(ftable=ftable[-1, ,drop=FALSE], path=path))
        }
        if(userIntent == 'a'){
            return(changeMultipleName(ftable=ftable, path=path))
        }
        if(userIntent == 's'){
            return(changeNameOneByOne(ftable=ftable[-1, ,drop=FALSE], path=path))
        }
        if(userIntent == 'p'){
            return()
        }
        cancelOperation()
    }
}

changeFileContentOneByOne = function(old, new, files, cfixed, cignore.case, all, ...){
    if(length(files)){
        cat("Process the following file?\n", files[1], "\n")
        userIntent = askUserIntent2()
        if(userIntent == 'y'){
            changeOneFileContent(old, new, files[1], cfixed, cignore.case, all, ...)
            return(changeFileContentOneByOne(old=old, new=new, files=files[-1], 
                cfixed=cfixed, cignore.case=cignore.case, all=all, ...))
        }
        if(userIntent == 'a'){
            return(changeMultipleFileContent(old=old, new=new, files=files, 
                cfixed=cfixed, cignore.case=cignore.case, all=all, ...))
        }
        if(userIntent == 's'){
            return(changeFileContentOneByOne(old=old, new=new, files=files[-1], 
                cfixed=cfixed, cignore.case=cignore.case, all=all, ...))
        }
        if(userIntent == 'p'){
            return()
        }
        cancelOperation()
    }
}

changeOneFileContent = function(old, new, file, cfixed, cignore.case, all, ...){
    #read the file content
    fileContent = read_text(file = file, lazy.input = FALSE)
    #replace old string with new string
    if(all){
        fileContent = gsub(pattern = old, replacement = new, x=fileContent, 
            ignore.case = cignore.case, fixed = cfixed, ...)
    }else{
        fileContent = sub(pattern = old, replacement = new, x=fileContent, 
            ignore.case = cignore.case, fixed = cfixed, ...)
    }
    #rewrite the file
    cat(fileContent, file = file, append = FALSE)
}

changeMultipleFileContent = function(old, new, files, cfixed, cignore.case, all, ...){
    for(file in files){
        changeOneFileContent(old = old, new = new, file = file, 
             cfixed = cfixed, cignore.case = cignore.case, all = all, ...)
    }
}

cancelOperation = function(){
    stop("operation is canceled by user.")
}

newFileName = function(old, new, files, cfixed, cignore.case, all, ...){
    if(all){
        return(gsub(pattern = old, replacement = new, x = files, ignore.case = cignore.case, fixed = cfixed, ...))
    }
    return(sub(pattern = old, replacement = new, x = files, ignore.case = cignore.case, fixed = cfixed, ...))
}

changeOneName = function(files, path){
    from = join_path(path, files[1])
    to = join_path(path, files[2])
    file.rename(from = from, to = to)
}

changeMultipleName = function(ftable, path){
    n = nrow(ftable)
    i = 1
    while(i <= n){
        changeOneName(ftable[i, ], path)
        i = i + 1
    }
}
