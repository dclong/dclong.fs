
#' @title File/Direcotry Manipulation
#' @description Add, delete, update, move, extract and show directories defined by user. 
#' You can also edit the file containing these directories directly.
#' Function \code{addDirDef} or \code{adddd} add new defined directories; 
#' function \code{deleteDirDef} or \code{deletedd} delete defined directories; 
#' function \code{updateDirDef} or \code{updatedd} update existing defined directories; 
#' function \code{moveDirDef} or \code{movedd} move existing defined directories up or down; 
#' function \code{swapDirDef} or \code{swapdd} swap the position of two existing defined directories; 
#' function \code{getDirDef} or \code{getdd} return the path of defined directories; 
#' function \code{showDirDef} or \code{showdd} print all defined directories; 
#' function \code{editDirDef} or \code{editdd} allows you open the file containing defined directories and edit them directly.
#' @author Chuanlong Benjamin Du
#' @keywords directory manipulation

#' @param path path of a directory.
#' @param index a positive integer.
#' @param warning logical; if ture warning information is display if any, 
#' o.w., warning information is supressed. 
#' @export
#' @rdname dd
updateDirDef <-
function(path, index,warning=TRUE)
{
  if(any(index<1)){
    stop("some specified index is out of range.")
  }
  filepath=getDirDefConf()
  #check whether the path is legal or not
  path=trim(path)
  #get rid of trailing \\ and /
  path=gsub('\\\\+$',"",path)
  path=gsub('/+$',"",path)
  paste('\\DEFAULTPATH{',path,'}',sep="")->path
  if(!all(file.exists(path))){
    stop("some path specified does not exist!")
  }
  dpath=readLines(filepath)
  dpath=dpath[grepl(pattern="\\\\DEFAULTPATH\\{.+\\}",x=dpath)]
  n.dpath=length(dpath)
  if(any(index>n.dpath))
    stop("some index specified is out of range.")
  if(path %in% dpath){
    if(warning){
      duplicatedPathWarning()
    }
  }else{
    #update a specified path
    dpath[index]=path
    writeLines(text=dpath,con=filepath)
  }
}

#' @export
#' @rdname dd
updatedd = updateDirDef

#' @export
#' @rdname dd
#' @param index1 a positive integer.
#' @param index2 a positive integer.
swapDirDef=function(index1,index2){
  if(any(index1<1 | index2<1))
    stop("some index specified are out of range.")
  filepath=getDirDefConf()
  dpath=readLines(filepath)
  dpath=dpath[grepl(pattern="\\\\DEFAULTPATH\\{.+\\}",x=dpath)]
  n.dpath=length(dpath)
  if(index1>n.dpath | index2>n.dpath)
    stop("some index specified is out of range.")
  #swap the paths
  swapElements(dpath,index1,index2)->dpath
  writeLines(text=dpath,con=filepath)
}

#' @export
#' @rdname dd
swapdd = swapDirDef

#' @export
#' @rdname dd
showDirDef <-
function()
{
  filepath=getDirDefConf()
  #print all default paths
    dpath=readLines(filepath)
    dpath=dpath[grepl(pattern="\\\\DEFAULTPATH\\{.+\\}",x=dpath)]
    dpath=gsub(pattern="^\\\\DEFAULTPATH\\{","",x=dpath)
    dpath=gsub(pattern="\\}$","",x=dpath)
    dpath=paste("Default Path ",1:length(dpath),": ",trim(dpath),sep="")
    #print paths information
    writeLines(dpath)
}

#' @export
#' @rdname dd
showdd = showDirDef

#' @export
#' @rdname dd
#' @param up an integer indicate how many position you want to move a directory up. 
#' It doesn't have to be positive. If it's negative, then the selected directory will be moved down.
#' @param down an integer indicate how many position you want to move a directory down. 
#' It doesn't have to be positive. If it's negative, then the selected directory will be moved up.
moveDirDef=function(index,up,down=-up){
  if(index<1)
    stop("argument index is out of range.")
  filepath=getDirDefConf()
  if(down!=0){
    dpath=readLines(filepath)
    dpath=dpath[grepl(pattern="\\\\DEFAULTPATH\\{.+\\}",x=dpath)]
    n.dpath=length(dpath)
    if(index>n.dpath)
      stop("argument index is out of range.")
    #move the path to the new position
    moveElement(dpath,index=index,down=down)->dpath
    writeLines(text=dpath,con=filepath)
  }
}

#' @export
#' @rdname dd
movedd = moveDirDef

getDirDefConf=function(){
  osType=.Platform$OS.type
  if(osType=="windows"){
    dirdef.file="dirdef_Windows.txt"
  }
  else{
    if(osType=="linux"){
      dirdef.file="dirdef_Linux.txt"
    }
    else{
      dirdef.file="dirdef_Mac.txt"
    }
  }
  filepath=paste(system.file(package="dclong.FileSystem"),dirdef.file,sep="/")
  if(!file.exists(filepath)){
    file.create(filepath)
  }
  return(filepath)
}

#' @export
#' @rdname dd
getDirDef <-
function(index)
{
  filepath=getDirDefConf()
  #get the specified default paths
    dpath=readLines(filepath)
    dpath=dpath[grepl(pattern="\\\\DEFAULTPATH\\{.+\\}",x=dpath)]
    path=dpath[index]
    path=gsub(pattern="^\\\\DEFAULTPATH\\{","",x=path)
    gsub(pattern="\\}$","",x=path)->path
    return(path)
}

#' @export
#' @rdname dd
getdd = getDirDef

#' @export
#' @rdname dd
editDirDef <-
function()
{
  filepath=getDirDefConf()
  shell.exec(filepath)
}

#' @export
#' @rdname dd
editdd = editDirDef

#' @export
#' @rdname dd
deleteDirDef <-
function(index)
{
  filepath=getDirDefConf()
  #delete the specified path if it's in the configuration file
  dpath=readLines(filepath)
  dpath=dpath[grepl(pattern="\\\\DEFAULTPATH\\{.+\\}",x=dpath)]
  dpath=dpath[-index]
  writeLines(text=dpath,con=filepath)
}

#' @export
#' @rdname dd
deletedd = deleteDirDef

#' @export
#' @rdname dd
#' @examples
#' \dontrun{
#' #adddd(getwd(),1)}
addDirDef <-
function(path,index=NULL,warning=TRUE)
{
  filepath=getDirDefConf()
  #write the path into configuration file
  path=trim(path)
  path=gsub('\\\\+$',"",path)
  path=gsub('/+$',"",path)
  if(!file.exists(path))
    stop("the path specified does not exist!")
  dpath=readLines(filepath)
  dpath=dpath[grepl(pattern="\\\\DEFAULTPATH\\{.+\\}",x=dpath)]
  paste('\\DEFAULTPATH{',path,'}',sep="")->path
  if(path %in% dpath){
    if(warning){
      duplicatedPathWarning()
    }
  }else{
    if(is.null(index))
      dpath=c(dpath,path)
    else{
      insertElement(x=dpath,index=index,path)->dpath
    }
    writeLines(text=dpath,con=filepath)
  }
}
#' @export
#' @rdname dd
adddd = addDirDef

duplicatedPathWarning <- function(){
  warning("the path is already there.")
}


insertElement = function(x,index,value){
  nx=length(x)
  if(index>nx)
    return(c(x,value))
  if(index<1)
    return(c(value,x))
  if(index==nx)
    return(c(x[-nx],value,x[nx]))
  if(index==1)
    return(c(x[1],value,x[-1]))
  return(c(x[1:(index-1)],value,x[index:nx]))
}

moveElement = function(x,index,up,down=-up){
  if(down>0){
    newIndex = index + down
    xLength = length(x)
    if(newIndex>xLength){
      newIndex = xLength
    }
    temp = x[index]
    x[index:(newIndex-1)] = x[(index+1):newIndex]
    x[newIndex] = temp
    return(x)
  }
  if(down<0){
    newIndex = index + down
    if(newIndex<1){
      newIndex = 1
    }
    temp = x[index]
    x[(newIndex+1):index] = x[newIndex:(index-1)]
    x[newIndex] = temp
    return(x)
  }
  return(x)
}

swapElements = function(x,index1,index2){
  temp = x[index1]
  x[index1] = x[index2]
  x[index2] = temp
}
