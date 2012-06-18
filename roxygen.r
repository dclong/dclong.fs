#!/usr/bin/env Rscript
library(roxygen2)
roxygenize("dclong.fs")
f = 'dclong.fs/R/.Rhistory'
if(file.exists(f)){
    file.remove(f)
}
# system('rm dclong.fs/R/.Rhistory')

