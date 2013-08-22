#' Transfomr variables to new ones.
#' This function is for the convenience of manipulate variables in batch.
#' @param v.old a vector of names of variables to be manipulated.
#' @param f the function to be applied to each variable in \code{vs}.
#' @param v.new a vector of names for new variables.
#' If you want to transform variables in place 
#' (i.e., replace old variables), 
#' let \code{v.new} be the same as \code{v.old}.
#' @param ... extra parameters to be passed to the function \code{f} or \code{g}.
#' @rdname vtran
#' @export
#'
tranv = function(v.old, f, v.new, ...){
    n = length(v.old)
    for(i in 1:n){
        if(missing(v.new){
            f(v.old[i], ...)
        }else
            assign(x=v.new[i], value=f(v.old[i], ...), pos=1)
        }
    }
} 
#' Run the function \code{f} with specified parameters.
#' @rdname vtran
#' @export
collapse = function(f, ...){
    x = do.call(c, list(...))
    eval(parse(text=paste(quote(f), "(", paste(x, collapse=", "), ")", sep="")))
}
#' "Growth" function for vectors (time series variables).
#' This is a generic function which allows you specify the 
#' function to be applied to \code{x_{k}} and \code{x_{k-1}}.
#' @param x a vector of chronological data.
#' @param g a function takes 2 arguments. 
#' It has the form \code{g(x2, x1)},
#' where \code{x2} is the vector with larger subscripts
#' and \code{x1} is the vector with smaller subscripts.
#' @param l the lag to use when calculate the generic growth function.
#' For example, suppose \code{x} is quarterly data. 
#' To calculate YOY growth,
#' one have to use lag of 4.
#' @param fixed.length logical; if \code{TRUE}, 
#' return a vector of the same length as \code{x}.
#' The first \code{lag} values of the returned vector is padded with \code{NA}.
#' If \code{FALSE}, return a vector with \code{lag}-element shorter than \code{x}.
#' @param drop.names integer; the index of the name to be dropped.
#' so you have to choose the name to be dropped.
#' It doesn't matter if \code{x} doesn't have names.
#' Usually you want drop either the first \code{lag} or the last \code{lag} names.
#' @rdname vtran
#' @export
#'
growth = function(x, g, l=1, fixed.length=FALSE, drop.names, ...){
    n = length(x)
    x.names = names(x)
    if(fixed.length){
        x.na = c(rep(NA, lag), x[1:(n - lag)])
        x = g(x, x.na, ...)
        names(x) = x.names
    }else{
        x = g(x[-(1:lag)], x[-((n-lag+1):n)], ...)
        if(missing(drop.names)){
            drop.names = 1:lag
        }
        names(x) = x.names[-drop.names]
    }
    x
}
#' Lag function as a special case of \code{growth}.
#' @rdname vtran
#' @export
lag = function(x, l=1, fixed.length=FALSE){
    growth(x=x, f=function(x2, x1){x1}, lag=k, fixed.length=fixed.length)
}
#' Difference function as a special case of \code{growth}.
#' @param times the order of differentiation.
#' @rdname vtran
#' @export
diff = function(x, l=1, times=1, fixed.length=FALSE){
    i = 1
    while(i <= times){
        x = growth(x=x, f=function(x2, x1){x2-x1}, lag=lag, fixed.length=fixed.length)
        i = i + 1
    }
    x
}
#' QoQ growth as a special case of \code{growth}.
#' @param q quarterly data.
#' @rdname vtran
#' @export
qtr.growth = function(q, fixed.length=FALSE){
    growth(x=q, f=function(x2,x1){x2/x1-1}, lag=1, fixed.length=fixed.length)
}
#' Annualized QoQ growth as a special case of \code{growth}.
#' @rdname vtran
#' @export
qtr2ann.growth = function(q, fixed.length=FALSE){
    growth(x=q, f=function(x2,x1){(x2/x1)^4-1}, lag=1, fixed.length=fixed.length)
}
#' YoY growth as a special case of \code{growth}.
#' @rdname vtran
#' @export
ann.growth = function(q, fixed.length=FALSE){
    growth(x=q, f=function(x2,x1){x2/x1-1}, lag=4, fixed.length=fixed.length)
}

