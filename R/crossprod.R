#' @include FLMatrix.R
NULL



setGeneric("crossprod",function(x,y=x)
                   standardGeneric("crossprod"))


## gk: why does it not suffice to define
## gk: move checks into %*% because then they are checked only once ane more easily maintained
setMethod("crossprod",
        signature(x ="ANY",y ="ANY"),
        function(x,y) t(x) %*% y )

setMethod("crossprod",
        signature(x ="FLMatrix",y ="FLMatrix"),
        function(x,y){
        if(nrow(x)!=nrow(y))
         stop("Dimensions are not correct")
        else{
          x<-t(x)
          return(do.call("%*%",list(x,y)))
        }})



setMethod("crossprod",
           signature(x = "FLMatrix",y = "vector"),
           function(x,y){
                if(length(y)==nrow(x))
                y <- as.FLMatrix(matrix(y))
                else if(nrow(x)==1){
                y <- as.FLMatrix(matrix(y))
              }
                else
                stop("non-conformable dimensions")
                return(do.call(crossprod,list(x,y)))
           })

setMethod("crossprod",
           signature(x = "FLMatrix",y = "matrix"),
           function(x,y){
                y <- as.FLMatrix(y)
                return(do.call(crossprod,list(x,y)))
           })

setMethod("crossprod",
           signature(x = "FLMatrix",y = "FLVector"),
           function(x,y){
                if(length(y) == nrow(x))
                y <- as.FLMatrix(y)
                else if(nrow(x)==1)
                y <- as.FLMatrix(y)
                else
                stop("non-conformable dimensions")
            return(do.call(crossprod,list(x,y)))
           })

setMethod("crossprod",
           signature(x = "FLVector",y = "FLVector"),
           function(x,y){
           if(length(y) != length(x)) stop(" non-conformable dimensions ")
            x <- as.FLMatrix(x,rows=1,cols=length(x))
            y <- as.FLMatrix(y)
            return(do.call("%*%",list(x,y)))
           })

setMethod("crossprod",
           signature(x = "FLVector",y = "FLMatrix"),
           function(x,y){
           if(length(x) == nrow(y)){
            x <- as.FLMatrix(x,rows=1,cols=length(x))
            return(do.call("%*%",list(x,y)))
          }
           else if(ncol(y)==1)
            x <- as.FLMatrix(x)
           else
            stop(" non-conformable dimensions ")

           return(do.call(crossprod,list(x,y)))
           })

setMethod("crossprod",
          signature(x = "FLVector",y = "vector"),
          function(x,y){
          if(length(x) != length(y))
            stop("non-conformable dimensions")
          else
            y <- as.FLMatrix(matrix(y))

          return(do.call(crossprod,list(x,y)))
          })

setMethod("crossprod",
          signature(x = "FLVector",y = "matrix"),
          function(x,y){
    ## gk: check here?
          y <- as.FLMatrix(y)
          return(do.call(crossprod,list(x,y)))
          })

setMethod("crossprod",
           signature(x = "matrix",y = "FLMatrix"),
           function(x,y){
               if(nrow(x)!=nrow(y))
               stop("non-conformable dimensions")
               else{
               x <- as.FLMatrix(x)
               return(do.call(crossprod,list(x,y)))
               }
           })


setMethod("crossprod",
           signature(x = "matrix",y = "FLVector"),
           function(x,y){
               if(length(y)!=nrow(x))
               stop("non-conformable dimensions")
               else{
               x <- as.FLMatrix(x,getFLConnection(y))
               return(do.call(crossprod,list(x,y)))
               }
           })

## todo: resolve masking
setMethod("crossprod",
          signature(x = "matrix",y = "matrix"),
          function(x,y){
          x<-t(x)
         return(do.call("%*%",list(x,y)))
          })

## todo: resolve masking
setMethod("crossprod",
          signature(x ="matrix",y = "vector"),
          function(x,y){
          x<-t(x)
          return(do.call("%*%",list(x,y)))
          })
