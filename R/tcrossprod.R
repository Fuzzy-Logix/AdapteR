#' @include FLMatrix.R
NULL


setGeneric("tcrossprod",function(x,y)
                   standardGeneric("tcrossprod"))

setMethod("tcrossprod",
        signature(x = "FLMatrix",y = "FLMatrix"),
        function(x,y){
        if(nrow(x)!=nrow(y))
         stop("Dimensions are not correct")
        else{
          x<-t(x)
          return(do.call("%*%",list(x,y)))
        }})

        

setMethod("tcrossprod",
           signature(x = "FLMatrix",y = "vector"),
           function(x,y){
                if(length(y)==nrow(x))
                y <- as.FLMatrix(matrix(y))
                else if(nrow(x)==1){
                y <- as.FLMatrix(matrix(y,1))
              }
                else
                stop("non-conformable dimensions")
                return(do.call(tcrossprod,list(x,y)))
           }) 

setMethod("tcrossprod",
           signature(x = "FLMatrix",y = "matrix"),
           function(x,y){
                y <- as.FLMatrix(y)
                return(do.call(tcrossprod,list(x,y)))
           })

setMethod("tcrossprod",
           signature(x = "FLMatrix",y = "FLVector"),
           function(x,y){
                if(length(y) == nrow(x))
                y <- as.FLMatrix(y)
                else if(nrow(x)==1)
                y <- as.FLMatrix(y,rows=1,cols=length(y))
                else
                stop("non-conformable dimensions")
            return(do.call(tcrossprod,list(x,y)))
           })

setMethod("tcrossprod",
           signature(x = "FLVector",y = "FLVector"),
           function(x,y){
           if(length(y) != length(x)) stop(" non-conformable dimensions ")
            x <- as.FLMatrix(x,rows=1,cols=length(x))
            y <- as.FLMatrix(y)
            return(do.call("%*%",list(x,y)))
           })

setMethod("tcrossprod",
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

           return(do.call(tcrossprod,list(x,y)))
           })

setMethod("tcrossprod",
          signature(x = "FLVector",y = "vector"),
          function(x,y){
          if(length(x) != length(y))
            stop("non-conformable dimensions")
          else
            y <- as.FLMatrix(matrix(y))

          return(do.call(tcrossprod,list(x,y)))  
          })

setMethod("tcrossprod",
          signature(x = "FLVector",y = "matrix"),
          function(x,y){
          y <- as.FLMatrix(y)
          return(do.call(tcrossprod,list(x,y)))
          })

setMethod("tcrossprod",
           signature(x = "matrix",y = "FLMatrix"),
           function(x,y){
               if(ncol(x)!=ncol(y))
               stop("non-conformable dimensions")
               else{
               x <- as.FLMatrix(x,getFLConnection(y))
               return(do.call(tcrossprod,list(x,y)))
               }
           })


setMethod("tcrossprod",
           signature(x = "matrix",y = "FLVector"),
           function(x,y){
               if(length(y)!=nrow(x))
               stop("non-conformable dimensions")
               else{
               x <- as.FLMatrix(x,getFLConnection(y))
               return(do.call(tcrossprod,list(x,y)))
               }
           })

setMethod("tcrossprod",
          signature(x = "matrix",y = "matrix"),
          function(x,y){
          y<-t(y)
          return(do.call("%*%",list(x,y)))
          })

setMethod("tcrossprod",
          signature(x ="matrix",y = "vector"),
          function(x,y){
          return(do.call("%*%",list(x,t(y))))})
