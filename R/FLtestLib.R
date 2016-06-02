#' @include utilities.R

#' @export
setGeneric("FLexpect_equal",
           function(object,expected,...)
               standardGeneric("FLexpect_equal"))
setMethod("FLexpect_equal",
          signature(object="FLMatrix",expected="ANY"),
          function(object,expected,...)
              testthat::expect_equal(as.matrix(object),
                                     expected,...))
setMethod("FLexpect_equal",
          signature(object="FLMatrix",expected="FLMatrix"),
          function(object,expected,...)
              testthat::expect_equal(as.matrix(object),
                                     as.matrix(expected),...))
setMethod("FLexpect_equal",
          signature(object="ANY",expected="FLMatrix"),
          function(object,expected,...)
              testthat::expect_equal(object,
                                     as.matrix(expected),...))
setMethod("FLexpect_equal",
          signature(object="FLVector",expected="vector"),
          function(object,expected,...)
              testthat::expect_equal(as.vector(object),
                                     expected,...))
setMethod("FLexpect_equal",
          signature(object="FLVector",expected="FLVector"),
          function(object,expected,...)
              testthat::expect_equal(as.vector(object),
                                     as.vector(expected),...))
setMethod("FLexpect_equal",signature(object="list",expected="list"),
          function(object,expected,...)
              llply(names(object),
                    function(i)
                        FLexpect_equal(object[[i]],
                                       expected[[i]],...)))
setMethod("FLexpect_equal",
          signature(object="ANY",expected="ANY"),
          function(object,expected,...)
              testthat::expect_equal(object,
                                     expected,...))

#' @export
expect_eval_equal <- function(initF,FLcomputationF,RcomputationF,benchmark=FALSE,...)
{
  I <- initF(...)
  if(!is.list(I$FL))
  I <- list(FL=list(I$FL),R=list(I$R))
    FLexpect_equal(do.call(FLcomputationF,I$FL),
                 do.call(RcomputationF,I$R),
                 check.attributes=FALSE)
}

#' @export
expect_flequal <- function(a,b,...){
    if(is.list(a))
        for(i in 1:length(a))
            expect_flequal(a[[i]],b[[i]],...)

    FLexpect_equal(a,b,...)
}

## Increase n for increasing length of FLVector.
## If isRowVec=TRUE, rowVector(one observation of all columns) is returned.
#' @export
initF.FLVector <- function(n,isRowVec=FALSE)
{
  sqlSendUpdate(getOption("connectionFL"),
                      c(paste0("DROP TABLE ",getOption("ResultDatabaseFL"),".test_vectortable_AdapteR;"),
                        paste0("CREATE TABLE ",getOption("ResultDatabaseFL"),".test_vectortable_AdapteR 
                          AS(SELECT 1 AS VECTOR_ID,a.serialval AS VECTOR_INDEX,
                            CAST(RANDOM(0,100) AS FLOAT)AS VECTOR_VALUE  
                          FROM ", getOption("ResultDatabaseFL"),".fzzlserial a 
                          WHERE a.serialval < ",ifelse(isRowVec,2,n+1),") WITH DATA ")))

  table <- FLTable(connection=getOption("connectionFL"),
                 getOption("ResultDatabaseFL"),
                 "test_vectortable_AdapteR",
                 "VECTOR_INDEX",
                 whereconditions=paste0(getOption("ResultDatabaseFL"),".test_vectortable_AdapteR.VECTOR_ID = 1")
                 )

  if(isRowVec)
  flv <- table[1,base::sample(c("VECTOR_VALUE","VECTOR_INDEX"),n,replace=TRUE)]
  else
  flv <- table[1:n,"VECTOR_VALUE"]

  Rvector <- as.vector(flv)
  return(list(FL=flv,R=Rvector))
}

## Increase the value of n to increase the dimensions of FLMatrix returned.
## Returns n*n or n*(n-1) based on isSquare.
#' @export
initF.FLMatrix <- function(n,isSquare=FALSE)
{
  sqlSendUpdate(getOption("connectionFL"),
                      c(paste0("DROP TABLE ", getOption("ResultDatabaseFL"),".test_matrixtable_AdapteR;"),
                        paste0("CREATE TABLE ",getOption("ResultDatabaseFL"),".test_matrixtable_AdapteR 
                          AS(SELECT 1 AS MATRIX_ID,a.serialval AS ROW_ID,
                            b.serialval AS COL_ID,CAST(random(0,100) AS FLOAT)AS CELL_VAL 
                          FROM ",getOption("ResultDatabaseFL"),".fzzlserial a,",getOption("ResultDatabaseFL"),".fzzlserial b
                          WHERE a.serialval < ",n+1," and b.serialval < ",ifelse(isSquare,n+1,n),") WITH DATA ")))
  flm <- FLMatrix(
      database          = getOption("ResultDatabaseFL"),
      table_name = "test_matrixtable_AdapteR",
      matrix_id_value   = 1,
      matrix_id_colname = "Matrix_ID",
      row_id_colname    = "Row_ID",
      col_id_colname    = "Col_ID",
      cell_val_colname  = "Cell_Val",
      connection=getOption("connectionFL"))
  Rmatrix <- as.matrix(flm)
  return(list(FL=flm,R=Rmatrix))
}

#' @export
initF.FLTable <- function(rows,cols)
{
  WideTable <- FLTable(connection=getOption("connectionFL"),
                      getOption("ResultDatabaseFL"),
                      "fzzlserial",
                      "serialval",
                      whereconditions=paste0(getOption("ResultDatabaseFL"),".fzzlserial.serialval<100"))
  return(WideTable[1:rows,base::sample(c("randval","serialval"),cols,replace=TRUE)])
}


##' initF.default helps to return a list of list.
##' Can be used for comparing results of R and FL functions which require two objects.

initF.default <- function(specs=list(c(n=5,isSquare = TRUE),c(n =5,isRowVec = FALSE)),
        classes = c("FLMatrix","FLVector")){
        #browser()
        l<-lapply(1:length(classes),function(x){
            #browser()
            I <- do.call(paste0("initF.",classes[x]),list(specs[[x]]))
            return(I)
            })
        FL <- lapply(1:length(l),function(x){
                    #browser()
                    if(classes[x] %in% c("FLMatrix","FLVector","FLTable"))
                    subscript <- "FL"
                    else subscript <- "R"
                    return(do.call("$",list(l[[x]],subscript)))
            })
        R <- lapply(1:length(l),function(x)l[[x]]$"R")
        return(list(FL=FL,R=R)) 
        }
##' tests if a R matrix is correctly stored and
##' represented when casting the R matrix into FLMatrix
##' and correctly recieved back, when cast to a vector.
##' checking dimnames, checking for subsetting.
##' For an optical check, both matrices are printed.
##' 
##' @param a an R Matrix
##' @author  Gregor Kappler <g.kappler@@gmx.net>
##' @export
expect_equal_RMatrix_FLMatrix <- function(a){
    # browser()
    debugOld <- getOption("debugSQL")
    options(debugSQL=FALSE)
    b <- as.FLMatrix(a)
    a <- Matrix(a)
    options(debugSQL=debugOld)
    expect_equal_Matrix(a,b,
                        "cast Matrix equal")

    test_Matrix_Subsetting(a,b,"as.FLMatrix")
}

##' converts FLMatrix to r matrix and checks if
##' recursive identical subsetting results in identical
##' matrices.
##'
##' @param b FLMatrix
##' @author  Gregor Kappler <g.kappler@@gmx.net>
##' @export
test_equal_FLMatrix_RMatrix<- function(b){
    # browser()
    debugOld <- getOption("debugSQL")
    options(debugSQL=FALSE)
    a <- as.matrix(b)
    options(debugSQL=debugOld)
    expect_equal_Matrix(a,b,
                        "cast Matrix equal")

    test_Matrix_Subsetting(a,b,"as.FLMatrix")
}


##' tests matrix subsetting by names and by index recursively.
##' 
##' @param a 
##' @param b 
##' @param desc 
##' @author  Gregor Kappler <g.kappler@@gmx.net>
##' @export
test_Matrix_Subsetting <- function(a,b, desc=""){
    if(nrow(a)<3) return()
    nr <- nrow(a) -2 ##%/% 2
    nc <- ncol(a) -2 ## %/% 2
    rowi <- sample(1:nrow(a),nr)
    coli <- sample(1:ncol(a),nc)
    asel <- a[rowi,coli,drop=FALSE]
    bsel <- b[rowi,coli,drop=FALSE]
    expect_equal_Matrix(asel,bsel,
                        paste0(
                            "subset by index of ",
                            desc))
    ## recursively test!
    test_Matrix_Subsetting(asel,bsel,
                        paste0(
                            "indexed subset of ",
                            desc))
    
    if(!is.null(rownames(a)))
        rowi <- sample(rownames(a),nr)
    if(!is.null(colnames(a)))
        coli <- sample(colnames(a),nc)
    asel <- a[rowi,coli,drop=FALSE]
    bsel <- b[rowi,coli,drop=FALSE]
    expect_equal_Matrix(asel,bsel,
                        paste0(
                            "subset by names of ",
                            desc))
    ## recursively test!
    test_Matrix_Subsetting(asel,bsel,
                        paste0(
                            "named subset of ",
                            desc))
}

##' @export
expect_equal_Matrix <- function(a,b,desc="",debug=TRUE){
    if(debug==TRUE){
        cat("\n-------------- ",desc,"\nR Matrix Object:\n")
        print(a)
        cat("\nFL Matrix Object:\n")
        print(b)
    }
    stripNames <- function(x) {
        if(is.null(x)) return(NULL)
        if(is.numeric(x) & all(x==as.numeric(names(x))))
            x <- NULL
        else 
            names(x) <- NULL
        if(is.list(x)) x <- llply(x,stripNames)
        ##if(is.null(unlist(x))) x <- NULL
        x
    }
    test_that(desc,{
        testthat::expect_equal(dimnames(a),stripNames(dimnames(b)))
        testthat::expect_equal(rownames(a),stripNames(rownames(b)))
        testthat::expect_equal(colnames(a),stripNames(colnames(b)))
        testthat::expect_equal(as.vector(a),as.vector(b))
    })
}

expect_equal_RVector_FLVector <- function(a){
    # browser()
    debugOld <- getOption("debugSQL")
    options(debugSQL=FALSE)
    b <- as.FLVector(a)
    options(debugSQL=debugOld)
    expect_equal_Vector(a,b,
                        "cast Vector equal")

    test_Vector_Subsetting(a,b,"as.FLVector")
    if(!is.null(names(a)))
    test_Vector_Subsetting(a,b,"as.FLVector",index=FALSE)
}

##' tests vector subsetting by names and by index recursively.
##' 
##' @param a 
##' @param b 
##' @param desc 
##' @author  Gregor Kappler <g.kappler@@gmx.net>
##' @export
test_Vector_Subsetting <- function(a,b, desc="",index=TRUE){
    if(length(a)<3) return()
    len <- length(a)-2
    if(index){
      leni <- sample(1:length(a),len)
      cat("index is ... ",leni,"\n")
      asel <- a[leni]
      bsel <- b[leni]
      expect_equal_Vector(asel,bsel,
                          paste0(
                              "subset by index of ",
                              desc))
      ## recursively test!
      test_Vector_Subsetting(asel,bsel,
                            paste0(
                              "indexed subset of ",
                              desc))
    }
    else{
      leni <- sample(names(a),len)
      cat("index is ... ",leni,"\n")
      asel <- a[leni]
      bsel <- b[leni]
      expect_equal_Vector(asel,bsel,
                          paste0(
                              "subset by names of ",
                              desc))
      ## recursively test!
      test_Vector_Subsetting(asel,bsel,
                          paste0(
                              "named subset of ",
                              desc))
    }
}

##' @export
expect_equal_Vector <- function(a,b,desc="",debug=TRUE){
    if(debug==TRUE){
        cat("\n-------------- ",desc,"\nR vector Object:\n")
        print(a)
        cat("\nFLVector Object:\n")
        print(b)
    }
    stripNames <- function(x) {
        if(is.null(x)) return(NULL)
        if(is.numeric(x) & all(x==as.numeric(names(x))))
            x <- NULL
        else 
            names(x) <- NULL
        if(is.list(x)) x <- llply(x,stripNames)
        ##if(is.null(unlist(x))) x <- NULL
        x
    }
    test_that(desc,{
        testthat::expect_equal(names(a),stripNames(names(b)))
        testthat::expect_equal(a,as.vector(b))
    })
}

initF.numeric <- initF.FLVector
initF.data.frame <- initF.FLTable
initF.matrix <- initF.FLMatrix