require(testthat)

expect_eval_equal <- function(initF,FLcomputationF,RcomputationF,benchmark=FALSE,...)
{
  I <- initF(...)
    expect_equal(FLcomputationF(I$FL),
                 RcomputationF(I$R),
                 check.attributes=FALSE)
}

expect_flequal <- function(a,b,...){
    if(is.list(a))
        for(i in 1:length(a))
            expect_flequal(a[[i]],b[[i]],...)

    expect_equal(a,b,...)
}

## Increase n for increasing length of FLVector.
## If isRowVec=TRUE, rowVector(one observation of all columns) is returned.
initF.FLVector <- function(n,isRowVec=FALSE)
{
  sqlSendUpdate(connection,
                      c(paste0("DROP TABLE FL_DEMO.test_vectortable_AdapteR;"),
                        paste0("CREATE TABLE FL_DEMO.test_vectortable_AdapteR 
                          AS(SELECT 1 AS VECTOR_ID,a.serialval AS VECTOR_INDEX,
                            CAST(RANDOM(0,100) AS FLOAT)AS VECTOR_VALUE  
                          FROM FL_DEMO.fzzlserial a 
                          WHERE a.serialval < ",ifelse(isRowVec,2,n+1),") WITH DATA ")))

  table <- FLTable(connection,
                 "FL_DEMO",
                 "test_vectortable_AdapteR",
                 "VECTOR_INDEX",
                 whereconditions=paste0("FL_DEMO.test_vectortable_AdapteR.VECTOR_ID = 1")
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
initF.FLMatrix <- function(n,isSquare=FALSE)
{
  sqlSendUpdate(connection,
                      c(paste0("DROP TABLE FL_DEMO.test_matrixtable_AdapteR;"),
                        paste0("CREATE TABLE FL_DEMO.test_matrixtable_AdapteR 
                          AS(SELECT 1 AS MATRIX_ID,a.serialval AS ROW_ID,
                            b.serialval AS COL_ID,CAST(random(0,100) AS FLOAT)AS CELL_VAL 
                          FROM FL_DEMO.fzzlserial a,FL_DEMO.fzzlserial b
                          WHERE a.serialval < ",n+1," and b.serialval < ",ifelse(isSquare,n+1,n),") WITH DATA ")))
  flm <- FLMatrix(connection,
              database          = "FL_DEMO",
              table_name = "test_matrixtable_AdapteR",
              matrix_id_value   = 1,
              matrix_id_colname = "Matrix_ID",
              row_id_colname    = "Row_ID",
              col_id_colname    = "Col_ID",
              cell_val_colname  = "Cell_Val")
  Rmatrix <- as.matrix(flm)
  return(list(FL=flm,R=Rmatrix))
}

initF.FLTable <- function(rows,cols)
{
  WideTable <- FLTable(connection, 
                      "FL_DEMO",
                      "fzzlserial",
                      "serialval",
                      whereconditions=paste0("FL_DEMO.fzzlserial.serialval<100"))
  return(WideTable[1:rows,base::sample(c("randval","serialval"),cols,replace=TRUE)])
}



setMethod("expect_equal",signature("FLMatrix","matrix"),
          function(object,expected,...) expect_equal(as.matrix(object),expected,...))
setMethod("expect_equal",signature("FLMatrix","FLMatrix"),
          function(object,expected,...) expect_equal(as.matrix(object),as.matrix(expected),...))
setMethod("expect_equal",signature("dgCMatrix","FLMatrix"),
          function(object,expected,...) expect_equal(object,as.matrix(expected),...))

setMethod("expect_equal",signature("FLVector","vector"),
          function(object,expected,...) expect_equal(as.vector(object),expected,...))
setMethod("expect_equal",signature("FLVector","FLVector"),
          function(object,expected,...) expect_equal(as.vector(object),as.vector(expected),...))
setMethod("expect_equal",signature("matrix","matrix"),
          function(object,expected,...) testthat::expect_equal(as.vector(object),as.vector(expected),...))

setMethod("expect_equal",signature("list","list"),
          function(object,expected,...)
              llply(names(object),
                    function(i)
                        expect_equal(object[[i]],expected[[i]],...)))


require(testthat)

##' tests if a R matrix is correctly stored and
##' represented when casting the R matrix into FLMatrix
##' and correctly recieved back, when cast to a vector.
##' checking dimnames, checking for subsetting.
##' For an optical check, both matrices are printed.
##' 
##' @param a an R Matrix
##' @author  Gregor Kappler <g.kappler@@gmx.net>
test_equal_RMatrix_FLMatrix<- function(a){
    # browser()
    debugOld <- getOption("debugSQL")
    options(debugSQL=FALSE)
    b <- as.FLMatrix(a,connection)
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


##' 
##'
##' 
##' @param a 
##' @param b 
##' @param desc 
##' @return 
##' @author  Gregor Kappler <g.kappler@@gmx.net>
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
        expect_equal(dimnames(a),stripNames(dimnames(b)))
        expect_equal(rownames(a),stripNames(rownames(b)))
        expect_equal(colnames(a),stripNames(colnames(b)))
        expect_equal(as.vector(a),as.vector(b))
    })
}


