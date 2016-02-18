## This script is for documentation. 
## startup and setup:
source("./setup-jdbc.R")

## there are different ways to index matrices in R
## no dimnames, just proper 1:N numeric indices:
matrixNullNull <- matrix(1:25,5)

## character rownames, no colnames, just proper 1:N numeric indices:
matrixCharNull <- matrix(1:25,5)
rownames(matrixCharNull) <- rev(c("a","b","c","d","e"))


## character colnames, no rownames, just proper 1:N numeric indices:
matrixNullChar <- matrix(1:25,5)
colnames(matrixNullChar) <- rev(c("a","b","c","d","e"))

## character rownames and colnames
matrixCharChar <- matrix(1:25,5)
rownames(matrixCharChar) <- c("a","b","c","d","e")
colnames(matrixCharChar) <- c("p","q","r","s","t")

## numeric rownames (arbitrary unique) and character colnames
matrixNumChar <- matrix(1:25,5)
rownames(matrixNumChar) <- c(9,6,5,2,1)
colnames(matrixNumChar) <- c("p","q","r","s","t")


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



source("/Users/gregor/fuzzylogix/AdapteR/RWrappers/AdapteR/R/FLMatrix.R")
source("/Users/gregor/fuzzylogix/AdapteR/RWrappers/AdapteR/R/FLSubsetting.R")

options(debugSQL=FALSE)
test_equal_RMatrix_FLMatrix(matrixCharChar)
test_equal_RMatrix_FLMatrix(matrixNumChar)

## Note: subsetting an unnamed R Matrix will
## result in persistent dim-names needed for
## db reference
rownames(matrixNullChar)
test_equal_RMatrix_FLMatrix(matrixNullNull)
test_equal_RMatrix_FLMatrix(matrixCharNull)
test_equal_RMatrix_FLMatrix(matrixNullChar)




#############################################################
## For in-database analytix the matrix is in the warehouse
## to begin with.
## Create a remote matrix object
##
m <- FLMatrix(connection,
              database          = "FL_DEMO",
              table_name  = "finEquityReturns",
              matrix_id_value   = "",
              matrix_id_colname = "",
              row_id_colname    = "TxnDate",
              col_id_colname    = "TickerSymbol",
              cell_val_colname  = "EquityReturn")


test_equal_FLMatrix_RMatrix(m[sample(rownames(m),10),
                              sample(colnames(m),10)])





if(FALSE){ ## debugging code cruft



    rownames(fl[4:5,4:5])
    ## play
    flm <- as.FLMatrix(matrixCharChar,connection)
    flm
    dbGetQuery(connection,constructSelect(flm))
    rownames(matrixCharNull)
    matrixNullChar[4:5,4:5]
    flm[4:5,4:5]


    matrixNullChar[3:5,3:5][1,]


    ## play
    flm <- as.FLMatrix(matrixNullNull,connection)
    cat(constructSelect(flm))
    flm[1:3,2:4]


    ## play
    flm <- as.FLMatrix(matrixCharNull,connection)
    flm
    options(debugSQL=TRUE)
    flm[c("a","c"),]
    flm[2:4,][1,4:5]
    flm2 <- FLMatrix(connection,
                 flm@select@database,
                 flm@select@table_name,
                 flm@mapSelect@table_name,
                 5,"MATRIX_ID"
                 )

    flm2[,2]
    rownames(matrixCharNull)
}

FLjoin <- function(sfs, ## named list of SelectFroms
                   byconditions, 
                   variables=unlist(llply(names(sfs),function(sfn){
                       names(sf@variables) <- paste0(sfn,"_",names(sf@variables))
                       return(sf@variables)
                   }))){
    wcs <- unlist(llply(names(sfs),function(sfn){
                       return(constructSelect())
                   }))
    select <- new("FLSelectFrom",
                  connection = sfs[[1]]@connection,
                  variables=variables,
                  whereconditions=c(),
                  order = "character",
                  database = "character",
                  table_name = "character"
                  )
}
