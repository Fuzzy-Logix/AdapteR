## This script is for documentation. 
## startup and setup:
source("./setup-jdbc.R")
source("./FLtestLib.R")

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
matrixCharChar <- matrix(rnorm(25),5)
rownames(matrixCharChar) <- c("a","b","c","d","e")
colnames(matrixCharChar) <- c("p","q","r","s","t")

## numeric rownames (arbitrary unique) and character colnames
matrixNumChar <- matrix(1:25,5)
rownames(matrixNumChar) <- c(9,6,5,2,1)
colnames(matrixNumChar) <- c("p","q","r","s","t")


##source("/Users/gregor/fuzzylogix/AdapteR/RWrappers/AdapteR/R/FLMatrix.R")
##source("/Users/gregor/fuzzylogix/AdapteR/RWrappers/AdapteR/R/FLSubsetting.R")

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
eqnRtn <- FLMatrix(connection,
              database          = "FL_DEMO",
              table_name  = "finEquityReturns",
              matrix_id_value   = "",
              matrix_id_colname = "",
              row_id_colname    = "TxnDate",
              col_id_colname    = "TickerSymbol",
              cell_val_colname  = "EquityReturn")


test_equal_FLMatrix_RMatrix(eqnRtn[sample(rownames(eqnRtn),10),
                                   sample(colnames(eqnRtn),10)])

test_that("Named matrix rows and columns",{

    a <- eqnRtn[2001:2010,"MSFT"]
    b <- eqnRtn[2001:2010,"ORCL"]
    a2 <- eqnRtn[2011:2020,"MSFT"]
    b2 <- eqnRtn[2011:2020,"ORCL"]
    
    cat(constructSelect(a))


##############################
    ## bind for matrices with character dimnames
    ## note: no data movement.
    ab <- cbind(a,b)
    
    cat(constructSelect(ab))

    as.matrix(ab)

    ## note: currently only works for unique row and col ids (dimnames)
    dimnames(ab)

    test_equal_FLMatrix_RMatrix(ab)
    ab[1,1]
    
    expect_equal(
        dim(ab),
        c(nrow(a), ncol(a)+ncol(b)))

    ## cbind of 2 rbinds:
    a2b2 <- cbind(a2,b2)
    AB <- rbind(ab, a2b2)
    dimnames(AB)
    cat(constructSelect(AB))

    expect_equal(dim(AB),
                 c(nrow(a) + nrow(a2),
                   ncol(a) + ncol(b)))
    
    AB
    ##ABs <- store(AB)
})

test_that("Named matrix rows and columns",{
    
    cat(constructSelect(a,"a"))


##############################
    ## bind for matrices with character dimnames
    ## note: no data movement.
    ab <- cbind(a,b)
    
    cat(constructSelect(ab))

    ab

    ## note: currently only works for unique row and col ids (dimnames)
    dimnames(ab)


    expect_equal(
        dim(ab),
        c(nrow(a), ncol(a)+ncol(b)))

    ## cbind of 2 rbinds:
    a2b2 <- cbind(a2,b2)
    AB <- rbind(ab, a2b2)
    dimnames(AB)
    cat(constructSelect(AB))

    expect_equal(dim(AB),
                 c(nrow(a) + nrow(a2),
                   ncol(a) + ncol(b)))
    
    AB
    ##ABs <- store(AB)
})




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
