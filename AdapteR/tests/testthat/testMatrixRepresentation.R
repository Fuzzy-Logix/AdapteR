## This script is for documentation. 
##
## R has very nice vector and matrix syntax.
## AdapteR mimics the parsimonious syntax.
##
## Make sure your working dir is where you unpacked
## the zip dir.
## 
##
## devtools::document()
## devtools::load_all(".")
## setwd("tests/testthat/") ## here reside the demo scripts


require(RODBC)
connection <- odbcConnect("Gandalf")
##
## OR
##
## startup and setup with jdbc:
##
require(RJDBC)
user     <- "database user"
passwd   <- "database password" 
host     <- "10.200.4.116" ## Gandalf
database <- "Fl_demo"
source("./setup-jdbc.R")


source("./FLtestLib.R")


## a in-memory matrix in R 
(m <- rMatrix <- matrix(1:25,5))


## Subsetting a row
m[1,]

## Subsetting a column
m[,1]

## Subsetting a part of a matrix
m[2:5,4:5]

##################################################################
## converting the R matrix into an in-DB object
## (data is transfered through network)
## (and refetched for printing)
##
(m <- flMatrix <- as.FLMatrix(rMatrix,
                              connection))


## you can run above functions on m=flMatrix again with identical results!
## Subsetting a row
m[1,]

## Subsetting a column
m[,1]

## Subsetting a part of a matrix
m[2:5,4:5]


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



## inspect names representation in AdapteR
{ ## 
    flm <- as.FLMatrix(matrixCharChar,connection)
    flm
    
    rownames(matrixCharChar)
    ## the rownames have an index that is the mapped
    ## numeric index in-database.
    ## These indices are a character vector
    ## (an R restriction).
    rownames(flm)
    names(rownames(flm))

    ## Names are selected
    matrixCharChar[4:5,4:5]
    flm[4:5,4:5]

    matrixNullChar
    flm <- as.FLMatrix(matrixNullChar,connection)
    flm
    ## R looses the indexes if no names are set.
    ## (Note that rows start at 1)
    matrixNullChar[3:5,3:5]
    ## FLMatrix keeps the index because otherwise
    ## it would loose reference.
    flm[3:5,3:5]

    ## A technical note on joining the names of rows and columns in SQL:
    ## constructSelect by default does join the rownames
    cat(constructSelect(flm))
    ## constructSelect by default does join the rownames
    cat(constructSelect(flm,joinNames=FALSE))
}



#############################################################
## Hierachical test suite of selects of selects...
## of above mentioned matrix names combinations:
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




## For in-database analytics the matrix is in the warehouse
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

## Hierachical tests of a name-providing matrix
test_equal_FLMatrix_RMatrix(eqnRtn[sample(rownames(eqnRtn),10),
                                   sample(colnames(eqnRtn),10)])


test_that("Binding named (not indexed) matrix rows and columns",{
    ##
    a <- eqnRtn[2001:2010,"MSFT"]
    b <- eqnRtn[2001:2010,"ORCL"]
    a2 <- eqnRtn[2011:2020,"MSFT"]
    b2 <- eqnRtn[2011:2020,"ORCL"]
    ##
    ##
    ra <- as.matrix(a)
    rb <- as.matrix(b)
    ra2 <- as.matrix(a2)
    rb2 <- as.matrix(b2)
    ##
    ## 
    ## note: no data movement.
    ab <- cbind(a,b)
    expect_equal_Matrix(ab,cbind(ra,rb))
    ##
    ##cat(constructSelect(ab))
    ##
    a2b2 <- cbind(a2,b2)
    expect_equal_Matrix(a2b2,cbind(ra2,rb2))
    ##
    ## rbind of 2 cbinds:
    AB <- rbind(ab, a2b2)
    expect_equal_Matrix(AB,rbind(cbind(ra,rb),cbind(ra2,rb2)))
    AB
})





