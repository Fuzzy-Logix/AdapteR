
## a in-memory matrix in R
(m <- rMatrix <- matrix(1:25,5))

##################################################################
## converting the R matrix into an in-DB object
## (data is transfered through network)
## (and refetched for printing)
##
(m <- flMatrix <- as.FLMatrix(rMatrix))


## you can run above functions on m=flMatrix again with identical results!
## Subsetting a row
rMatrix[1,]
flMatrix[1,]

## Subsetting a column
rMatrix[,1]
flMatrix[,1]

## Subsetting a part of a matrix
rMatrix[2:5,4:5]

## Subsetting a part of a matrix
rMatrix[2:5,4:5]
flMatrix[2:5,4:5]
## note: the names are persistent in FLMatrix



## there are different ways to index matrices in R
## no dimnames, just proper 1:N numeric indices:
matrixNullNull <- matrix(1:25,5)

## character rownames, no colnames, just proper 1:N numeric indices:
matrixCharNull <- matrix(1:25,5)
rownames(matrixCharNull) <- rev(c("a","b","c","d","e"))
matrixCharNull

## character colnames, no rownames, just proper 1:N numeric indices:
matrixNullChar <- matrix(1:25,5)
colnames(matrixNullChar) <- rev(c("a","b","c","d","e"))
matrixNullChar
rownames(matrixNullChar)
colnames(matrixNullChar)

## character rownames and colnames
matrixCharChar <- matrix(1:25,5)
rownames(matrixCharChar) <- c("a","b","c","d","e")
colnames(matrixCharChar) <- c("p","q","r","s","t")
matrixCharChar

## numeric rownames (arbitrary unique) and character colnames
matrixNumChar <- matrix(1:25,5)
rownames(matrixNumChar) <- c(9,6,5,2,1)
colnames(matrixNumChar) <- c("p","q","r","s","t")
matrixNumChar


## inspect names representation in AdapteR
flm <- as.FLMatrix(matrixCharChar)
flm

rownames(matrixCharChar)

## the rownames have an index that is the mapped
## numeric index in-database.
## These indices are a character vector
## (an R restriction).
rownames(flm)
names(rownames(flm))

## Names are consistent
matrixCharChar[4:5,4:5]
flm[4:5,4:5]

matrixCharChar[c("a","b"),c("s","t")]
flm[c("a","b"),c("s","t")]

matrixNullChar
flm <- as.FLMatrix(matrixNullChar)
flm
## R looses the indexes if no names are set.
## (Note that rows start at 1)
matrixNullChar[3:5,3:5]
## FLMatrix keeps the index because otherwise
## it would loose reference.
flm[3:5,3:5]
## A technical note on joining the names of rows and columns in SQL:
## constructSelect by default does join the rownames
##cat(constructSelect(flm))
## constructSelect by default does join the rownames
##cat(constructSelect(flm,joinNames=FALSE))


########################################
## Quality control: extensive unit tests
test_that(
    "Named Matrices: Hierachical test suite of selects of selects...",
    {
    expect_equal_RMatrix_FLMatrix(matrixCharChar)
    expect_equal_RMatrix_FLMatrix(matrixNumChar)
    })

## Note: subsetting an unnamed R Matrix will
## result in persistent dim-names needed for
## db reference
rownames(matrixNullChar)
test_that(
    "Unnamed Matrices: Hierachical test suite of selects of selects...",
    expect_equal_RMatrix_FLMatrix(matrixNullNull))

test_that(
    "Partly named matrices: Hierachical test suite of selects of selects...",
    {
        expect_equal_RMatrix_FLMatrix(matrixCharNull)
        expect_equal_RMatrix_FLMatrix(matrixNullChar)
    })





###########################################################
##
## A look under the hood:
##
## memory consumption
##

## For in-database analytics the matrix is in the warehouse
## to begin with.
## Create a remote matrix object
##
if(!exists("eqnRtn"))
    eqnRtn <- FLMatrix(table_name  = getTestTableName("finEquityReturns"),
                       matrix_id_value   = "",
                       matrix_id_colname = "",
                       row_id_colname    = "TxnDate",
                       col_id_colname    = "TickerSymbol",
                       cell_val_colname  = "EquityReturn")

Nstocks <- 100
subEqnRtn <- eqnRtn[, sample(colnames(eqnRtn),Nstocks)]
rEqnRtn <- as.matrix(subEqnRtn)
dim(eqnRtn)
dim(subEqnRtn)

## ## only dimension names are in local memory:
## cat(paste0("Total client memory size for remote equity return table\n"))
## print(object.size(eqnRtn),units = "Kb")
## cat(paste0("dimnames client memory size for remote equity return table\n"))
## print(object.size(eqnRtn@dimnames),units = "Kb")
## cat(paste0("total client memory size for subset of remote equity return table\n"))
## print(object.size(subEqnRtn),units = "Kb")
## cat(paste0("dimnames client memory size for subset of remote equity return table\n"))
## print(object.size(subEqnRtn@dimnames),units = "Kb")

## ## Download a subset of the remote Table into R Memory
## ## rEqnRtn <- as.matrix(subEqnRtn)

## ## compare memory consumption:
## cat(paste0("dimnames client memory size for r matrix with subset of equity return table\n"))
## print(object.size(rEqnRtn),units = "Kb")





test_that("Casting base R matrix <---> in-database Matrices",{
    ## Creating simple base R matrix
    matrix1 <- matrix(1:25,5)
    matrix2 <- matrix(1:25,5)
    rownames(matrix1) <- c("a","b","c","d","e")
    colnames(matrix1) <- c("p","q","r","s","t")
    rownames(matrix2) <- c("A","B","C","D","E")
    colnames(matrix2) <- c("P","Q","R","S","T")
    ##  FLMatrices from R matrices
    m1 <- as.FLMatrix(matrix1)
    m2 <- as.FLMatrix(matrix2)
    expect_equal(dim(m1),c(5,5))
    expect_equal(dim(m2),c(5,5))
    expect_equal(as.vector(m1), as.vector(matrix1))
    expect_equal(as.vector(m2), as.vector(matrix2))
    ##
    ##
    ## FLMatrix -> R matrix
    matrix3 <- as.matrix(m2)
    expect_equal(dim(matrix3), c(5,5))
    expect_equal(as.vector(m2), as.vector(matrix3))
})


# ## Hierachical tests of a name-providing matrix
# test_equal_FLMatrix_RMatrix(eqnRtn[sample(rownames(eqnRtn),10),
#                                    sample(colnames(eqnRtn),10)])


# test_that("Binding named (not indexed) matrix rows and columns",{
#     ##
#     a <- eqnRtn[2001:2010,"MSFT"]
#     b <- eqnRtn[2001:2010,"ORCL"]
#     a2 <- eqnRtn[2011:2020,"MSFT"]
#     b2 <- eqnRtn[2011:2020,"ORCL"]
#     ##
#     ##
#     ra <- as.matrix(a)
#     rb <- as.matrix(b)
#     ra2 <- as.matrix(a2)
#     rb2 <- as.matrix(b2)
#     ##
#     ##
#     ## note: no data movement.
#     ab <- cbind(a,b)
#     expect_equal_Matrix(ab,cbind(ra,rb))
#     ##
#     ##cat(constructSelect(ab))
#     ##
#     a2b2 <- cbind(a2,b2)
#     expect_equal_Matrix(a2b2,cbind(ra2,rb2))
#     ##
#     ## rbind of 2 cbinds:
#     AB <- rbind(ab, a2b2)
#     expect_equal_Matrix(AB,rbind(cbind(ra,rb),cbind(ra2,rb2)))
#     AB
# })








## Testing FLMatrix Subsetting
test_that("check matrix subsetting",
{

    ## Testing result
    expect_eval_equal(initF=function(n,isSquare=FALSE) {
        a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a))
    },function(x) do.call("[",list(x,c("b","a"),2:1)),
    function(x) do.call("[",list(x,c("b","a"),2:1)),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a))
    },function(x) do.call("[",list(x)),
    function(x) do.call("[",list(x)),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a))
    },function(x) "["(x,c("b","c"),),
    function(x)"["(x,c("b","c"),),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a))
    },function(x) "["(x,,2:3),
    function(x) "["(x,,2:3),n=4
  )
   expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a))
    },function(x) dimnames("["(list(x,c("b","a"),2:1))),
    function(x) dimnames("["(list(x,c("b","a"),2:1))),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a))
    },function(x) dimnames("["(list(x))),
    function(x) dimnames("["(list(x))),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a))
    },function(x) dimnames("["(x,c("b","c"),)),
    function(x) dimnames("["(x,c("b","c"),)),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a))
    },function(x) dimnames("["(x,,2:3)),
    function(x) dimnames("["(x,,2:3)),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
        a <- FLMatrix(getTestTableName("tblmatrixMulti"),2,
                   "MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
                    dimnames=list(c("a","b","c"),1:3))
        list(R=as.matrix(a),
             FL=a)
    },function(x) do.call("[",list(x,c("b","a"),2:1)),
    function(x) do.call("[",list(x,c("b","a"),2:1)),n=6
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
        a <- FLMatrix(getTestTableName("tblmatrixMulti"),2,
                   "MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
                    dimnames=list(c("a","b","c"),1:3))
        list(R=as.matrix(a),
             FL=a)
    },function(x) do.call("[",list(x)),
    function(x) do.call("[",list(x)),n=6
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- FLMatrix(getTestTableName("tblmatrixMulti"),2,
                    "MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
                    dimnames=list(c("a","b","c"),1:3))

      list(R=as.matrix(a),
           FL=a)
    },function(x) "["(x,c("b","c"),),
    function(x)"["(x,c("b","c"),),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
        a <- FLMatrix(getTestTableName("tblmatrixMulti"),2,
                   "MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
                    dimnames=list(c("a","b","c"),1:3))
        list(R=as.matrix(a),
             FL=a)
    },function(x) "["(x,,2:3),
    function(x) "["(x,,2:3),n=4
  )
})



