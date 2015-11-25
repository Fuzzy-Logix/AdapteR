
test_that("subsetting matrices", {

    ## Testing Subsetting
    ## Non-symmetric singular matrix of dimension 5x5
    ## in R memory
    rMatrix <- matrix(1:25,5)
    rMatrix
    dim(rMatrix)
    diag(rMatrix)
    
    ## converting the R matrix into 
    ## an in-DB object,
    ## CAREFUL: DATA IS TRANSFERED THROUGH NETWORK
    m <- as.FLMatrix(rMatrix,connection)
    dim(m)
    ##diag(m)

    m1 <- m[1,]
    m[,1]
    m[2:3,4:5]

##############################ä
    ## GOAL:
    dbGetQuery(connection,"SELECT a.TickerSymbol AS Ticker1,
b.TickerSymbol AS Ticker2,
FLCorrel(a.EquityReturn, b.EquityReturn) AS FLCorrel
FROM finEquityReturns a,
finEquityReturns b
WHERE b.TxnDate = a.TxnDate
AND a.TickerSymbol = 'MSFT'
AND b.TickerSymbol IN ('AAPL','HPQ','IBM','MSFT','ORCL')
GROUP BY a.TickerSymbol, b.TickerSymbol
ORDER BY 1, 2;")
    
    eqnRtn <- FLMatrix(connection,database="FL_DEMO",
                       matrix_table="finEquityReturns",
                       matrix_id_value = "",
                       matrix_id_colname = "",
                       row_id_colname = "TickerSymbol",
                       col_id_colname = "TxnDate",
                       cell_val_colname = "EquityReturn")

    head(rownames(eqnRtn))
    
    E <- eqnRtn[1:10,1]
    eqnRtn["ROVI",1:10]

    eqnRtn[1:10,
           grep("2005-12.*",colnames(eqnRtn))]

    
    constructWhere(constraintsSQL(eqnRtn))
    constructWhere(constraintsSQL(E,"a"))
    
    myCorr <- cor(t(eqnRtn[c('HPQ','IBM','MSFT','ORCL'),]),
                  t(eqnRtn[c('HPQ','IBM','MSFT','ORCL'),]))
    myCorr
    
    require(corrplot)
    corrplot(myCorr)

    
    dbGetQuery(connection,"
show function FLCorrel")

    
    dbSendQuery(connection,"
DATABASE FL_DEV ")

    
    
    dbGetQuery(connection,"
WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS
(
SELECT a.Matrix_ID,
a.Row_ID,
a.Col_ID,
a.Cell_Val
FROM tblMatrixMulti a
WHERE a.Matrix_ID = 5
)
SELECT a.*
FROM TABLE (
FLMatrixDetUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val)
HASH BY z.Matrix_ID
LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
) AS a
ORDER BY 1;
")
    

    

    
    test_that("selection of entire matrix",
    {
        expect_true(
            is.FLMatrix(
                m[,]
            ))
        expect_true(nrow(m[,])==nrow(m))
        expect_true(ncol(m[,])==ncol(m))
    })
    
    test_that("selection of cell row 1, column 1",
    {
        expect_true(
            is.FLMatrix(
                m[1,1]
            ))
        expect_true(
            length(m[1,1])==1
        )
    })

test_that("selection of first row",
{
    expect_true(
        is.FLMatrix(
            m[1,]
        ))
    expect_true(
        length(m[1,])==ncol(m))
})

test_that("selection of row by name works",
          expect_equal(
            (m[1,] == m[rownames(m)[[1]],]),
            TRUE
              ))

test_that("selection of column by name works",
          expect_equal(
              (m[,1] == m[,colnames(m)[[1]]]),
              TRUE))

test_that("selection of a part of the matrix",
{
    expect_equal(3,
                 nrow(m[1:3,]))
    expect_equal(ncol(m),
                 ncol(m[1:3,]))
})

test_that("selection of a part of the matrix",
{
    expect_equal(3,
                 ncol(m[,1:3]))
    expect_equal(nrow(m),
                 nrow(m[,1:3]))
    expect_equal(rownames(m),
                 rownames(m[,1:3]))
})

test_that("selection of row by name works",
          expect_equal(
              (m[1:2,] == m[rownames(m)[1:2],]),
              TRUE
          ))

test_that("selection of column by name works",
          expect_equal(
              (m[,1:2] == m[,colnames(m)[1:2]]),
              TRUE
          ))

test_that("selection of entire vector",
{
    expect_true(
        is.FLVector(
            v1[]
        ))
expect_true(length(v1[])==length(v1))
})

test_that("selection of part of vector",
{
    expect_true(
        is.FLVector(
            v1[2:3]
        ))
expect_true(length(v1[2:3])==2)
})

test_that("selection of an element from a vector",
{
    expect_true(
        is.FLVector(
            v1[2]
        ))
expect_true(length(v1[2])==1)
})


# Testing Addition/Subtraction
test_that("check matrix addition/subtraction",
{
  m + m2
  m - m2 
})

test_that("check sparse matrix addition/subtraction",
{
  sm2 + sm2
  sm1 - sm1 
})

test_that("check vector addition/subtraction",
{
  v1 + v1
  v2 - v2 
})

test_that("check sum of matrices",
          expect_equal(
            ((m + m) == 2*m),
            TRUE
              ))

test_that("check sum of sparse matrices",
          expect_equal(
            ((sm2 + sm2) == 2*sm2),
            TRUE
              ))

test_that("check sum of vectors",
          expect_equal(
            ((v2 + v2) == 2*v2),
            TRUE
              ))

test_that("check commutative law",
          expect_equal(
            ((m2 + m) == (m + m2)),
            TRUE
              ))

test_that("check associative law for addition",
          expect_equal(
            (((m + m) + m2) == (m + (m + m2))),
            TRUE
              ))

test_that("check identity of addition",
          expect_equal(
            ( (m + m0) == (m0 + m) ),
            TRUE
              ))

# Testing Multiplication
test_that("check matrix multiplication",
{
  m * m2
  m3 %*% m2
  m %*% m2 
})

test_that("check associative law for multiplication",
          expect_equal(
            ( ((m3 %*% m) %*% m2) == (m3 %*% (m %*% m2)) ),
            TRUE
              ))

test_that("check two scalar multiplication for matrices",
          expect_equal(
            ( ((2 * 3) * m) == (2 * (3 * m)) ),
            TRUE
              ))

test_that("check two scalar multiplication for sparse matrices",
          expect_equal(
            ( ((2 * 3) * sm2) == (2 * (3 * sm2)) ),
            TRUE
              ))

test_that("check two scalar multiplication for vectors",
          expect_equal(
            ( ((2 * 3) * v1) == (2 * (3 * v1)) ),
            TRUE
              ))

test_that("check scalar multiplication for matrices",
          expect_equal(
            ( ((2 * m) %*% m2) == (2 * (m %*% m2)) ),
            TRUE
              ))

test_that("check distributive properties for matrices",
{
    expect_equal(
          ( (m3 %*% (m2 + m)) == ((m3 %*% m2) + (m3 %*% m)) ),
          TRUE
            )
    expect_equal(
          ( ((m2 + m) %*% m) == ((m2 %*% m) + (m %*% m)) ),
          TRUE
            )  
    expect_equal(
          ( (2 * (m2 + m)) == ((2 * m2) + (2 * m)) ),
          TRUE
            )  
    expect_equal(
          ( ((2 + 3) *m) == ((2 * m) + (3 * m)) ),
          TRUE
            )  
})          
