## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.

vtemp <- readline("What this demo is about:")
## This demo shows how the
## AdapteR package of Fuzzy Logix is
## providing transparent matrix algrbra with
## the DB Lytix(TM) in-database library.

if(!exists("connection")) {
    vtemp <- readline("Check if connection exists:")
    cat("connection object not found \n ")
    cat("using connecting demo \n ")
    demo("connecting", package="AdapteR")
}

vtemp <- readline("Comparing (SQL from R) with (AdapteR):")
###########################################################
############# Comparing (SQL from R) with AdapteR ############
##
## Matrix Inversion
##
## The SQL from R way.SQL taken from Manual:
vresult <- sqlQuery(connection, "
WITH z (Matrix_ID, Row_ID, Col_ID, NumVal) AS
(
SELECT a.Matrix_ID,
       a.Row_ID,
       a.Col_ID,
       a.Cell_Val
FROM fl_TRAIN.tblMatrixMulti a
WHERE a.Matrix_ID = 5
)
SELECT a.*
FROM TABLE (
     FLMatrixInvUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.NumVal) HASH BY z.Matrix_ID
  LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
) AS a
ORDER BY 1,2,3;")

vtemp <- readline("Output of (SQL from R):")
print(vresult)

vtemp <- readline("You can use AdapteR with R syntax for inversion.  First we define the remote matrix:")
m <- FLMatrix(table_name        = "tblMatrixMulti",
              matrix_id_colname = "Matrix_ID",
              matrix_id_value   = "5",
              row_id_colname    = "Row_ID",
              col_id_colname    = "Col_ID",
              cell_val_colname  = "Cell_Val")

vtemp <- readline("Compute the inverse in-database:")
ms <- solve(m)

vtemp <- readline("Output is also a FLMatrix and data is not fetched:")
print(class(ms))

vtemp <- readline("You can check with the R in-memory computation after fetching the matrix with a cast:")
rm <- as.matrix(m)
## compute inverse in R after
solve(rm)
ms


vtemp <- readline("in-databse matrix multiplication with inverse
results in the identity matrix:")
round(m %*% ms,3)

vtemp <- readline("check if R and DB Lytix results match up:")
expect_equal(as.matrix(ms),
             solve(rm),
             check.attributes=FALSE)

vtemp <- readline("Push R matrix into database:")
flM <- as.FLMatrix(matrix(runif(25),5))
vtemp <- readline("Print the matrix:")
flM

vtemp <- readline("in-database addition of FLMatrices:")
result <- flM + flM

vtemp <- readline("Result is fetched only when printed:")
result

vtemp <- readline("in-database solution of expressions:")
result1 <- flM + flM/flM - flM*flM
result2 <- flM %*% flM
result <- result1+result2

vtemp <- readline("Result is fetched only when printed:")
result

vtemp <- readline("Check if Results match up:")
rM <- as.matrix(flM)
flResult <- solve(flM) %*% flM - flM
rResult <- solve(rM) %*% rM -rM

require(testthat)
FLexpect_equal(flResult,
             rResult,
             check.attributes=FALSE)

### END ###
### Thank You ####
