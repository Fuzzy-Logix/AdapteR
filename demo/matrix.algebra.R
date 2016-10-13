## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.

## This demo shows how the
## AdapteR package of Fuzzy Logix is
## providing transparent matrix algebra with
## the DB Lytix(TM) in-database library.
if(!exists("connection")) {
    vtemp <- readline("Check if connection exists:")
    cat("connection object not found \n ")
    cat("using connecting demo \n ")
    demo("connecting", package="AdapteR")
}

vtemp <- readline("This demo is about transparent in-database matrix algebra.\nPress <ENTER> to start.")

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

print(vresult)
vtemp <- readline("Ceck above that Matrix inversion output (SQL from R) is in deep format!\nPress <ENTER> to continue.")

m <- FLMatrix(table_name        = "tblMatrixMulti",
              matrix_id_colname = "Matrix_ID",
              matrix_id_value   = "5",
              row_id_colname    = "Row_ID",
              col_id_colname    = "Col_ID",
              cell_val_colname  = "Cell_Val")
vtemp <- readline("You can use AdapteR with R syntax for inversion.  Above we define the remote matrix.\nPress <ENTER> to continue.")

ms <- solve(m)
vtemp <- readline("Compute the inverse in-database with R syntax.\nPress <ENTER> to continue.")

print(class(ms))
vtemp <- readline("Output is also a FLMatrix and data is not fetched.\nPress <ENTER> to fetch and show the data.")
ms

vtemp <- readline("\nPress <ENTER> to check with the R in-memory computation after fetching the matrix with a cast.")

## Casting downloads data of the matrix and creates a R in-memory object
rm <- as.matrix(m)

## compute and print inverse in R
solve(rm)

## print computed inverse in FL
ms


vtemp <- readline("In-databse matrix multiplication with inverse results in the identity matrix:")
round(m %*% ms,3)

require(testthat)
expect_equal(as.matrix(ms),
             solve(rm),
             check.attributes=FALSE)
vtemp <- readline("AdapteR uses unit-tests with expect_equal to check if R and DB Lytix results match up for most of the R documentation examples of supported functions.\nPress <ENTER> to continue.")

flM <- as.FLMatrix(matrix(runif(25),5))
vtemp <- readline("Casting functions let you push an R matrix into database.\nPress <ENTER> to continue.")

result <- flM + flM
vtemp <- readline("Operator override is used to do in-database addition of FLMatrices.\nPress <ENTER> to show the result.")

result
vtemp <- readline("Result is fetched only when printed:")

result1 <- flM + flM/flM - flM*flM
result2 <- flM %*% flM
result <- result1+result2
vtemp <- readline("Operator override is used to do in-database addition of FLMatrices.\nPress <ENTER> to show the result.")

result
vtemp <- readline("Result is fetched only when printed:")

rM <- as.matrix(flM)
flResult <- solve(flM) %*% flM - flM
rResult <- solve(rM) %*% rM -rM

flResult

rResult
vtemp <- readline("Check if Results match up. \nPress <ENTER> to end the demo.")
### END ###
### Thank You ####
