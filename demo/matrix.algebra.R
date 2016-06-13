## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.

## This demo shows how the
## AdapteR package of Fuzzy Logix is
## providing transparent matrix algrbra with
## the DB Lytix(TM) in-database library.

if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}

###########################################################
##
## Matrix Inversion
##
## The SQL-through R way a la Manual:
sqlQuery(connection, "
WITH z (Matrix_ID, Row_ID, Col_ID, NumVal) AS
(
SELECT a.Matrix_ID,
       a.Row_ID,
       a.Col_ID,
       a.Cell_Val
FROM fl_dev.tblMatrixMulti a
WHERE a.Matrix_ID = 5
)
SELECT a.*
FROM TABLE (
     FLMatrixInvUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.NumVal) HASH BY z.Matrix_ID
  LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
) AS a
ORDER BY 1,2,3;")

readline("You can use AdapteR to use R syntax for inversion.  First we define the remote matrix:")
m <- FLMatrix(database          = "FL_DEMO",
              table_name        = "tblMatrixMulti",
              matrix_id_colname = "Matrix_ID",
              matrix_id_value   = "5",
              row_id_colname    = "Row_ID",
              col_id_colname    = "Col_ID",
              cell_val_colname  = "Cell_Val")

readline("Compute the inverse in-database:")
solve(m)

readline("You can check with the R in-memory computation after fetching the matrix with a cast:")
rm <- as.matrix(m)
## compute inverse in R after
solve(rm)


readline("in-databse matrix multiplication with inverse
results in the identity matrix:")
ms <- solve(m)
round(m %*% ms,3)

## check is R and DB Lytix results match up:
m.r <- as.matrix(m) ## download and convert to R matrix
expect_equal(as.matrix(ms),
             solve(m.r),check.attributes=FALSE)


flM <- as.FLMatrix(matrix(runif(25),5))

flM

flM + flM

flM - flM

flM / flM

a <- (flM %*% flM) - flM
a

solve(flM) %*% flM - flM
