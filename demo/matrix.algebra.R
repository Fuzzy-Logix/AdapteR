## Demo In-Database Matrix Algebra ##
if(!exists("connection")) {
  stop("Please run demo(connecting) to create connection object \n")
}
options(debugSQL=FALSE)
##############################################################################
m <- FLMatrix(table_name        = getTestTableName("tblMatrixMulti"),
              matrix_id_colname = "Matrix_ID",
              matrix_id_value   = "5",
              row_id_colname    = "Row_ID",
              col_id_colname    = "Col_ID",
              cell_val_colname  = "Cell_Val")
vtemp <- readline("You can use AdapteR with R syntax for inversion.  Above we define the remote matrix.\n")

ms <- solve(m)
ms
vtemp <- readline("Above: Compute the inverse in-database with R syntax.\nPress <ENTER> to continue.")

rm <- as.matrix(m)
solve(rm)
vtemp <- readline("Above: Compute the inverse in R \n ")

round(m %*% ms,3)
vtemp <- readline("In-databse matrix multiplication with inverse results in the identity matrix: \n ")

flM <- as.FLMatrix(matrix(runif(25),5))
vtemp <- readline("Casting functions let you push an R matrix into database.\n ")

result <- flM + flM
vtemp <- readline("Operator override is used to do in-database addition of FLMatrices.\n ")

result
vtemp <- readline("Result is fetched only when printed: \n")

result1 <- flM + flM/flM - flM*flM
result2 <- flM %*% flM
result <- result1+result2
vtemp <- readline("Several Arithmetic operations can be done \n ")

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
