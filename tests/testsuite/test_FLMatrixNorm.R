
#Initialized a matrix from tblMatrixMulti of database for testing FLMatrixNorm.

testmatrix <- FLMatrix(getOption("ResultDatabaseFL"), 
"tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")

#Presently, there are 4 Norm methods for FLMatrixNorm.
#Every result using different Norm method  would be printed in following test code.
#Class of every result would also be printed.

test_that("Check for FLMatrixNorm with different norm methods",{
    lapply(as.list(1:4),function(x){
        result = FLMatrixNorm(testmatrix,x)
        ##print(paste0("Result with norm method to be ",x,": ",result))
        ##print(paste0("Class of the result is : ",class(result)))
    })
})

## Testing FLMatrixNorm
test_that("check FLMatrixNorm working",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
  FLMatrixNorm(M,3)
})
