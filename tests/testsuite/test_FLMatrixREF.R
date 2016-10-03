#Function not in R
#FLMatrixREF

flmatrix <- as.FLMatrix(matrix(rnorm(80),10,8))
flresult <- FLMatrixREF(flmatrix)
flresult <- as.matrix(flresult)

test_that("testing REF conditions ",{
  vidx <- apply(flresult,1,function(x){
                vidx1 <- suppressWarnings({min(which(x==1))})
                if(vidx1 > 1 
                  && !is.infinite(vidx1) 
                  && !all(x[1:vidx1-1]==0))
                stop("error in REF")
                return(vidx1)
          })
  if(!all(vidx==sort(vidx)))
  stop("error in REF")
})

test_that("testing the example written in FLMatrixREF",{
  flmatrix <- FLMatrix("tblMatrixMulti", 
                        5,
                        "MATRIX_ID",
                        "ROW_ID",
                        "COL_ID",
                        "CELL_VAL")
  resultFLMatrix <- FLMatrixREF(flmatrix)
})


## Testing FLMatrixREF
test_that("check FLMatrixREF",
{
    FLMatrixREF(initF.FLMatrix(n=5,isSquare=TRUE)$FL)
})
