#Function not in R
#FLMatrixRREF

flmatrix <- as.FLMatrix(matrix(rnorm(80),10,8))
flresult <- FLMatrixREF(flmatrix)
flresult <- as.matrix(flresult)

test_that("testing RREF conditions ",{
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

  vidx <- vidx[!is.infinite(vidx)]
  t<-sapply(2:length(vidx),
            function(x){
                if(!all(flresult[1:x-1,vidx[x]]==0))
                stop("error in RREF")
          })
  })


test_that( "testing the example written in FLMatrixRREF",{
    flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"),
                          5,
                          "MATRIX_ID",
                          "ROW_ID",
                          "COL_ID",
                          "CELL_VAL")
    resultFLMatrix <- FLMatrixRREF(flmatrix)
    ##    print(resultFLMatrix)
 })

## Testing FLMatrixRREF
test_that("check FLMatrixRREF working",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
  FLexpect_equal(
      dim(FLMatrixRREF(M)),
      dim(M)
  )
  FLexpect_equal(
      dimnames(FLMatrixRREF(M)),
      dimnames(M)
  )
})

