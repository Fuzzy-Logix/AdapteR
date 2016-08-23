#Asana: https://app.asana.com/0/143316600934101/148450351472400
#testing operators
## MOD result matching fails for -ve numbers
Renv <- new.env(parent = globalenv())
Renv$x<- -1:12
Renv$y <- 1:12
FLenv <- as.FL(Renv)
test_that(
  "Testing + ",
  {
    result1<-eval_expect_equal({test1<-(x+1)},Renv,FLenv)
  }
)

test_that(
  "Testing arithmetic ",
  {
    result2<-eval_expect_equal({test2<-2*x+3},Renv,FLenv)
  }
)

test_that(
  "Testing integer division ",
  {
    result4<-eval_expect_equal({test4<-(x%/%5)},Renv,FLenv)
  }
)

## different test for MOD as results wont match for -ve values
test_that(
  "Testing remainder MOD ",
  {
    result3<-eval_expect_equal({
      test3<-(y%%2)
      test5 <- length(x%%2)
      },Renv,FLenv)
  }
)

n <- 5
isSquare <- T

## Testing M_Subtraction
test_that("check result for Matrix M_Subtraction",
{
  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- initF.FLMatrix(n,isSquare)
      b <- FLMatrix("tblmatrixMulti",
                    5, "MATRIX_ID",
                    "ROW_ID","COL_ID","CELL_VAL")
      list(R=list(a$R,
                  as.matrix(b)),
           FL=list(a$FL,
                   b))
  },function(x) (do.call("-",x)),
  function(x) do.call("-",x),n=5,isSquare=TRUE
  )
})


## Testing M_Subtraction
test_that("-: vector and matrix subtraction",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix("tblmatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  FLexpect_equal(M1$FL-M2,M1$R-M2R,check.attributes=FALSE)
  FLexpect_equal(V1-V2,V1R-V2R,check.attributes=FALSE)
  FLexpect_equal(P1$FL-P1$FL,P1$R-P1$R,check.attributes=FALSE)
  FLexpect_equal(V1-P1$FL,V1R-P1$R,check.attributes=FALSE)
  FLexpect_equal(P1$FL-V2,P1$R-V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL-V2),M1$R-V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL-P1$FL),M1$R-P1$R,check.attributes=FALSE)
  FLexpect_equal((P1$FL-M2),P1$R-M2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL-P1$FL-V1-V2-M2-P1$FL-M1$FL-V2),
                P1$R-P1$R-V1R-V2R-M2R-P1$R-M1$R-V2R,
                check.attributes=FALSE)
})



## Testing M_IntegerDivision. Only 2 FLMatrices
test_that("check result for M_IntegerDivision",
{
  expect_eval_equal(initF=function(n) {
      a <- initF.FLMatrix(n=5,isSquare=TRUE)
      b <- FLMatrix("tblmatrixMulti",
                    5, "MATRIX_ID",
                    "ROW_ID","COL_ID","CELL_VAL")
      list(R=list(a$R,
                  as.matrix(b)),
           FL=list(a$FL,
                   b))
  },function(x) (do.call("%/%",x)),
  function(x) do.call("%/%",x)
  )
})



## Testing M_IntegerDivision
test_that("check result for M_IntegerDivision",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix("tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  FLexpect_equal((M1$FL%/%M2),M1$R%/%M2R,check.attributes=FALSE)
  FLexpect_equal((V1%/%V2),V1R%/%V2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%/%P1$FL),P1$R%/%P1$R,check.attributes=FALSE)
  FLexpect_equal((V1%/%P1$FL),V1R%/%P1$R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%/%V2),P1$R%/%V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL%/%V2),M1$R%/%V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL%/%P1$FL),M1$R%/%P1$R,check.attributes=FALSE)
  FLexpect_equal((V1%/%M2),V1R%/%M2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%/%M2),P1$R%/%M2R,check.attributes=FALSE)
})


## Testing M_CrossProduct only two FLMatrices
test_that("check result for M_CrossProduct",
{
  expect_eval_equal(initF=function(n) {
      a <- initF.FLMatrix(n=5)
      b <- FLMatrix("tblmatrixMulti",
                    3, "MATRIX_ID",
                    "ROW_ID","COL_ID","CELL_VAL")
      list(R=list(a$R,
                  as.matrix(b)),
           FL=list(a$FL,
                   b))
  },function(x) (do.call("%*%",x)),
  function(x) do.call("%*%",x)
  )
})



## Testing M_CrossProduct
test_that("check result for M_CrossProduct",
{
  M1 <- initF.FLMatrix(n=5) # 5*4 matrix
  M2 <- FLMatrix("tblmatrixMulti",3,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL") # 4*5 matrix
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,5))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,5))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=5,isRowVec=TRUE)
    FLexpect_equal((M1$FL %*% M2),M1$R%*%M2R,check.attributes=FALSE)
    FLexpect_equal((V1%*%V1),V1R%*%V1R,check.attributes=FALSE)
    FLexpect_equal((P1$FL%*%P1$FL),P1$R%*%P1$R,check.attributes=FALSE)
  FLexpect_equal((V1%*%P1$FL),V1R%*%P1$R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%*%V1),P1$R%*%V1R,check.attributes=FALSE)
    FLexpect_equal((M2%*%V2),M2R%*%V2R,check.attributes=FALSE)
    FLexpect_equal((M2%*%P1$FL),M2R%*%P1$R,check.attributes=FALSE)
    FLexpect_equal((V1%*%M1$FL),V1R%*%M1$R,check.attributes=FALSE)
    FLexpect_equal((P1$FL%*%M1$FL),P1$R%*%M1$R,check.attributes=FALSE)
})



## Testing M_Addition
test_that("check result for Matrix M_Addition",
{
  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- initF.FLMatrix(n,isSquare)
      b <- FLMatrix("tblmatrixMulti",
                    5, "MATRIX_ID",
                    "ROW_ID","COL_ID","CELL_VAL")
      list(R=list(a$R,
                  as.matrix(b)),
           FL=list(a$FL,
                   b))
  },function(x) (do.call("+",x)),
  function(x) do.call("+",x),n=5,isSquare=TRUE
  )
})



## Testing M_Addition
test_that("check result for M_Addition",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix("tblmatrixMulti",
                  5, "MATRIX_ID",
                  "ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  FLexpect_equal(M1$FL+M2,
               M1$R+M2R,
               check.attributes=FALSE)
  FLexpect_equal(V1+V2,
               V1R+V2R,
               check.attributes=FALSE)
  FLexpect_equal(P1$FL+P1$FL,
               P1$R+P1$R,
               check.attributes=FALSE)
  FLexpect_equal(V1+P1$FL,
               V1R+P1$R,
               check.attributes=FALSE)
  FLexpect_equal(P1$FL+V2,
               P1$R+V2R,
               check.attributes=FALSE)
  FLexpect_equal(M1$FL+V2,
               M1$R+V2R,
               check.attributes=FALSE)
  FLexpect_equal(M1$FL+P1$FL,
               M1$R+P1$R,
               check.attributes=FALSE)
  # FLexpect_equal(V1+M2,
  #              V1R+M2R,
  #              check.attributes=FALSE)
  # FLexpect_equal(P1$FL+M2,
  #              P1$R+M2R,
  #              check.attributes=FALSE)
  # FLexpect_equal(P1$FL+P1$FL+V1+V2+M2+P1$FL+M1$FL+V2,
  #              P1$R+P1$R+V1R+V2R+M2R+P1$R+M1$R+V2R,
  #              check.attributes=FALSE)
})




## Testing M_Division
test_that("check result for M_Division",
{
    M1 <- initF.FLMatrix(n=5,
                         isSquare=TRUE)
    M2 <- FLMatrix("tblmatrixMulti",
              5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
    M2R <- as.matrix(M2)
    V1 <- as.FLVector(sample(1:100,
                             10))
    V1R <- as.vector(V1)
    V2 <- as.FLVector(sample(1:100,
                             10))
    V2R <- as.vector(V2)
    P1 <- initF.FLVector(n=10,
                         isRowVec=TRUE)
    FLexpect_equal((M1$FL/M2), M1$R/M2R, check.attributes=FALSE)
    ##FLexpect_equal((V1/V2), V1R/V2R, check.attributes=FALSE)
    FLexpect_equal((P1$FL/P1$FL), P1$R/P1$R, check.attributes=FALSE)
    FLexpect_equal((V1/P1$FL), V1R/P1$R, check.attributes=FALSE)
    FLexpect_equal((P1$FL/V2), P1$R/V2R, check.attributes=FALSE)
    FLexpect_equal((M1$FL/V2), M1$R/V2R, check.attributes=FALSE)
    FLexpect_equal((M1$FL/P1$FL), M1$R/P1$R, check.attributes=FALSE)
    FLexpect_equal((V1/M2), V1R/M2R, check.attributes=FALSE)
    FLexpect_equal((P1$FL/M2), P1$R/M2R, check.attributes=FALSE)
})



## Testing M_Multiplication
test_that("check result for M_Multiplication",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix("tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  ##
  FLexpect_equal(M1$FL*M2,M1$R*M2R,check.attributes=FALSE)
  FLexpect_equal(V1*V2,V1R*V2R,check.attributes=FALSE)
  FLexpect_equal(P1$FL*P1$FL,P1$R*P1$R,check.attributes=FALSE)
  FLexpect_equal(V1*P1$FL,V1R*P1$R,check.attributes=FALSE)
  FLexpect_equal(P1$FL*V2,P1$R*V2R,check.attributes=FALSE)
  FLexpect_equal(M1$FL*V2,M1$R*V2R,check.attributes=FALSE)
  FLexpect_equal(M1$FL*P1$FL,M1$R*P1$R,check.attributes=FALSE)
  FLexpect_equal(V1*M2,V1R*M2R,check.attributes=FALSE)
  FLexpect_equal(P1$FL*M2,P1$R*M2R,check.attributes=FALSE)
  a <- P1$FL*P1$FL*V1*V2*M2*P1$FL*M1$FL
  FLexpect_equal(a *V2,
                 P1$R*P1$R*V1R*V2R*M2R*P1$R*M1$R*V2R,
                 check.attributes=FALSE)
})


## Testing FLIdentical
test_that("check result for identical",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix("tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M3 <- as.FLMatrix(as.matrix(M2))
  M3R <- as.matrix(M2)
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  ##
  FLexpect_equal(identical(M1$FL,M2),identical(M1$R,M2R),check.attributes=FALSE)
  FLexpect_equal(identical(M1$FL,M1$FL),identical(M1$R,M1$R),check.attributes=FALSE)
  FLexpect_equal(identical(M2,M3),identical(M2R,M3R),check.attributes=FALSE)
  FLexpect_equal(identical(V1,V1R),identical(V1R,V1R),check.attributes=FALSE)
  FLexpect_equal(identical(P1$FL,P1$FL),identical(P1$R,P1$R),check.attributes=FALSE)
  FLexpect_equal(identical(V1,P1$FL),identical(V1R,P1$R),check.attributes=FALSE)
  FLexpect_equal(identical(P1$FL,P1$R),identical(P1$R,P1$R),check.attributes=FALSE)
  FLexpect_equal(identical(M1$FL,V2),identical(M1$R,V2R),check.attributes=FALSE)
  FLexpect_equal(identical(M1$FL,P1$FL),identical(M1$R,P1$R),check.attributes=FALSE)
  FLexpect_equal(identical(V1,V1),identical(V1R,V1R),check.attributes=FALSE)
  FLexpect_equal(identical(P1$FL,M2),identical(P1$R,M2R),check.attributes=FALSE)
})


## testing M_Subtraction with different length vectors
test_that("-: fzzlSerial column vectors of different length",
{
  flt <- FLTable("FL_DEMO.fzzlSerial","SerialVal", whereconditions = "serialval<100")
  flv1 <- flt[1:8,"RandVal"]
  flv <- flt[1:10,"RandVal"]
  flv1R <- as.vector(flv1)
  flvR <- as.vector(flv)
  FLexpect_equal(flv-flv1,flvR-flv1R,check.attributes=FALSE)
})



## Testing FLTranspose
test_that("check transpose",{
    expect_eval_equal(initF.FLMatrix,AdapteR::t,base::t,n=5)
})
