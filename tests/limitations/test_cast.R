test_that("Internationalization: Upload of German umlauts",{
    rv <- c("let", "us", "test for non-äöü-printable","characters")
    flv  <- as.FL(rv)
    expect_equal(as.R(flv),rv)
})



## Testing FLCastFunctions
## gk: these need reviewiing and commenting.
## todo phani, kumar:
## what you cannot comment, please remove
test_that("cast: repeated testing sometimes results in precision errors -- investigate",
{
  M1 <- initF.FLMatrix(n=5)
  V1 <- as.FLVector(sample(1:100,5))
  V1R <- as.vector(V1)
  P1 <- initF.FLVector(n=5,isRowVec=TRUE)
  T1 <- initF.FLTable(rows=5,cols=5)
  FLexpect_equal(as.vector(M1$FL),as.vector(M1$R),check.attributes=FALSE)
  FLexpect_equal(as.vector(P1$FL),as.vector(P1$R),check.attributes=FALSE)
  FLexpect_equal(as.data.frame(M1$FL),as.data.frame(M1$R),check.attributes=FALSE)
  testthat::expect_equal(as.matrix(P1$FL),as.matrix(P1$R),check.attributes=FALSE)
  testthat::expect_equal(as.matrix(V1),as.matrix(V1R),check.attributes=FALSE)
  FLexpect_equal(as.FLMatrix(M1$R),M1$FL,check.attributes=FALSE)
  FLexpect_equal(P1$FL,as.FLVector(P1$R),check.attributes=FALSE)
  FLexpect_equal(as.matrix(as.FLMatrix(V1)),as.matrix(V1R),check.attributes=FALSE)
  FLexpect_equal(as.matrix(as.FLMatrix(P1$R)),as.matrix(P1$R),check.attributes=FALSE)
  FLexpect_equal(as.vector(as.FLVector(M1$R)),as.vector(M1$R),check.attributes=FALSE)
  FLexpect_equal(as.FLVector(M1$FL),as.vector(M1$R),check.attributes=FALSE)
})



## Testing Equality
test_that("check result for M_Equality",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M3 <- as.FLMatrix(as.matrix(M2))
  M3R <- as.matrix(M2)
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  ##
  FLexpect_equal(M1$FL==M2,(M1$R==M2R),check.attributes=FALSE)
  FLexpect_equal(M1$FL==M1$FL,M1$R==M1$R,check.attributes=FALSE)
  FLexpect_equal(M2==M3,M2R==M3R,check.attributes=FALSE)
  FLexpect_equal(V1==V1R,V1R==V1R,check.attributes=FALSE)
  FLexpect_equal(P1$FL==P1$FL,P1$R==P1$R,check.attributes=FALSE)
  FLexpect_equal(V1==P1$FL,V1R==P1$R,check.attributes=FALSE)
  FLexpect_equal(P1$FL==P1$R,P1$R==P1$R,check.attributes=FALSE)
  ##FLexpect_equal(M1$FL==V2,M1$R==V2R,check.attributes=FALSE)
  ##FLexpect_equal(M1$FL==P1$FL,M1$R==P1$R,check.attributes=FALSE)
  FLexpect_equal(V1==V1,V1R==V1R,check.attributes=FALSE)
  ##FLexpect_equal(P1$FL==M2,P1$R==M2R,check.attributes=FALSE)
})
