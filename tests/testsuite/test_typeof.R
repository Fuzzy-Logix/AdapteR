

## Testing typeof
test_that("typeof: matrix, vector and expressions",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getTestTableName("tblmatrixMulti"),5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  expect_equal(typeof(M2),"double")
  expect_equal(typeof(M1$FL),"double")
  V1 <- as.FLVector(sample(1:100,10))
  expect_equal(typeof(V1),"integer")

  widetable  <- FLTable("tblstringID",
                         "stringID",
                         type=c("integer","character"))
  V2 <- widetable[1:6,"string"]
  expect_equal(typeof(V2),"character")
  expect_equal(typeof(FLIsHex(V2)),"logical")
  expect_equal(typeof(regexpr("A",V2)),"integer")
  expect_equal(typeof(gsub("A","X",V2)),"character")
  ## GK Added this needs review in FLMatrixArithmetic
  ## hence comparisions not working.
  # expect_equal(typeof(M2>M2),"logical")
  # expect_equal(typeof(as.R(M2>M2)),"logical")
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  expect_equal(typeof(P1$FL),"double")

  expect_equal(typeof(P1$FL*P1$FL),"double")
  expect_equal(typeof(V1*M2*P1$FL*M1$FL),"double")
  expect_equal(typeof(P1$FL*P1$FL*V1*M2*P1$FL*M1$FL),"double")
  ## gk: please add more tests systematically
})

## Testing typeof
test_that("typeof: FLTable fzzlSerial, subsetting vector",
{
  flt <- FLTable(getTestTableName("fzzlSerial"),"SERIALVAL", whereconditions = "SERIALVAL<100")
  flv1 <- flt[1:8,"RANDVAL"]
  flv <- flt[1:10,"RANDVAL"]
  expect_equal(typeof(flv1),"double")
  expect_equal(typeof(flv),"double")
  expect_equal(flv1@type,"double")
  expect_equal(flv@type,"double")
})
