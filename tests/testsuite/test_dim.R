## Testing FLDims
test_that("check FLDims if all elements of a row are zero",
{
  m <- Matrix(c(0,1,0,2),2,sparse=T)
  m <- as(m,"dgCMatrix")
  M <- as.FLMatrix(m)
  T1 <- initF.FLTable(rows=5,cols=5)
  T1R <- as.data.frame(T1)
  expect_equal(AdapteR::dim(M),
               base::dim(m),
               check.attributes=FALSE)
  expect_equal(AdapteR::dim(T1),
               base::dim(T1R),
               check.attributes=FALSE)
})
