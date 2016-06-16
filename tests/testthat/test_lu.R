Renv <- new.env(parent = globalenv())
Renv$x <- Matrix(rnorm(9),3,3)

FLenv <- as.FL(Renv)

test_that("LU on dense square matrix",
eval_expect_equal({
  result1 <- lu(x)
},Renv,FLenv,check.attributes=FALSE))
test_that("LU on dense non-square matrix",
          eval_expect_equal({
            result2 <- round(10 * x[,-3],2)
            result3 <- expand(lu(result2))
          },Renv,FLenv,check.attributes=FALSE))

eval_expect_equal({
   result2 <- round(10 * x[,-3])
  
},Renv,FLenv,check.attributes=FALSE)