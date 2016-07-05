## Fails https://app.asana.com/0/143316600934101/144495595719769
## storing character matrices with as.FLMatrix
test_that("check casting and store for character matrix",{
    x <- as.FLMatrix(matrix(c("a","b"),2))
    FLexpect_equal(x,store(x),check.attributes=FALSE)
})