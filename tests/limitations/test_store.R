test_that("casting character vectors and matrix: https://app.asana.com/0/143316600934101/144495595719769",{
    x <- as.FLMatrix(matrix(c("a","b","c","d"),nrow=2))
    FLexpect_equal(x,store(x),check.attributes=FALSE)
    z <- as.FLVector(c("a","b"))
    FLexpect_equal(z,store(z),check.attributes=FALSE)
})
