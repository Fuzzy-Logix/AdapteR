##No store for R objects


test_that("check casting and store for integer,character types",{
    x <- as.FLMatrix(matrix(9:1,3,3))
    y <- as.FLVector(5:1)
    z <- as.FLVector(c("a","b"))
    FLexpect_equal(x,store(x),check.attributes=FALSE)
    FLexpect_equal(y,store(y),check.attributes=FALSE)
    FLexpect_equal(z,store(z),check.attributes=FALSE)
})

test_that("check cross database name mapping",{
    x <- FLMatrix("FL_TRAIN","tblmatrixMulti",
                    5,"Matrix_id","ROW_ID",
                    "COL_ID","CELL_VAL",
                    dimnames = list(5:1,letters[1:5]))
    y <- as.matrix(x)
    FLexpect_equal(solve(x),solve(y),check.attributes=FALSE)
    FLexpect_equal(dimnames(solve(x)),dimnames(solve(y)),check.attributes=FALSE)
})

## Fails https://app.asana.com/0/143316600934101/144495595719769
test_that("check casting and store for character matrix",{
    x <- as.FLMatrix(matrix(c("a","b"),2))
    FLexpect_equal(x,store(x),check.attributes=FALSE)
})