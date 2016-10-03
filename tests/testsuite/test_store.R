test_that("cast: numeric types",{
    rx <- matrix(9:1,3,3)
    x <- as.FLMatrix(rx)
    ry <- 5:1
    y <- as.FLVector(ry)
    FLexpect_equal(x,rx,check.attributes=FALSE)
    FLexpect_equal(y,ry,check.attributes=FALSE)
})

test_that("check cross database name mapping",{
    x <- FLMatrix("FL_TRAIN.tblmatrixMulti",
                    5,"Matrix_id","ROW_ID",
                    "COL_ID","CELL_VAL",
                    dimnames = list(5:1,letters[1:5]))
    y <- as.matrix(x)
    FLexpect_equal(solve(x),solve(y),check.attributes=FALSE)
    FLexpect_equal(dimnames(solve(x)),dimnames(solve(y)),check.attributes=FALSE)
})
