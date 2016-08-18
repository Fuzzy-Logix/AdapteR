Renv = new.env(parent = globalenv())
Renv$mat1 = cbind(c(1,-1), c(-1,1))
Renv$mat2 = cbind(1, c(1,-1))
Renv$mat4 <- cbind( 1, 3:1, 1:3)

## In order for examples to work in a chain, FLenv is needed
FLenv <- as.FL(Renv)

test_that("eigen values",{
    result = eval_expect_equal({
        e1 <- eigen(mat1)
    }, Renv, FLenv,
    expectation="e1")
})

test_that("eigen, option only.values",{
    result = eval_expect_equal({
        e2 <- eigen(mat2, only.values = TRUE)
    },Renv,FLenv,
    tolerance=1e-6)
})

##Different signs for R and FL.
## Checking other properties
## different signs in FL and R output
test_that("eigen values are correct:- det(A-vI)=0 ",{
    e4 <- eigen(FLenv$mat4)
    for(i in 1:length(e4$values))
        FLexpect_equal(det(FLenv$mat4-((e4$values[i])*diag(nrow(FLenv$mat4)))),
                       0)
})


test_that("eigen vectors are correct:- A %*% X = vI %*% X ",{
    e4 <- eigen(FLenv$mat4)
    for(i in 1:length(e4$values)){
        FLexpect_equal(as.vector((FLenv$mat4)%*%(e4$vectors[,i])),
            as.vector((e4$values[i]*diag(nrow(FLenv$mat4)))%*%e4$vectors[,i]),
            tolerance=0.0001)
    }
})
