## demo("connecting")

############################################################
## First you pull all variable / data setup out of the example:
Renv <- new.env(parent = globalenv())
Renv$a <- 1:10
Renv$b <- 1:5

FLenv <- as.FL(Renv)


## if you want to use initF, you do it this way:
tmp <- initF.FLMatrix(10,TRUE)
Renv$bigobject <- tmp$R
FLenv$bigobject <- tmp$FL

############################################################
## R documentation example from stats::cor
## run expectations within test_that as blocks
test_that("Variance single column.",
          eval_expect_equal({
              vara <- var(a)  # 9.166667
              length(a)
          }, Renv, FLenv))

test_that("Correlation of two vectors. https://app.asana.com/0/136555696724838/143778401455751",{
    eval_expect_equal({
        varb <- var(b, b) # 2.5
    }, Renv, FLenv)
})

## TODO: change more from here
test_that("Correlation examples -- data needs pulling out!",{
    eval_expect_equal({
        ## Two simple vectors
        cor(1:10, 2:11) # == 1
    }, Renv, FLenv)
})
