#testing standard deviation
Renv <- new.env(parent = globalenv())
Renv$a <-1:2
FLenv <- as.FL(Renv)
test_that("Testing sd",
{
result1=eval_expect_equal({test1<-sd(a)^2},Renv,FLenv)
##print(result1)
}
)
