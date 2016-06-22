#testing standard deviation
Renv <- new.env(parent = globalenv())
test_that("Testing sd",
{
Renv$a <-1:2
eval_expect_equal({test1<-sd(a)^2},Renv)
}
)


