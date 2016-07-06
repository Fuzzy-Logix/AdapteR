Renv = new.env(parent = globalenv())
Renv$var1 = as.character(1:12)

FLenv <- as.FL(Renv)

test_that("paste0: ",{
    result = eval_expect_equal({
        test1 = paste0(var1)},
        Renv,FLenv)
})
