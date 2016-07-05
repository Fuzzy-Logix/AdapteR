Renv = new.env(parent = globalenv())
Renv$var1 = as.character(1:12)

FLenv <- as.FL(Renv)

#Test failed .Not overloaded for FL Vector type.
test_that("paste0: https://app.asana.com/0/134161214112401/145368493400281",{
    result = eval_expect_equal({
        test1 = paste0(var1)},
        Renv,FLenv)
    ##    print(result)
    })
