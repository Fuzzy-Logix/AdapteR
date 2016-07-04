Renv = new.env(parent = globalenv())


Renv$var1 = 1:12

FLenv <- as.FL(Renv)

#Test failed .Not overloaded for FL Vector type.
#Asana ticket - https://app.asana.com/0/134161214112401/145368493400281
test_that("Check for paste function ",{
    result = eval_expect_equal({
        test1 = paste0(var1)},
        Renv,FLenv)
    ##    print(result)
    })
