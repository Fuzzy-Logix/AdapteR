Renv = new.env(parent = globalenv())
Renv$df <- data.frame(a=1:5,b=6:10)

##https://app.asana.com/0/143778401455745/140240837628916
## preserving table structure in as.FLTable
test_that("length of data.frame",{
    result = eval_expect_equal({
        Ldf <- length(df)
    },Renv,FLenv)
    ##print(result)
})