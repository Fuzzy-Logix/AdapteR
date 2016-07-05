Renv = new.env(parent = globalenv())
Renv$testdf <- data.frame(mylogic=c(TRUE,FALSE,TRUE),
                          myinteg=1:3,
                          myfloat=1:3/3,
                          myfact=as.factor(c("a","b","a")),
                          mychar=c("one","two","three"))

## rownames column needs to be removed
## https://app.asana.com/0/143778401455745/140240837628916
##options(debugSQL=TRUE)
test_that("FLTable supports different types",{
    FLexpect_equal(FLenv$testdf,Renv$testdf)
    ##print(result)
})
