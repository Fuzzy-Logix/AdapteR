Renv = new.env(parent = globalenv())

var1 = c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
var2 = c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
var3 = rep(0:1,each = 10)
var4 = c(Renv$var1,Renv$var2)
Renv$var5 = data.frame(var4 = Renv$var4,var3 =Renv$var3)

FLenv = as.FL(Renv)

## data needs to be explicitly specified.
## Intercept and interaction terms not supported.
#Asana Ticket -  https://app.asana.com/0/143316600934101/146934264360575
test_that("Check for lm function without intercept and data",{
          result = eval_expect_equal({
                   test2 = lm(var4~var3 - 1)
            },Renv,FLenv)
          print(result)
    })