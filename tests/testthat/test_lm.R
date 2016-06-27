Renv = new.env(parent = globalenv())

Renv$var1 =  c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
Renv$var2 =  c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
Renv$var3 = rep(0:1,each = 10)
Renv$var4 = c(Renv$var1,Renv$var2)
Renv$var5 = data.frame( var4 = Renv$var4,var3 =Renv$var3)

FLenv = as.FL(Renv)

#Test for lm function with and without intercept
#Test Failed.
#lm not overloaded for sufficient classes.In this example if we do not provide data argument as I did.
#It will call default lm of R with formula consisting FL Vector, then R gives an error message.
#Asana Ticket -  https://app.asana.com/0/143316600934101/146934264360575
test_that("Check for lm function with and without intercept",{
          result = eval_expect_equal({
                   test1 = lm(var4~var3)
                   test2 = lm(var4~var3 - 1)
            },Renv,FLenv)
          print(result)
    })

#Test failed.
#Not proper arguments given.
#Asana Ticket -  https://app.asana.com/0/143316600934101/146934264360575
test_that("Check for lm function with and without intercept",{
          result = eval_expect_equal({
                   test1 = lm(var4~var3,var5)
                   test2 = lm(var4~var3 - 1,var5)
            },Renv,FLenv)
          print(result)
    })

#Not tested as lm was not working.
# Check for plot function of Linear Regression.
test_that("Check for lm function",{
          result = eval_expect_equal({
                   plot(test1,las= 1)
            },Renv,FLenv)
          print(result)
    })


