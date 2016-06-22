Renv = new.env(parent = globalenv())

Renv$var1 = seq(1,5)
Renv$var2 = (Renv$var1)^2
Renv$var3 = data.frame(Renv$var1,Renv$var2)

FLenv = as.FL(Renv)

#Test Failed . SQL error in conversion.
#Asana ticket = 
test_that("Check for harmonic mean function",{
          result = eval_expect_equal({
                   test1 = harmonic.mean(var1)
                   test2 = harmonic.mean(var2)
            },Renv,FLenv)
          print(result)
    })


Renv$var2[2] = NA
Renv$var3 = data.frame(Renv$var1,Renv$var2)

FLenv=as.FL(Renv)

#test failed . Non Numeric argument to binary operator
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360538
test_that("Check for harmonic mean function with NA value in one column",{
          result = eval_expect_equal({
                   test3 = harmonic.mean(var3)
                   test4 = harmonic.mean(var3,na.rm=FALSE)
            },Renv,FLenv)
          print(result)
    })
