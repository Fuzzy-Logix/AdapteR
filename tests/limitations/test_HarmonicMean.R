Renv = new.env(parent = globalenv())

Renv$var1 = seq(1,5)
Renv$var2 = (Renv$var1)^2
Renv$var3 = data.frame(Renv$var1,Renv$var2)
Renv$var4 = Renv$var2
Renv$var4[2] = NA
Renv$var5 = data.frame(Renv$var1,Renv$var4)

FLenv = as.FL(Renv)

#test failed . Not running for class FLTable.
## We want to do this columnwise.
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360538
test_that("Check for harmonic mean function with NA value in one column",{
          result = eval_expect_equal({
                   test3 = harmonic.mean(var5)
                   test4 = harmonic.mean(var5,na.rm=FALSE)
            },Renv,FLenv)
          ##print(result)
    })