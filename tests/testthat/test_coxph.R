Renv = new.env(parent = globalenv())

Renv$var1 = list(time1=c(4,3,1,1,2,2,3), 
              status=c(1,1,1,0,1,1,0), 
              x=c(0,2,1,1,1,0,0), 
              sex=c(0,0,0,0,1,1,1))

#Done this as list was not being converted to FLVector as it was integer type.
Renv$var1 = as.data.frame(Renv$var1)

Renv$var2 = list(start1=c(1,2,5,2,1,7,3,4,8,8), 
                stop1=c(2,3,6,7,8,9,9,9,14,17), 
                event=c(1,1,1,1,1,1,1,0,0,0), 
                x=c(1,0,0,1,0,1,1,1,0,0))

#Done this as list was not being converted to FLVector as it was integer type.
Renv$var2 = as.data.frame(Renv$var2)

Renv$var3 = bladder[bladder$enum < 5, ]
#Done as number is akeyword in DBLytix
colnames(Renv$var3)[3] = "number1"

Renv$var4 = lung
#Done as time is a keyword in DBLytix.
colnames(Renv$var4)[2] = "time1"
#Done this here to avoid having different column names as DBLytix removes "." from its column name.
colnames(Renv$var4)[6:10] =c("phecog","phkarno","patkarno","mealcal","wtloss")

FLenv = as.FL(Renv)

#Test failed. strata argument is not defined in AdapteR.
#Number of coefficients do not match.AdapteR gives coefficients for "sex" and "x"
#While R gives coefficients for "x" only.
#Asana Ticket - https://app.asana.com/0/143316600934101/149125521251137
test_that("Check for coxph with stratified model",{
          result = eval_expect_equal({
          test1 = coxph(Surv(time1, status) ~ x + strata(sex), var1)
          test1 = test1$coefficients
            },Renv,FLenv,check.attributes = FALSE)
          print(result)
    })
#Test failed.
#AdapteR and R coefficients are different.Other parameters are also different.
#Asana Ticket - https://app.asana.com/0/143316600934101/149125521251137
test_that("Check for coxph model",{
          result = eval_expect_equal({
          test5 = coxph(Surv(time1, status) ~ x, var1)
          test5 = test5$coefficients
            },Renv,FLenv,check.attributes = FALSE)
          print(result)
    })

#Test failed .
#AdapteR and R gives different result.
#Asana Ticket - https://app.asana.com/0/143316600934101/149125521251137
test_that("Check for coxph (time dependent model)",{
          result = eval_expect_equal({
          test2 = coxph(Surv(start1, stop1, event) ~ x, var2)
            },Renv,FLenv,check.attributes = FALSE)
          print(result)
    })

#Incorrect number of dimensions.
test_that("Check for coxph with stratified model on cluster of patients",{
          result = eval_expect_equal({
          test3 = coxph(Surv(stop, event) ~ (rx + size + number1) * strata(enum) + 
                        cluster(id), var3)
            },Renv,FLenv,check.attributes = FALSE)
          print(result)
    })

#Not able to use "tt" argument.
#Asana ticket - https://app.asana.com/0/143316600934101/149125521251137
test_that("Check for coxph with time transform model model",{
          result = eval_expect_equal({
          test4 = coxph(Surv(time1, status) ~ phecog + tt(age), data=var4,
                   tt=function(x,t,...) pspline(x + t/365.25))
            },Renv,FLenv,check.attributes = FALSE)
          print(result)
    })






