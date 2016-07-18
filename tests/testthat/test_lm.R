Renv = new.env(parent = globalenv())

var1 = c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
var2 = c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
var3 = rep(0:1,each = 10)
var4 = c(var1,var2)
var5 = data.frame(var4 = var4,var3 =var3)
rownames(var5) <- 1:nrow(var5)
Renv$var5 <- var5
FLenv = as.FL(Renv)
Renv$lmobj <- lm(var4~var3,data=Renv$var5,x=TRUE,y=TRUE)
FLenv$lmobj <- lm(var4~var3,data=FLenv$var5)

#Test failed.
#Not proper arguments given.
#Asana Ticket -  https://app.asana.com/0/143316600934101/146934264360575
test_that("Check for lm function with and without intercept",{
    result = eval_expect_equal({
             sapply(c("coefficients","residuals",
                      "fitted.values","df.residual",
                      "rank","terms"),
             function(x)
             assign(i,do.call("$",list(lmobj,i)))
             modelDim <- dim(lmobj$model)
             xDim <- dim(lmobj$x)
             ylength <- length(lmobj$y))
      },Renv,FLenv)
    sapply(c("FLCoeffStdErr","FLCoeffTStat",
              "FLCoeffPValue","FLCoeffNonZeroDensity",
              "FLCoeffCorrelWithRes"),
            function(x)
             FLexpect_equal(length(Renv$coefficients),
                      length(do.call("$",list(FLenv$lmobj,x))))
          )
    FLexpect_equal(ncol(FLenv$lmobj$FLLinRegrStats),21)
})

#Not tested as lm was not working.
# Check for plot function of Linear Regression.
test_that("Check for plot of lm function",{
          result = eval_expect_equal({
                   plot(lmobj,las= 1)
            },Renv,FLenv)
    })
