Renv = new.env(parent = globalenv())

var1 <- rnorm(2000)
var2 <- rnorm(2000)
var3 <- sample( c(0, 1), 2000, replace = TRUE)
var4 <- data.frame(var1 = var1,var2 =var2, var3 = var3)
rownames(var4) <- 1:nrow(var4)
Renv$var4 <- var4
FLenv = as.FL(Renv)

fam <- c("poisson","binomial")
sapply(fam, function(z){
              
              Renv$glmobj <- glm(var3 ~ var2 + var1 , data = Renv$var4, family = z , x=TRUE, y=TRUE)
              FLenv$glmobj <- glm(var3 ~ var2  + var1, data = FLenv$var4, family = z)
              
              test_that("Check for glm function its output coefficients",{
              
                result = eval_expect_equal({
                  sapply(c("coefficients","residuals",
                           "fitted.values","df.residual"
                          ),
                            function(i){
                              assign(i,do.call("$",list(glmobj,i)))
                            }
                            )
                  modelDim <- dim(glmobj$model)
                  xDim <- dim(glmobj$x)
                  ylength <- length(glmobj$y)

                            
                          },Renv,FLenv)



                FLexpect_equal(ncol(FLenv$glmobj$FLLogRegrStats),16)

                sapply(c("FLCoeffStdErr","FLCoeffTChiSq",
                           "FLCoeffPValue"), 
                            function(x){
                              FLexpect_equal(length(Renv$coefficients),
                                length(do.call("$",list(FLenv$glmobj,x))))
                            }
          )
           
})
  })

# Test passes till line 33 or fails after FLLogRegrStats
