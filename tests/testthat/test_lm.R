
parts <- c("coefficients","residuals",
           "fitted.values","df.residual",
           "rank","terms")

Renv = new.env(parent = globalenv())

ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
groupf <- gl(2, 10, 20, labels = c("Ctl","Trt"))
groupb = rep(0:1,each = 10)
weight <- c(ctl, trt)
dataframe = data.frame(weight = weight,groupf =groupf,groupb =groupb)
rownames(dataframe) <- 1:nrow(dataframe)
Renv$dataframe <- dataframe
FLenv = as.FL(Renv)

test_that("lm: execution",{
    result = eval_expect_equal({
             lmobj <- lm(weight ~ groupb,data=dataframe)
             C <- coef(lmobj)
    },Renv,FLenv,
    expectation="C",
    noexpectation = "lmobj",
    check.attributes=F)
})

test_that("lm: coefficient names https://app.asana.com/0/143316600934101/156989386834241",{
    result = eval_expect_equal({},Renv,FLenv,
                               expectation="C")
})

test_that("lm: equality of coefficients, residuals, fitted.values, rank and terms",{
  result = eval_expect_equal({
    sapply(parts,
           function(i){
             ##cat(paste0("getting lmobj$",i,"\n"))
             assign(i,do.call("$",list(lmobj,i)))
           })
    modelDim <- dim(lmobj$model)
  },Renv,FLenv,
  noexpectation = "lmobj",
  expectation = c(parts,"modelDim"))
})


test_that("lm: support of factors, https://app.asana.com/0/143316600934101/146934264360575",{
    result = eval_expect_equal({
      lmobj <- lm(weight ~ groupf,data=dataframe)
      C <- coef(lmobj)
    },Renv,FLenv,
    expectation="C",
    noexpectation = "lmobj")
})

test_that("lm: execution with x=TRUE,y=TRUE",{
    result = eval_expect_equal({
             lmobj <- lm(weight ~ groupb,data=dataframe,x=TRUE,y=TRUE)
             xDim <- dim(lmobj$x)
             ylength <- length(lmobj$y)
    },Renv,FLenv,
    noexpectation = "lmobj")
})

test_that("lm: summary.lm https://app.asana.com/0/143316600934101/156948192818458",{
  result = eval_expect_equal({
    lmSum <- summary(lmobj)
  },Renv,FLenv,
  expectation = "lmSum")
})

## Check for plot function of Linear Regression.
## 
## check to run manually for equal results
if(FALSE){
  plot(Renv$lmobj,las= 1)
  plot(FLenv$lmobj)
}
