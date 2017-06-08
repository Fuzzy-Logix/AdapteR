library(neuralnet)

## For 1 Layer:

Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
FLenv$tbl <- FLTable("tblwinetrain", obs_id_colname = "OBSID")
Renv$tbl <- as.R(FLenv$tbl)
Renv$tbl <- Renv$tbl[, -c(1)]
n <- names(Renv$tbl)
f <- as.formula(paste("Wine_Type ~", paste(n[!n %in% "Wine_Type"], collapse = " + ")))

FLenv$mod <- neuralnet(f, data = FLenv$tbl, hidden = c(5), layers = 1)
Renv$mod <- neuralnet(f, data = Renv$tbl, hidden = c( 5))


## Comparison of Weights: 
test_that("Comparison of Weights:",{expect_equal(FLenv$mod$weights,
                                                 Renv$mod$weights )})


## Prediction, Residuals, fitted.values: 
test_that("Comparison of model.list, cost:",{result <- eval_expect_equal({
    modlist <- mod$model.list
  },Renv,FLenv,
expectation=c("modlist"),
check.attributes=FALSE,
verboose = TRUE)})

## Comparison of Weights: 
test_that("Comparison of response:",{result <- eval_expect_equal({
    pred <- predict(mod)
  },Renv,FLenv,
expectations=c("pred"),
check.attributes=FALSE,
verboose = TRUE)})

FLexpect_equal(FLenv$mod$cost,0.688, tolerance = .01 )





## For 2 layer 
FLenv$mod <-neuralnet(f, data = FLenv$tbl, hidden = c(10,5))
Renv$mod <- neuralnet(f, data = Renv$tbl, hidden = c(10, 5))



## Comparison of Weights: 
test_that("Comparison of Weights:",{expect_equal(FLenv$mod$weights,
                                                 Renv$mod$weights )})

 
test_that("Comparison of model.list, cost:",{result <- eval_expect_equal({
    modlist <- mod$model.list
  },Renv,FLenv,
expectation=c("modlist"),
check.attributes=FALSE,
verboose = TRUE)})

## Comparison of Weights: 
test_that("Comparison of response:",{result <- eval_expect_equal({
    pred <- predict(mod)
  },Renv,FLenv,
expectations=c("pred"),
check.attributes=FALSE,
verboose = TRUE)})

FLexpect_equal(FLenv$mod$cost,0.689, tolerance = .01 )







## R example 2:
set.seed(100)
Renv <- new.env(parent = globalenv())
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)
Renv$tbl <- cbind(traininginput,trainingoutput)
colnames(Renv$tbl) <- c("InputCol","OutputCol")
FLenv <- as.FL(Renv)

##
Renv$mod <- neuralnet(OutputCol~InputCol,data=Renv$tbl,hidden=c(10),linear.output=T)
FLenv$mod <- neuralnet(OutputCol~InputCol,data=FLenv$tbl,hidden=c(10), IsSigmoid = 0, layers = 1)



## Comparison of Weights: 
test_that("Comparison of Weights:",{expect_equal(FLenv$mod$weights,
                                                 Renv$mod$weights )})

## Prediction, Residuals, fitted.values: 
test_that("Comparison of model.list, cost:",{result <- eval_expect_equal({
    modlist <- mod$model.list
  },Renv,FLenv,
expectation=c("modlist"),
check.attributes=FALSE,
verboose = TRUE)})

## Comparison of Response: 
test_that("Comparison of response:",{result <- eval_expect_equal({
    pred <- predict(mod)
  },Renv,FLenv,
expectations=c("pred"),
check.attributes=FALSE,
verboose = TRUE)})

FLexpect_equal(FLenv$mod$cost,6.40, tolerance = .01 )
