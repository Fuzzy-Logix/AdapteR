
t <- rlm(stackloss~., data = dt, psi = "huber")

dtR <- as.R(dt)
t <- rlm(Diameter~ ShuckedWeight +  VisceraWeight, data = dtR, psi = "huber")



                                        #PLS, OPLS

for pls what to compute still and what is done

done:
Xmeans, Ymeans, method(not needed),

to-do:
fitted.values, residuals, predict, Yscores, loading weights, Yloadings, 


deeptbl  <- FLTable("tblPLSDeep2y", "ObsID", "VarID", "Num_Val")
rtbl <- as.R(deeptbl)
names(rtbl) <- letters[1:16]
flmod<- pls(A~., data =deeptbl, nfactor = 15 )
rmod <- mvr(a~., data = rtbl)

## Robust Regression
## Deep Table
library(MASS)
#options(debugSQL =FALSE)

Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)

deeptbl  <- FLTable("tblRobustRegr", "ObsID","VarID", "Num_Val")
FLenv$DAT <- deeptbl

## varmapping(deeptbl) <- c("a","i","var")
## varmapping(deeptbl) <- X(table="fzzlRegrDataPrepMap",id="Final_VarID", value="COLUMN_NAME")
RD <- as.data.frame(deeptbl, drop.intercept=TRUE)
names(RD) <- c("a","i","var") ## should be done by varmapping
RD$i <- NULL  ## should be dropped during as.data.frame
Renv$DAT <- RD

eval_expect_equal({
    fit <- rlm(a~., data = DAT)
},Renv,FLenv,
noexpectation = "fit")

eval_expect_equal({
    fitS <- summary(fit)
    print(fitS)
},Renv,FLenv)

eval_expect_equal({
    fitP <- predict(fit) ## todo: use FLSUMPROD
    fitPbyname <- fit$fitted.values
},Renv,FLenv,
expectation=c("fitP","fitPbyname"),
check.attributes=F)

## Wide Table:
FLenv <- new.env(parent = globalenv())
FLenv$widetbl <- FLTable("tblautompg", "ObsID")
Renv <- as.R(FLenv) ## todo: add temporary=FALSE

eval_expect_equal({
    fit <- rlm(Weight ~ Acceleration , data = widetbl)
},Renv,FLenv,
noexpectation=c("fit"),
check.attributes=F)

Renv$widetbl

eval_expect_equal({
    fitS <- summary(fit)
    print(fitS)
},Renv,FLenv)

eval_expect_equal({
    fitP <- predict(fit) ## todo: use FLSUMPROD
    fitPbyname <- fit$fitted.values
},Renv,FLenv,
expectation=c("fitP","fitPbyname"),
check.attributes=F)

eval_expect_equal({
    fitC <- coefficients(t)
    fitR <- residuals(t)
},Renv,FLenv,
expectation=c("fitR","fitR"),
check.attributes=F)

