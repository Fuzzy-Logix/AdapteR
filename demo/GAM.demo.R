## GAM Demo AdapteR ##
oldWarn = getOption("warn")
options(warn=-1)
options(debugSQL=FALSE)

FLTableObject <- FLTable(getTestTableName("tblGAMSimData"),"ObsID")

readline("Above: FLTable object is created: ")

head(FLTableObject)

readline("Above: use head to examine the data ")

myformula <- yVal~s(x0Val,by="groupID",m=3,k=10)+te(x1Val,x2Val,m=3,k=5)
if(is.TDAster())
	myformula <- yval~s(x0val,by="groupid",m=3,k=10)+te(x1val,x2val,m=3,k=5)

gamobject <- gam(myformula,data=FLTableObject)

readline("Above: Fit a GAM model based on formula object: smooth and Tensor interactions supported: ")


gamobject$coefficients

gamobject$df.null

gamobject$edf

gamobject$knots

gamobject$df.residual

readline("Above: Examine key output statistics with $ operator -- simple R like syntax")

print(gamobject)

readline("Below: Observe fitted values and residuals after scoring on same dataset")
gamobject$fitted.values

## Observe residuals
gamobject$residuals

readline("Below: Use predict to score the model on a test dataset")
predictedValues <- predict(gamobject,FLTableObject)
head(predictedValues)

## Thank You!
options(warn=oldWarn)
####################
