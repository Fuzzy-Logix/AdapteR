## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.

## The following script demos the fitting of 
## generalized Additive Models using DB-Lytix and
## AdapteR.

## Main Value Points:
##      Ease of modelling due to syntactical similarity with R
##      Scalable in-database computation

## Intial set-up
oldWarn = getOption("warn")
options(warn=-1)


## Set-up the ODBC connection to demo database.
demo("connecting",package="AdapteR")

## Create a FLTable object (Equivalent to R's data.frame)
## to hold the metadata of the remote table

FLTableObject <- FLTable("tblGAMSimData","ObsID")

readline("Above: FLTable object is created: ")

head(FLTableObject)

readline("Above: use head to examine the data ")

myformula <- yVal~s(x0Val,by="groupID",m=3,k=10)+te(x1Val,x2Val,m=3,k=5)

gamobject <- gam(myformula,data=FLTableObject)

readline("Above: Fit a GAM model based on formula object: smooth and Tensor interactions supported: ")

## Access key output statistics with 
## $ operator -- simple R like syntax
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
