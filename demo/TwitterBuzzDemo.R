## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.

## This demo shows how the
## AdapteR package of Fuzzy Logix is
## easing interaction with the DB Lytix(TM) in-database
## library.
##
## The demo highlights how to perform
## Linear Regression on tblTwitterBuzz dataset
## to predict the buzz magnitude
if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}


#############################################################
## Create a FLTable object for tblTwitterBuzz table
## Refer ?FLTable for help on creating FLTable Objects.
FLtbl <- FLTable("tblTwitterBuzz","OBSID")

vtemp <- readline("Below: Examining data structure using head \n ")
head(FLtbl)

dim(FLtbl)

vtemp <- readline("Below: Fitting lm model on data \n ")
vresFL <- lm(Buzz_Magnitude~.,data=FLtbl)

print(vresFL)
vtemp <- readline("Above: Print method on fitted object \n ")

head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients \n ")

summary(vresFL)
vtemp <- readline("Above: summary method on fitted object \n ")

FLfit <- predict(vresFL,FLtbl)
vtemp <- readline("Above: Prediction on same dataset \n ")

head(FLfit)
vtemp <- readline("Above: Examining the fitted values on new dataset \n ")

head(vresFL$residuals)
vtemp <- readline("Above: Examining the residuals \n ")

head(vresFL$fitted.values)
vtemp <- readline("Above: Examining the fitted values on same data \n ")
