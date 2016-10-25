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
?FLTable
FLtbl <- FLTable("tblTwitterBuzz","OBSID",fetchIDs=FALSE)
vtemp <- readline("Above: wide FLTable object created. \n ")

str(FLtbl)
vtemp <- readline("Above: str prints a summary of the table \n ")

dim(FLtbl)
vtemp <- readline("Above: the number of rows and columns of the table \n ")

## Using display=TRUE fetches and returns result as R object
## Recommended for Large objects
head(FLtbl,n=10,display=TRUE)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to fit an lm model on data\n(automatically running data prep only if a wide table is provided.)\n ")

vresFL <- lm(Buzz_Magnitude~.,data=FLtbl)

####
#### Print the ouput object. Similar to 'lm' object printing
print(vresFL)
vtemp <- readline("Above: Print method on fitted object \n ")

####
#### Examine the Coefficients. Syntax exactly mimics default stats::lm behavior
head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients \n ")

####
#### Summary of fit model. Similar to summary on 'lm' object
summary(vresFL)
vtemp <- readline("Above: summary method on fitted object \n ")

### Print residuals after scoring on same dataset as used for model training.
### (Mimics R behaviour:- Properties of lm object are supported)
head(vresFL$residuals,n=10,display=TRUE)
vtemp <- readline("Above: Examining the residuals \n ")

### Print fitted values after scoring on same dataset as used for model training.
### (Mimics R behaviour:- Properties of lm object are supported)
head(vresFL$fitted.values,display=TRUE)
vtemp <- readline("Above: Examining the fitted values on same data \n ")

####### END #######
#### Thank You ####
