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
## Linear Regression on Hadoop Platform 
## using the example from the stats::lm documentation

### Pre-setup
oldWarn <- getOption("warn")
options(warn=-1)


if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}


#############################################################
## Use the data from R example stats::lm 

ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
groupf <- gl(2, 10, 20, labels = c("Ctl","Trt"))
groupb = rep(0:1,each = 10)
weight <- c(ctl, trt)
dataframe = data.frame(weight = weight,groupf =groupf,groupb =groupb)
rownames(dataframe) <- 1:nrow(dataframe)

## FLTable object can be created by casting
## dataframe into FLTable object 
FLwideTable <- as.FLTable(dataframe,
                          temporary=F,
                          tableName="ARBaselmRExampleDemo",
                          drop=TRUE)
vtemp <- readline("Above: Create FLTable object by pushing R dataframe in-database \n ")

str(FLwideTable)
vtemp <- readline("Above: str prints a summary of the table \n ")

dim(FLwideTable)
vtemp <- readline("Above: the number of rows and columns of the table \n ")

## Using display=TRUE fetches and returns result as R object
## Recommended for Large objects
head(FLwideTable,n=10,display=TRUE)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to prepare the data for fitting a linear model.\n ")

## You can explicitly use prepareData
## to remove highly collinear predictors
## and near-constant predictors and
## prepare data in a sparse format for
## the fast in-database linear regression
## algorithm exploiting sparseness.
deepTableName <- "ARBaselmRExampleDemoDeep"
## Drop Deeptable if already exists
## To demo Data Prep Step
dropTable(deepTableName)


if(!existsRemoteTable(tableName=deepTableName)){
    FLdeepTable <- prepareData(formula         = weight ~ groupb ,
                               data            = FLwideTable,
                               outDeepTableName= deepTableName,
                               fetchIDs        = FALSE)
} else {
    ## or you can use an already created deep table again:
    FLdeepTable <- FLTable(deepTableName,
                           obs_id_colname   = 'obsid',    
                           var_id_colnames  = 'varid', 
                           cell_val_colname = 'numval',
                           fetchIDs = FALSE)
}
vtemp <- readline("Press <ENTER> to start in-database linear regression. \n ")

## NOTE:
## You can omit the prepareData step, and call linear regression on
## a wide table directly:
##
## > vresFL <- lm(Buzz_Magnitude ~ ., data=FLwideTable)
##
## then AdapteR is doing data prep for you automatically.
## The created deep table is accessible afterwards for
## further analyses.
vresFL <- lm(weight ~ groupb, data=FLdeepTable)

summary(vresFL)
#### Summary of fit model. Similar to summary on 'lm' object
vtemp <- readline("Above: summary method on fitted object \n ")

####
#### Examine the Coefficients. Syntax exactly mimics default stats::lm behavior
head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients and other elements like for an  \n ")

### AdapteR + DB Lytix does provide you with
### 
### Print residuals after scoring on same dataset as used for model training.
### (Mimics R behaviour: Properties of lm object are supported)
head(vresFL$residuals,n=10,display=TRUE)
vtemp <- readline("Above: Examining the residuals \n ")

### Print fitted values after scoring on same dataset as used for model training.
### (Mimics R behaviour: Properties of lm object are supported)
head(vresFL$fitted.values,display=TRUE)
vtemp <- readline("Above: Examining the fitted values on same data \n ")

### Examine the plots
plot(vresFL,method="FL")

####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
