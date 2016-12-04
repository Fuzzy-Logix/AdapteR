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
FLwideTable <- FLTable("tblTwitterBuzz","OBSID",fetchIDs=FALSE)
vtemp <- readline("Above: wide FLTable object created. \n ")

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
deepTableName <- "tblTwitterBuzzDeepARDemo"
## here we drop the table
dropTable(deepTableName)

if(!existsRemoteTable(tableName=deepTableName)){
    FLdeepTable <- prepareData(formula         = Buzz_Magnitude ~ . ,
                               data            = FLwideTable,
                               outDeepTable    = deepTableName,
                               makeDataSparse  = 1,
                               performVarReduc = 0,
                               minStdDev       = .01,
                               maxCorrel       = .8,
                               fetchIDs        = FALSE)
} else {
    ## or you can use an already created deep table again:
    FLdeepTable <- FLTable(deepTableName,
                           obs_id_colname   = 'obs_id_colname',    
                           var_id_colnames  = 'var_id_colname', 
                           cell_val_colname = 'cell_val_colname',
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
vresFL <- lm(Buzz_Magnitude ~ ., data=FLdeepTable)

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

## plot(vresFL,method="FL")

####### END #######
#### Thank You ####
