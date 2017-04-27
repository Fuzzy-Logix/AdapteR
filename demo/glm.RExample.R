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
## Logistic Regression on Hadoop Platform 
## using the randomly generated data

### Pre-setup
oldWarn <- getOption("warn")
options(warn=-1)


if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}


#############################################################
## Use the data from R example stats::lm 

dataframe <- data.frame(var1 = rnorm(200),
                        var2 = rnorm(200), 
                        var3 = sample( c(0, 1), 200, replace = TRUE))

## FLTable object can be created by casting
## dataframe into FLTable object 
FLwideTable <- as.FLTable(dataframe,
                          temporary=F,
                          tableName="ARBaseglmRExampleDemo",
                          drop=TRUE)
vtemp <- readline("Above: Create FLTable object by pushing R dataframe in-database \n ")

str(FLwideTable)
vtemp <- readline("Above: str prints a summary of the table \n ")

dim(FLwideTable)
vtemp <- readline("Above: the number of rows and columns of the table \n ")

## Using display=TRUE fetches and returns result as R object
## Recommended for Large objects
head(FLwideTable,n=10,display=TRUE)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to prepare the data for fitting the model.\n ")

## You can explicitly use prepareData
## to remove highly collinear predictors
## and near-constant predictors and
## prepare data in a sparse format for
## the fast in-database logistic regression
## algorithm exploiting sparseness.


if(!exists("FLdeepTable")){
    deepTableName <- "ARBaseglmRExampleDemoDeep"
    ## Drop Deeptable if already exists
    ## To demo Data Prep Step
    dropTable(deepTableName)
    if(!existsRemoteTable(tableName=deepTableName)){
        FLdeepTable <- prepareData(formula         = var3 ~ var1 + var2 ,
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
}
vtemp <- readline("Press <ENTER> to start in-database logistic regression. \n ")

## NOTE:
## You can omit the prepareData step, and call logistic regression on
## a wide table directly:
##
## > vresFL <- glm(var3 ~ var1 + var2, data=FLwideTable, family = "binomial")
##
## then AdapteR is doing data prep for you automatically.
## The created deep table is accessible afterwards for
## further analyses.
if(!exists("vresFL"))
vresFL <- glm(var3 ~ var1 + var2, data=FLdeepTable, family = "binomial")

summary(vresFL)
#### Summary of fit model. Similar to summary on 'glm' object
vtemp <- readline("Above: summary method on fitted object \n ")

####
#### Examine the Coefficients. Syntax exactly mimics default stats::glm behavior
head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients and other elements like for an  \n ")

### AdapteR + DB Lytix does provide you with
### 
### Print residuals after scoring on same dataset as used for model training.
### (Mimics R behaviour: Properties of glm object are supported)
head(vresFL$residuals,n=10,display=TRUE)
vtemp <- readline("Above: Examining the residuals \n ")

### Print fitted values after scoring on same dataset as used for model training.
### (Mimics R behaviour: Properties of glm object are supported)
head(vresFL$fitted.values,display=TRUE)
vtemp <- readline("Above: Examining the fitted values on same data \n ")

### Examine the plots
plot(vresFL,method="R")

####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
