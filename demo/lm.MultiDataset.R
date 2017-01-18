## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.

## The following script demos the fitting of 
## linear model parallely to multiple datasets 
## using AdapteR and DB-Lytix.

## Main Value Points:
##      Ease of modelling due to syntactical similarity with R
##      Scalable in-database computation
##      Function running completely parallely across multiple datasets

## Intial set-up
oldWarn = getOption("warn")
options(warn=-1)
require(plyr)
library(plyr)

## Set-up the ODBC connection to demo database.
demo("connecting",package="AdapteR")

## Create a FLTable object (Equivalent to R's data.frame)
## to hold the metadata of the remote table

FLTableMDObject <- FLTableMD(table=getTestTableName("tblAutoMPGMD"),
                            group_id_colname="GroupID",
                            obs_id_colname="ObsID",
                            group_id = c(2,4))

readline("Above: FLTable object is created: ")

head(FLTableMDObject)

readline("Above: Examine the data structure using head ")

## You can explicitly use prepareData
## to remove highly collinear predictors
## and near-constant predictors and
## prepare data in a sparse format for
## the fast in-database linear regression
## algorithm exploiting sparseness.
deepTableName <- "tblAutoMPGMDDeepARDemo"
## here we drop the table
dropTable(deepTableName)

vformula <- MPG~HorsePower+Displacement+Weight+Acceleration

if(!existsRemoteTable(tableName=deepTableName)){
    FLdeepTableMD <- prepareData(formula         = vformula,
                               data            = FLTableMDObject,
                               outDeepTable    = deepTableName,
                               makeDataSparse  = 1,
                               performVarReduc = 0,
                               minStdDev       = .01,
                               maxCorrel       = .8,
                               fetchIDs        = FALSE)
} else {
    ## or you can use an already created deep table again:
    FLdeepTableMD <- FLTable(deepTableName,
                           obs_id_colname   = 'obs_id_colname',    
                           var_id_colnames  = 'var_id_colname', 
                           cell_val_colname = 'cell_val_colname',
                           fetchIDs = FALSE)
}
vtemp <- readline("Press <ENTER> to start in-database linear regression. \n ")

lmfit <- lm(vformula,
            data=FLdeepTableMD)

readline("Above: fitting the linear model to multiple datasets parallely")
coeffList <- coef(lmfit)
print(coeffList)

readline("Above: List of model coefficients ")
summaryList <- summary(lmfit)
print(summaryList)
readline("Above: Summary of fitted models ")

### END ####
### Thank You ####
## clean up ###
options(warn=oldWarn)
###
