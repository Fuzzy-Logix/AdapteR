## Fit Linear Models on Multiple Datasets parallely.
## Intial set-up
oldWarn = getOption("warn")
options(warn=-1)
options(debugSQL=FALSE)
require(plyr)
library(plyr)
if(!exists("connection")) {
    stop("Please run demo(connecting) to create connection object \n")
}

##########################################################################

FLTableMDObject <- FLTableMD(table=getTestTableName("tblAutoMPGMD"),
                            group_id_colname="GroupID",
                            obs_id_colname="ObsID",
                            group_id = c(2,4))

readline("Above: FLTable object is created: ")

head(FLTableMDObject)

readline("Above: Examine the data structure using head ")


deepTableName <- "tblAutoMPGMDDeepARDemo"
dropTable(deepTableName)

vformula <- MPG~HorsePower+Displacement+Weight+Acceleration
if(is.TDAster())
  vformula <- mpg~horsepower+displacement+weight+acceleration

FLdeepTableMD <- prepareData(formula         = vformula,
                           data            = FLTableMDObject,
                           outDeepTable    = deepTableName,
                           makeDataSparse  = 1,
                           performVarReduc = 0,
                           minStdDev       = .01,
                           maxCorrel       = .8,
                           fetchIDs        = FALSE)

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
