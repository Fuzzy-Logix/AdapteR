## lm demo AdapteR ##
### Pre-setup
oldWarn <- getOption("warn")
options(warn=-1)
options(debugSQL=FALSE)

if(!exists("connection")) {
  stop("Please run demo(connecting) to create connection object \n")
}


#############################################################

ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
groupf <- gl(2, 10, 20, labels = c("Ctl","Trt"))
groupb = rep(0:1,each = 10)
weight <- c(ctl, trt)
dataframe = data.frame(weight = weight,groupf =groupf,groupb =groupb)
rownames(dataframe) <- 1:nrow(dataframe)

FLwideTable <- as.FLTable(dataframe,
                          temporary=F,
                          tableName="ARBaselmRExampleDemo",
                          drop=TRUE)
vtemp <- readline("Above: Create FLTable object by pushing R dataframe in-database \n ")

str(FLwideTable)
vtemp <- readline("Above: str prints a summary of the table \n ")

dim(FLwideTable)
vtemp <- readline("Above: the number of rows and columns of the table \n ")


head(FLwideTable,n=10,display=TRUE)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to prepare the data for fitting a linear model.\n ")


deepTableName <- "ARBaselmRExampleDemoDeep"
dropTable(deepTableName)

FLdeepTable <- prepareData(formula         = weight ~ groupb ,
                           data            = FLwideTable,
                           outDeepTableName= deepTableName,
                           fetchIDs        = FALSE)

vtemp <- readline("Press <ENTER> to start in-database linear regression. \n ")

vresFL <- lm(weight ~ groupb, data=FLdeepTable)

summary(vresFL)
vtemp <- readline("Above: summary method on fitted object \n ")

head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients and other elements like for an  \n ")

head(vresFL$residuals,n=10,display=TRUE)
vtemp <- readline("Above: Examining the residuals \n ")


head(vresFL$fitted.values,display=TRUE)
vtemp <- readline("Above: Examining the fitted values on same data \n ")


plot(vresFL,method="FL")

####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
