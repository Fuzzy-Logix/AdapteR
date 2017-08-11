## Robust Regression TwitterBuzz Data AdapteR ##
oldWarn <- getOption("warn")
options(debugSQL=FALSE)
options(warn=-1)
if(!exists("connection")) {
    stop("Please run demo(connecting) to create connection object \n")
}

#############################################################
FLwideTable <- FLTable("tblTwitterBuzz","OBSID",
					fetchIDs=FALSE,whereconditions=" OBSID<4001 ")
vtemp <- readline("Above: wide FLTable object created. \n ")

str(FLwideTable)
vtemp <- readline("Above: str prints a summary of the table \n ")

dim(FLwideTable)
vtemp <- readline("Above: the number of rows and columns of the table \n ")

head(FLwideTable,n=10,display=TRUE)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to prepare the data for fitting a linear model.\n ")


deepTableName <- "tblTwitterBuzzDeepARDemo"
dropTable(deepTableName)
vtemp <- readline("Press <ENTER> to start in-database Robust linear regression. \n ")

vresFL <- rlm(Buzz_Magnitude ~ ., data=FLwideTable, maxiter = 100)

summary(vresFL)
vtemp <- readline("Above: summary method on fitted object \n ")


head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients and other elements like for an  \n ")


head(vresFL$residuals,n=10,display=TRUE)
vtemp <- readline("Above: Examining the residuals \n ")


head(vresFL$fitted.values,display=TRUE)
vtemp <- readline("Above: Examining the fitted values on same data \n ")

####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
