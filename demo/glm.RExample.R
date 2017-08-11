### Pre-setup
oldWarn <- getOption("warn")
options(warn=-1)
options(debugSQL=FALSE)

if(!exists("connection")) {
  stop("run demo connecting to create a connection object \n")
}


#############################################################
## Use the data from R example stats::lm 

dataframe <- data.frame(var1 = rnorm(200),
                        var2 = rnorm(200), 
                        var3 = sample( c(0, 1), 200, replace = TRUE))

FLwideTable <- as.FLTable(dataframe,
                          temporary=F,
                          tableName="ARBaseglmRExampleDemo",
                          drop=TRUE)
vtemp <- readline("Above: Create FLTable object by pushing R dataframe in-database \n ")

str(FLwideTable)
vtemp <- readline("Above: str prints a summary of the table \n ")

dim(FLwideTable)
vtemp <- readline("Above: the number of rows and columns of the table \n ")


head(FLwideTable,n=10,display=TRUE)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to prepare the data for fitting the model.\n ")


deepTableName <- "ARBaseglmRExampleDemoDeep"
dropTable(deepTableName)
FLdeepTable <- prepareData(formula         = var3 ~ var1 + var2 ,
                           data            = FLwideTable,
                           outDeepTableName= deepTableName,
                           fetchIDs        = FALSE)

vtemp <- readline("Press <ENTER> to start in-database logistic regression. \n ")

## vresFL <- glm(var3 ~ var1 + var2, data=FLwideTable, family = "binomial")

vresFL <- glm(var3 ~ var1 + var2, data=FLdeepTable, family = "binomial")

summary(vresFL)

vtemp <- readline("Above: summary method on fitted object \n ")

head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients and other elements like for an  \n ")


head(vresFL$residuals,n=10,display=TRUE)
vtemp <- readline("Above: Examining the residuals \n ")


head(vresFL$fitted.values,display=TRUE)
vtemp <- readline("Above: Examining the fitted values on same data \n ")

plot(vresFL,method="R")

####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
