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
## Logistic Regression on tblLoanData dataset
## to identify the loan defaulters
if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}
#############################################################
## Create Train and Test DataSets from tblLoanData.
vSampleDataTables <- suppressWarnings(SampleData(pTableName=getTestTableName("tblLoanData"),
                                  pObsIDColumn="Loanid",
                                  pTrainTableName="ARtblLoanDataTrain",
                                  pTestTableName="ARtblLoanDataTest",
                                  pTrainDataRatio=0.3,
                                  pTemporary=FALSE,
                                  pDrop=TRUE))
vTrainTableName <- vSampleDataTables["TrainTableName"]
vTestTableName <- vSampleDataTables["TestTableName"]

vtemp <- readline("Above: Using SampleData to create Train & Test Data\n ")

## Create a FLTable object for Training table
## Refer ?FLTable for help on creating FLTable Objects.
?FLTable
FLtbl <- FLTable(vTrainTableName,"Loanid",fetchIDs=FALSE)
str(FLtbl)
FLTestTbl <- FLTable(vTestTableName,"Loanid",fetchIDs=FALSE)
str(FLTestTbl)
vtemp <- readline("Above: wide FLTable object created, and the SQL SELECT statements they represent are printed. \n ")

## Using display=TRUE fetches and returns result as R object
## Recommended for Large objects
head(FLtbl,n=10,display=TRUE)
vtemp <- readline("Above: Examining data structure using head \n ")

### Data Preparation and Fitting a binomial glm model
vtemp <- readline("Below: Fitting glm model on data (Binomial family) excluding some columns\n ")

vCategoricalCols <- c("sub_grade","emp_name",
                    "emp_length","addr_city",
                    "addr_state","bc_util",
                    "earliest_cr_line")
vresFL <- glm(default_ind~.,
            data=FLtbl,
            pThreshold=0.5,
            classSpec=list(term="36 months",
                         grade="A",
                         home_ownership="RENT",
                         is_inc_v="TRUE",
                         purpose="debt_consolidation"),
            makeDataSparse=TRUE,
            minStdDev=0.1,
            maxCorrel=0.75,
            performVarReduc=1,
            excludeCols=vCategoricalCols)


####
#### Print the ouput object. Similar to 'glm' object printing
print(vresFL)
vtemp <- readline("Above: Print method on fitted object \n ")

####
#### Examine the Coefficients. Syntax exactly mimics default stats::glm behavior
head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients \n ")

### Print residuals after scoring on same dataset as used for model training.
### (Mimics R behaviour:- Properties of glm object are supported)
head(vresFL$residuals,n=10,display=TRUE)
vtemp <- readline("Above: Examining the residuals \n ")

####
#### Summary of fit model. Similar to summary on 'glm' object
summary(vresFL)
vtemp <- readline("Above: summary method on fitted object \n ")

####
### Score on Test data using 'predict'
FLfit <- predict(vresFL,FLTestTbl)
vtemp <- readline("Above: Prediction on Test dataset \n ")

### Print y(hat) values
head(FLfit,n=10,display=TRUE)
vtemp <- readline("Above: Examining the fitted values on new dataset \n ")

glm.roc <- roc(FLTestTbl$default_ind, FLfit)
glm.roc
vtemp <- readline("Above: Area under the Precision-Recall-Curve on Test dataset \n ")

plot(glm.roc, limit = 1000, main = "glm-roc")
vtemp <- readline("Above: Plotting the Precision-Recall-Curve on Test dataset \n ")

####### END #######
#### Thank You ####
