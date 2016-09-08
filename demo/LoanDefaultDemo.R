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
vSampleDataTables <- FLSampleData(pTableName="tblLoanData",
                                  pObsIDColumn="Loanid",
                                  pTrainTableName="ARtblLoanDataTrain",
                                  pTestTableName="ARtblLoanDataTest")
vTrainTableName <- vSampleDataTables["TrainTableName"]
vTestTableName <- vSampleDataTables["TestTableName"]

vtemp <- readline("Above: Using FLSampleData to create Train & Test Data\n ")

## Create a FLTable object for Training table
## Refer ?FLTable for help on creating FLTable Objects.
?FLTable
vtemp <- readline("Below: wide FLTable object created. \n ")
FLtbl <- FLTable(vTrainTableName,"Loanid")

vtemp <- readline("Below: Examining data structure using head \n ")
head(FLtbl)

vtemp <- readline("Below: Fitting glm model on data (Binomial family) excluding some columns\n ")
vExcludeCols <- c("sub_grade","emp_name",
                "emp_length","addr_city",
                "addr_state","bc_util",
                "earliest_cr_line")
vresFL <- glm(default_ind~.,
            data=FLtbl,
            excludeCols=vExcludeCols,
            classSpec=list(term="36 months",
                         grade="A",
                         home_ownership="RENT",
                         is_inc_v="TRUE",
                         purpose="debt_consolidation"),
            makeDataSparse=TRUE,
            minStdDev=0.1,
            maxCorrel=0.7
            )

print(vresFL)
vtemp <- readline("Above: Print method on fitted object \n ")

head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients \n ")

summary(vresFL)
vtemp <- readline("Above: summary method on fitted object \n ")

vtemp <- readline("Below: Prediction on Test dataset \n ")
FLTestTbl <- FLTable(vTestTableName,"Loanid")
FLfit <- predict(vresFL,FLTestTbl)

vtemp <- readline("Below: Examining the fitted values on new dataset \n ")
head(FLfit)

head(vresFL$residuals)
vtemp <- readline("Above: Examining the residuals \n ")

head(vresFL$fitted.values)
vtemp <- readline("Above: Examining the fitted values on same data \n ")

####### END #######
#### Thank You ####
