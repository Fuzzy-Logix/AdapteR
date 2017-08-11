## Loan Default Demo AdapteR ##

if(!exists("connection")) {
  stop("Please run demo(connecting) to create connection object \n")
}
options(debugSQL=FALSE)
#############################################################

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


FLtbl <- FLTable(vTrainTableName,"Loanid",fetchIDs=FALSE)

FLTestTbl <- FLTable(vTestTableName,"Loanid",fetchIDs=FALSE)

vtemp <- readline("Above: wide FLTable object created, and the SQL SELECT statements they represent are printed. \n ")


head(FLtbl,n=10,display=TRUE)
vtemp <- readline("Above: Examining data structure using head \n ")


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

vtemp <- readline("Above: Fitting glm model on data (Binomial family) excluding some columns\n ")

print(vresFL)
vtemp <- readline("Above: Print method on fitted object \n ")


head(vresFL$coefficients)
vtemp <- readline("Above: Examining the fitted coefficients \n ")


head(vresFL$residuals,n=10,display=TRUE)
vtemp <- readline("Above: Examining the residuals \n ")


summary(vresFL)
vtemp <- readline("Above: summary method on fitted object \n ")


FLfit <- predict(vresFL,FLTestTbl)
head(FLfit,n=10,display=TRUE)
vtemp <- readline("Above: Prediction on Test dataset \n ")

glm.roc <- roc(FLTestTbl$default_ind, FLfit)
glm.roc
vtemp <- readline("Above: Area under the Precision-Recall-Curve on Test dataset \n ")

plot(glm.roc, limit = 1000, main = "glm-roc")
vtemp <- readline("Above: Plotting the Precision-Recall-Curve on Test dataset \n ")

####### END #######
#### Thank You ####
