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
vSampleDataTables <- suppressWarnings(SampleData(pTableName="ARcreditcard",
                                  pObsIDColumn="ObsID",
                                  pTrainTableName="ARcreditcardTrain",
                                  pTestTableName="ARcreditcardTest",
                                  pTrainDataRatio=0.7,
                                  pTemporary=FALSE,
                                  pDrop=TRUE))
vTrainTableName <- vSampleDataTables["TrainTableName"]
vTestTableName <- vSampleDataTables["TestTableName"]

vtemp <- readline("Above: Using SampleData to create Train & Test Data\n ")

## Create a FLTable object for Training table
## Refer ?FLTable for help on creating FLTable Objects.
## glm model , plot with auc.

FLtbl <- FLTable(vTrainTableName,"ObsID",fetchIDs=FALSE)
FLTestTbl <- FLTable(vTestTableName,"ObsID",fetchIDs=FALSE)
glm.model <- glm(Classvar ~ ., data = FLtbl, family = "binomial")
glm.predict <- predict(glm.model, newdata= FLTestTbl)
head(glm.predict, display = TRUE, n = 5)
glm.roc <- roc.FLVector(FLTestTbl$Classvar, glm.predict)
plot(glm.roc, limit = 1000)



## Decision Tree.
dt.model <- glm(Classvar ~ ., data = FLtbl, family = "binomial")
dt.predict <- predict(glm.model, newdata= FLTestTbl)
dt.roc <- roc.FLVector(FLtbl$Classvar, dt.predict)
