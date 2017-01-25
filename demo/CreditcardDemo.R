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
                                  pTrainDataRatio=0.3,
                                  pTemporary=FALSE,
                                  pDrop=TRUE))
vTrainTableName <- vSampleDataTables["TrainTableName"]
vTestTableName <- vSampleDataTables["TestTableName"]

vtemp <- readline("Above: Using SampleData to create Train & Test Data\n ")

## Create a FLTable object for Training table
## Refer ?FLTable for help on creating FLTable Objects.
## glm model , plot with auc.
?FLTable
FLtbl <- FLTable("ARcreditcardTrain","ObsID",fetchIDs=FALSE)
str(FLtbl)
FLTestTbl <- FLTable("ARcreditcardTest","ObsID",fetchIDs=TRUE)
str(FLTestTbl)
vtemp <- readline("Above: wide FLTable object created. \n ")
glm.model <- glm(Classvar ~ ., data = FLtbl, family = "binomial")
glm.predict <- predict(glm.model, newdata= FLTestTbl)
head(glm.predict, display = TRUE, n = 5)
flmod <- roc.FLVector(FLTestTbl$Classvar, glm.predict)
plot(flmod, limit = 1000)
