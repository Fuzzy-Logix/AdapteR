## make plot side by side of dt, rf, glm.
library(pROC)
if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}
#############################################################
## Create Train and Test DataSets from ARcreditcard.
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
FLtbl <- FLTable(vTrainTableName,"ObsID",fetchIDs=FALSE)
FLTestTbl <- FLTable(vTestTableName,"ObsID",fetchIDs=FALSE)


## glm model , plot with auc.
glm.model <- glm(Classvar ~ ., data = FLtbl, family = "binomial")
glm.predict <- predict(glm.model, newdata= FLTestTbl)
head(glm.predict, display = TRUE, n = 5)
glm.roc <- roc.FLVector(FLtbl$Classvar, glm.predict)
plot(glm.roc, limit = 1000)



## Decision Tree.
dt.model <- rpart(Classvar ~ ., data = FLtbl)
dt.predict <- predict(dt.model,type = "prob")
length(dt.predict)
dt.roc <- roc.FLVector(FLtbl$Classvar, dt.predict)
plot(dt.roc, limit = 1000)
