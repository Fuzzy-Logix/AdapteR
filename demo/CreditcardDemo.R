## make plot side by side of dt, rf, glm.
library(pROC)
library(randomForest)
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
plot(glm.roc, limit = 1000, main = "glm-roc")
plot2.FLROC(glm.roc, limit = 1000, main = "glm-roc")



## Decision Tree.
dt.model <- rpart(Classvar ~ ., data = FLtbl)
dt.predict <- predict(dt.model,type = "prob")
length(dt.predict)
dt.roc <- roc.FLVector(FLtbl$Classvar, dt.predict)
plot.FLROC(dt.roc, limit = 1000, main = "dt-roc", method = 0)


## Random Forest:
rf.model <- randomForest(Classvar ~ ., data = FLtbl)
rf.predict <- predict(rf.model,type = "prob")
length(dt.predict)
rf.roc <- roc.FLVector(FLtbl$Classvar, rf.predict)
plot.FLROC(rf.roc, limit = 1000, main = "rf-roc", method = 0)

## combined plot:
png("combined-plot.png")
par(mfrow = c(3, 1))
plot.FLROC(rf.roc, limit = 1000, main = "rf-roc")
plot.FLROC(dt.roc, limit = 1000, main = "dt-roc")
plot(glm.roc, limit = 1000, main = "glm-roc")
dev.off()
