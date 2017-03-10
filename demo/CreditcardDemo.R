## make plot side by side of dt, rf, glm.
##str <- paste0("SELECT a.vectorValueColumn AS depVar FROM (",constructSelect(depVar),") AS ## a ORDER BY a.vectorIndexColumn")
## use deep table instead of wide as consuming more time.
## ARBaseARcreditcardTrainD1485952077
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
                                  pTrainDataRatio=.05,
                                  pTemporary=FALSE,
                                  pDrop=TRUE))
vTrainTableName <- vSampleDataTables["TrainTableName"]
vTestTableName <- vSampleDataTables["TestTableName"]

vtemp <- readline("Above: Using SampleData to create Train & Test Data\n ")


deepTableName<- "ARBaseARcreditcardTrainD1485952077"
dropTable(deepTableName)

## Create a FLTable object for Training table
FLtbl <- FLTable(vTrainTableName,"ObsID",fetchIDs=FALSE)
FLTestTbl <- FLTable(vTestTableName,"ObsID",fetchIDs=FALSE)

vdependentColumn <- "Classvar"

myformula <- eval(parse(text=paste0(vdependentColumn,"~.")))
if(!existsRemoteTable(tableName=deepTableName)){
    FLdeepTable <- prepareData(formula         = myformula ,
                               data            = FLtbl,
                               outDeepTable    = deepTableName,
                               makeDataSparse  = 1,
                               performVarReduc = 0,
                               minStdDev       = .01,
                               maxCorrel       = .8,
                               fetchIDs        = FALSE)
} else {
    ## or you can use an already created deep table again:
    FLdeepTable <- FLTable(deepTableName,
                           obs_id_colname   = 'obsid',    
                           var_id_colnames  = 'varid', 
                           cell_val_colname = 'numval',
                           fetchIDs = FALSE)
}

## glm model , plot with auc.
glm.model <- glm(myformula, data = FLdeepTable, family = "binomial")
glm.predict <- predict(glm.model)
head(glm.predict, display = TRUE, n = 5)
glm.roc <- roc.FLVector(FLtbl$Classvar, glm.predict)
plot(glm.roc, limit = 1000, main = "glm-roc")
plot2.FLROC(glm.roc, limit = 1000, main = "glm-roc")



## Decision Tree.
## change purity level  -> .999
dt.model <- rpart(myformula,data = FLdeepTable, control = c(minsplit = 15, cp = .9999, maxdepth = 10))
dt.predict <- predict(dt.model,type = "prob")
length(dt.predict)
dt.roc <- roc.FLVector(FLtbl$Classvar, dt.predict)
plot(dt.roc, limit = 1000, main = "dt-roc", method = 0)

##
#### Random Forest:
##rf.model <- randomForest(myformula,  data = FLdeepTable, minsplit = 15, cp = .9999, maxdepth = 7)
##rf.predict <- predict(rf.model,type = "prob")
##length(rf.predict)
##rf.roc <- roc.FLVector(FLtbl$Classvar, rf.predict)
##plot.FLROC(rf.roc, limit = 1000, main = "rf-roc", method = 0)
##
##
##
##
##
#### combined plot:
##png("combined-plot1.png")
##par(mfrow = c(2, 1))
####rf.plot <- plot(rf.roc, limit = 1000, main = "rf-roc")
####ch <- paste0("auc of ",round(rf.plot$auc, digits=3))
####mtext(ch, side = 3)
##dt.plot <- plot(dt.roc, limit = 1000, main = "dt-roc", method = 0)
##ch <- paste0("auc of ",round(dt.plot$auc, digits=3))
##mtext(ch, side = 3)
##glm.plot <- plot(glm.roc, limit = 1000, main = "glm-roc")
##ch <- paste0("auc of ",round(glm.plot$auc, digits=3))
##mtext(ch, side = 3)
##dev.off()
##
##   
##
