Renv=new.env(globalenv())
FLenv=as.FL(Renv)

FLenv$table<-FLTable("tblBoostDT","ObsID","VarID","Num_Val")
Renv$table<-as.data.frame(FLenv$table)
Renv$table$`-1`<-as.factor(Renv$table$`-1`)
colnames(Renv$table)<-paste0("Col",1:ncol(Renv$table))
FLenv$newdata<-FLenv$table[1:150,]
Renv$newdata<-as.data.frame(FLenv$newdata)
Renv$newdata$`-1`<-as.factor(Renv$newdata$`-1`)
colnames(Renv$newdata)<-paste0("Col",1:ncol(Renv$newdata))
mfinal<-5

test_that("test for boosting on deeptables",{
  flobj<-boosting(FLenv$table, formula = -1~.,mfinal=mfinal)
  robj <- boosting(Col1~., data = Renv$table,mfinal=mfinal)
  result1= expect_equal(mfinal,length(flobj$trees))
  result2= expect_equal(all(unique(flobj$class)) %in% c(1,2,3) , TRUE)
  result3= expect_equal(as.numeric(rownames(FLenv$table)),as.numeric(flobj$votes$ObsID))
  result4= expect_equal(flobj$prob$PredictClassProb,(flobj$votes$Votes)/mfinal)
})

## @amal: this test fails due to an issue in subsetting FLTable
test_that("test for prediction in boostingDTs",{
  flobj<-boosting(FLenv$table, formula = -1~.,mfinal=mfinal)
  robj <- boosting(Col1~., data = Renv$table,mfinal=mfinal)
  flobj1<-predict(flobj, newdata=FLenv$newdata)
  robj1<-predict(robj, newdata = Renv$newdata)
  result1 = expect_equal(nrow(flobj1$pred), nrow(robj1$votes))
  result2 = expect_equal(nrow(flobj1$pred), nrow(robj1$prob))
  result3 = expect_equal(sum(flobj1$confusion), sum(robj1$confusion))
})
