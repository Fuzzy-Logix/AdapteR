Renv=new.env(globalenv())
FLenv=as.FL(Renv)

FLenv$table<-FLTable("tblBoostDT","ObsID","VarID","Num_Val")
Renv$table<-as.data.frame(FLenv$table)
Renv$table$`-1`<-as.factor(Renv$table$`-1`)
colnames(Renv$table)<-paste0("Col",1:ncol(Renv$table))
mfinal<-5

test_that("test for boosting on deeptables",{
  flobj<-boosting(FLenv$table, formula = -1~.,mfinal=mfinal)
  robj <- boosting(Col1~., data = Renv$table,mfinal=mfinal)
  result1= expect_equal(mfinal,length(flobj$trees))
  result2= expect_equal(as.integer(unique(Renv$table$Col1)),unique(flobj$class))
  result3= expect_equal(as.numeric(rownames(FLenv$table)),as.numeric(flobj$votes$ObsID))
  result4= expect_equal(flobj$prob$PredictClassProb,(flobj$votes$Votes)/max(flobj$votes$Votes))
})

test_that("test for prediction in boostingDTs",{
  flobj<-boosting(FLenv$table, formula = -1~.,mfinal=mfinal)
  flobj1<-predict(flobj, FLenv$newdata)
  result1 = expect_equal(nrow(flobj1), nrow(FLenv$newdata))
})
