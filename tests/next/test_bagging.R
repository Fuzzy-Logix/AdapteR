Renv=new.env(globalenv())
FLenv=as.FL(Renv)

FLenv$table<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
Renv$table<-as.data.frame(FLenv$table)
Renv$table$`-1`<-as.factor(Renv$table$`-1`)
colnames(Renv$table)<-paste0("Col",1:ncol(Renv$table))
FLenv$newdata<-FLenv$table[1:150,]
Renv$newdata<-as.data.frame(FLenv$newdata)
Renv$newdata$`-1`<-as.factor(Renv$newdata$`-1`)
colnames(Renv$newdata)<-paste0("Col",1:ncol(Renv$newdata))
mfinal<-5

test_that("test for bagging on deeptables",{
  flobj<-bagging(FLenv$table, formula = -1~.,mfinal=mfinal)
  robj <- bagging(Col1~., data = Renv$table,mfinal=mfinal)
  result1= expect_equal(mfinal,length(flobj$trees))
  result2= expect_equal(all(unique(flobj$class)) %in% c(1,2,3) , TRUE)
  result3= expect_equal(as.numeric(rownames(FLenv$table)),as.numeric(rownames(flobj$votes)))
})

test_that("test for predict in bagging",{
  flobj<-bagging(FLenv$table, formula = -1~.,mfinal=mfinal)
  robj <- bagging(Col1~., data = Renv$table,mfinal=mfinal)
  flobj1<-predict(flobj, FLenv$newdata)
  robj1<-predict(robj, newdata = Renv$newdata)
  flobj2<-predict(flobj, FLenv$newdata, type = "prob")
  robj2<-predict(robj, newdata = Renv$newdata, type = "prob")
  result1 = expect_equal(nrow(flobj1$votes), nrow(robj1$votes))
  result2 = expect_equal(nrow(flobj1$prob), nrow(robj1$prob))
  result3 = expect_equal(sum(flobj1$confusion), sum(robj1$confusion))
  result4 = expect_equal(nrow(flobj2$votes), nrow(robj2$votes))
  result5 = expect_equal(nrow(flobj2$prob), nrow(robj2$prob))
  result6 = expect_equal(sum(flobj2$confusion), sum(robj2$confusion))
})
