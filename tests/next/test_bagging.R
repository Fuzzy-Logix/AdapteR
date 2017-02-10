Renv=new.env(globalenv())
FLenv=as.FL(Renv)

FLenv$table<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
Renv$table<-as.data.frame(FLenv$table)
Renv$table$`-1`<-as.factor(Renv$table$`-1`)
colnames(Renv$table)<-paste0("Col",1:ncol(Renv$table))
FLenv$newdata<-FLenv$table[1:150,]
mfinal<-5

test_that("test for bagging on deeptables",{
	flobj<-bagging(FLenv$table, formula = -1~.,mfinal=mfinal)
	robj <- bagging(Col1~., data = Renv$table,mfinal=mfinal)
	result1= expect_equal(mfinal,length(flobj$trees))
	result2= expect_equal(as.integer(unique(Renv$table$Col1)),unique(flobj$class))
	result3= expect_equal(as.numeric(rownames(FLenv$table)),as.numeric(rownames(flobj$votes)))
})

test_that("test for predict in bagging",{
  flobj<-bagging(FLenv$table, formula = -1~.,mfinal=mfinal)
  flobj1<-predict(flobj, FLenv$newdata)
  flobj2<-predict(flobj, FLenv$newdata, type = "prob")
  result1 = expect_equal(nrow(flobj1), nrow(FLenv$newdata))
  result3 = expect_equal(nrow(flobj2), nrow(FLenv$newdata))
})
