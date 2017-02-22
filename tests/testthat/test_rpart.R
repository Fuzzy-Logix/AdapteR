Renv=new.env(globalenv())
FLenv=as.FL(Renv)

FLenv$table<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
Renv$table<-as.data.frame(FLenv$table)
Renv$table$`-1`<-as.factor(Renv$table$`-1`)
FLenv$newdata<-FLenv$table[1:150,]
colnames(Renv$table)<-paste0("Col",1:ncol(Renv$table))

print(methods("rpart"))

test_that("test for decision tree on deep tables",{
  flobj<-rpart(FLenv$table, formula = -1~.)
  robj <- rpart(Col1~., data = Renv$table,method = "class")
  result1= expect_equal(flobj$frame[1,"n"],robj$frame[1,"n"])
  result3= expect_equal(as.numeric(rownames(flobj$frame)),as.numeric(flobj$frame$NodeID))
  result5= expect_equal(any(flobj$frame$var=="<leaf>"),TRUE)
})

test_that("test for prediction in rpart",{
  flobj<-rpart(FLenv$table, formula = -1~.)
  flobj1<-predict(flobj, FLenv$newdata)
  flobj2<-predict(flobj, FLenv$newdata, type = "prob")
  result1 = expect_equal(length(flobj1), nrow(FLenv$newdata))
  result2 = expect_equal(nrow(flobj2), nrow(FLenv$newdata))
})

test_that("test for printing decision tree object",{
  flobj<-rpart(FLenv$table, formula = -1~.)
  result1= expect_output(print(flobj))
  result2= expect_output(print(flobj),paste0("n= ",flobj$frame[1,"n"]))
  for(i in 1:nrow(flobj$frame)){
    expect_output(print(flobj),paste0(i,")"))
  }
})
