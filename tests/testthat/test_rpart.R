Renv=new.env(globalenv())
FLenv=as.FL(Renv)

FLenv$table<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
Renv$table<-as.data.frame(FLenv$table)
Renv$table$`-1`<-as.factor(Renv$table$`-1`)
colnames(Renv$table)<-paste0("Col",1:ncol(Renv$table))

print(methods("rpart"))

test_that("test for decision tree on deep tables",{
  flobj<-rpart(FLenv$table, formula = -1~.)
  robj <- rpart(Col1~., data = Renv$table,method = "class")
  result1= expect_equal(flobj$frame[1,"n"],robj$frame[1,"n"])
  result3= expect_equal(as.numeric(rownames(flobj$frame)),as.numeric(flobj$frame$NodeID))
  result5= expect_equal(any(flobj$frame$var=="<leaf>"),TRUE)
})
