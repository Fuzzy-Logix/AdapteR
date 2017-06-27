### testcases doesn't runs on Aster because function flrandomforest does not exist.
### Asana Ticket: https://app.asana.com/0/136555696724838/371749207625403
Renv=new.env(globalenv())
FLenv=as.FL(Renv)

FLenv$table<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
Renv$table<-as.data.frame(FLenv$table)
Renv$table$`-1`<-as.factor(Renv$table$`-1`)
colnames(Renv$table)<-paste0("Col",1:ncol(Renv$table))
FLenv$newdata<-FLenv$table[1:150,]
ntree<-5

test_that("test for randomForests on deeptables",{
  flobj<-randomForest(FLenv$table, formula = -1~.,ntree=ntree)
  robj <- randomForest(Col1~., data = Renv$table,ntree=ntree)
  result1= expect_equal(flobj$ntree,length(flobj$forest))
  result2= expect_equal(as.integer(unique(Renv$table$Col1)),unique(flobj$classes))
  result3= expect_equal(as.numeric(rownames(FLenv$table)),as.numeric(flobj$votes$ObsID))
  result4= expect_equal(length(flobj$predicted),length(robj$predicted))
  result5= expect_equal(sum(flobj$confusion),length(robj$y))
})

test_that("test for predict in randomForest",{
  flobj<-randomForest(FLenv$table, formula = -1~.,ntree=ntree)
  flobj1<-predict(flobj, FLenv$newdata)
  flobj2<-predict(flobj, FLenv$newdata, type = "prob")
  flobj3<-predict(flobj, FLenv$newdata, type = "votes")
  flobj4<-predict(flobj, FLenv$newdata, type = "link")
  result1 = expect_equal(length(flobj1), nrow(FLenv$newdata))
  result2 = expect_equal(nrow(flobj2), nrow(FLenv$newdata))
  result3 = expect_equal(nrow(flobj3), nrow(FLenv$newdata))
  result3 = expect_equal(ncol(flobj2),length(unique(as.vector(flobj1))))
  result4 = expect_equal(ncol(flobj3),length(unique(as.vector(flobj1))))
  result5 = expect_equal(nrow(flobj4), nrow(FLenv$newdata))
})

test_that("test for summary in randomForest",{
    flobj<-randomForest(FLenv$table, formula = -1~.,ntree=ntree)
    flobj1<-summary(flobj)
    expect_equal(length(flobj1)-1 , ncol(combn(length(unique(as.vector(flobj$predicted))),2)))
    expect_equal(flobj1[[1]]$auc>0,TRUE)
  })
