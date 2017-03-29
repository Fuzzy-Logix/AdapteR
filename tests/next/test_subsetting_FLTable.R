## Test case to highlight the functional inadequacies in AdapteR for FLTable, FLMatrix and FLVector.
## asana - https://app.asana.com/0/143778401455745/285219081470787

table<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
newdata<-table[1:150,]
ntree<-5

test_that("Test case to highlight subsetting issues in FLMatrix and FLTable",{
  flobj<-randomForest(table, formula = -1~.)
  flmat<-predict(flobj, type = "prob")
  rmat<-as.matrix(flmat)
  tablename<-flmat@select@table_name
  fltab<-FLTable(tablename,"ObsID")
  rtab<-as.data.frame(fltab)
  expect_equal(nrow(rtab[rtab$PredictedClass==1,]),nrow(fltab[fltab$PredictedClass==1,]))
  expect_equal(length(flmat[flmat==1]),length(rmat[rmat==1]))
})

test_that("Test case to highlight non applicability of unique function to FLvectors",{
  flobj<-randomForest(table, formula = -1~.)
  flv<-predict(flobj, type = "response")
  rv<-as.vector(flv)
  expect_equal(length(unique(flv)),length(unique(rv)))
})