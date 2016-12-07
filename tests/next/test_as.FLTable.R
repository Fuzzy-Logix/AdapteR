## test case to document strange behaviour of FLRegrDataPRep
## when the dependent column is a categorical variable

library(rpart)
data(iris)

t<-as.FLTable(iris,temporary=FALSE)
deept<-FLRegrDataPrep(t,"Species")
table<-deept$table

## the table created is completely different from
## the converted widetable
print(deept)

## consequently, the function throws an error
a<-bagging(table,formula= -1~.)