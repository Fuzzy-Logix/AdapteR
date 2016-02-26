## kmeans demo on iris with FLCApply

library(AdapteR)
library(RODBC)
connection <- odbcConnect("Gandalf")
data(iris)
options(debugSQL=FALSE)
FLStartSession(connection)
options(debugSQL=FALSE)
irisFL <- as.FLTable(iris,connection,"AdapteRiris2")  ## or Alternatively irisFL <- FLTable(connection,"FL_DEMO","iris","rownames")
resultList <- FLCApply(data=irisFL,
						FUN=function(x)kmeans(x,3),
						column="Species")

print(resultList$setosa)
print(resultList$virginica)
print(resultList$versicolor)

resultList$setosa$centers
resultList$setosa$cluster
resultList$setosa$withinss

resultList$virginica$centers
resultList$virginica$cluster
resultList$virginica$withinss

plot(resultList$virginica)
plot(resultList$versicolor)
plot(resultList$setosa)
