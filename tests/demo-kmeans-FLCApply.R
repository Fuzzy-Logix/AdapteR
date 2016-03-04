## kmeans demo on iris with FLCApply

library(AdapteR)
library(RODBC)

if(!exists("connection"))
    connection <- flConnect(odbcHost = "Gandalf")


if(!exists("connection")){
    ## for JDBC set credential
    yourUser <- ""
    yourPassword <- ""

    ## set this to add jdbc driver and security jars to classpath: terajdbc4.jar tdgssconfig.jar
    yourJarDir <- NULL
    connection <- flConnect(host     = "10.200.4.116",
                            database = "Fl_demo",
                            user     = yourUser,
                            passwd   = yourPassword,
                            dir.jdbcjars = yourJarDir)
}
data(iris)
options(debugSQL=FALSE)


irisFL <- as.FLTable(iris,connection,"AdapteRiris2")
## or Alternatively irisFL <- FLTable(connection,"FL_DEMO","iris","rownames")

resultList <- FLCApply(data=irisFL,
                       FUN=function(x) kmeans(x,3),
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
