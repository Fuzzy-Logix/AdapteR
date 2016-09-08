## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.

## This demo shows how the
## AdapteR package of Fuzzy Logix is
## easing interaction with the DB Lytix(TM) in-database
## library.
##
## The demo highlights how to perform
## In-Database kmeans clustering
if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}
#############################################################

##generate data for clustering.50*2 matrix.
RmatrixObj <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                 matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))

FLMatrixObj <- as.FLMatrix(RmatrixObj)
vtemp <- readline("Above:Used casting to push matrix into database:")

class(FLMatrixObj)
vtemp <- readline("Above:FLMatrixObj is a FLMatrix object")

head(FLMatrixObj)
vtemp <- readline("Examine FLMatrixObj using head")

## The input data is in the database and is not fetched
## into the R session.

##...Perform kmeans on R matrix
RkmeansObj <- kmeans(RmatrixObj,2)

vtemp <- readline("As observed above, Default R function behaviour is retained for non-FL objects.")

##...Perform kmeans on FLMatrixObj
FLKmeansObj <- kmeans(FLMatrixObj,2)

vtemp <- readline("Above: kmeans on FLMatrix object")
##...Printing result objects
## .....R result printing:
RkmeansObj

## .....FL result printing:
FLKmeansObj

vtemp <- readline("As observed above, prinitng mimics default R behaviour.")

##...Plotting result objects
## .....FL result plotting:
cat("Plot and see the clusters formed. \n ")
plot(FLKmeansObj)

vtemp <- readline("Above: Plotting enabled for kmeans on FL objects")

##...Accessing components of result object
##......... R::::Cluster Vector
RkmeansObj$cluster

##......... FL::::Cluster Vector
FLKmeansObj$cluster

Sys.sleep(3)

##......... R::::Cluster Centers
RkmeansObj$centers

##......... FL::::Cluster Centers
FLKmeansObj$centers

Sys.sleep(3)

##......... R::::Clusters totss
RkmeansObj$totss

##......... FL::::Clusters totss
FLKmeansObj$totss

Sys.sleep(3)

##......... R::::Clusters withinss
RkmeansObj$withinss

##......... FL::::Clusters withinss
FLKmeansObj$withinss

Sys.sleep(3)

##......... R::::Clusters betweenss
RkmeansObj$betweenss

##......... FL::::Clusters betweenss
FLKmeansObj$betweenss

Sys.sleep(3)

##......... R::::Clusters tot.withinss
RkmeansObj$tot.withinss

##......... FL::::Clusters tot.withinss
FLKmeansObj$tot.withinss

Sys.sleep(3)

##......... R::::Clusters size
RkmeansObj$size

##......... FL::::Clusters size
FLKmeansObj$size

vtemp <- readline("As observed above, FL Result mimics default R result.")

###...END...###
###...Thank You...###