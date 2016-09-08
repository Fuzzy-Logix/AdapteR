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
data(iris)

## Drop table ARBaseIris if already exists
vtemp <- sqlSendUpdate(connection,"DROP TABLE ARBaseIris")

irisFL <- as.FLTable(iris,tableName="ARBaseIris")
vtemp <- readline("Above:Used casting to push iris into database:")

class(irisFL)
vtemp <- readline("Above:irisFL is a FLTable object")

head(irisFL)
vtemp <- readline("Displayed above is the head of irisFL")

## Data Preparation
#### Select all columns except Species Column
irisFL <- irisFL[,setdiff(colnames(irisFL),"Species")]

vtemp <- readline("Above: Subset FLTable just like data.frame subsetting.")
#### Examining data after subsetting
head(irisFL)

vtemp <- readline("Above: Examine data after subsetting.")

## The input data is in the database and is not fetched
## into the R session.

##...Perform kmeans on irisFL
FLKmeansObj <- kmeans(irisFL,3)

## .....FL result printing:
FLKmeansObj

vtemp <- readline("As observed above, prinitng mimics default R behaviour.")


## .....FL result plotting:
plot(FLKmeansObj)

vtemp <- readline("As observed above, plotting mimics default R behaviour.")

##...Accessing components of result object
####......... FL::::Cluster Vector
FLKmeansObj$cluster

##......... FL::::Cluster Centers
FLKmeansObj$centers

##......... FL::::Clusters totss
FLKmeansObj$totss

##......... FL::::Clusters withinss
FLKmeansObj$withinss

##......... FL::::Clusters betweenss
FLKmeansObj$betweenss

##......... FL::::Clusters tot.withinss
FLKmeansObj$tot.withinss

##......... FL::::Clusters size
FLKmeansObj$size

vtemp <- readline("As observed above, FL Result mimics default R result.")

###...END...###
###...Thank You...###