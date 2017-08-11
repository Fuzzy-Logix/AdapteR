## kmeans on iris dataset AdapteR ##
options(debugSQL=FALSE)
if(!exists("connection")) {
    stop("Please run demo(connecting) to create connection object \n")
}
#############################################################

irisFL <- FLTable(table = "iris", obs_id_colname = "obsid")
vtemp <- readline("Above:Used casting to push iris into database:")

head(irisFL)
vtemp <- readline("Displayed above is the head of irisFL")

## Data Preparation
irisFL <- irisFL[,setdiff(colnames(irisFL),"species")]

vtemp <- readline("Above: Subset FLTable just like data.frame subsetting.")

#### Examining data after subsetting
head(irisFL)

vtemp <- readline("Above: Examine data after subsetting.")

FLKmeansObj <- kmeans(irisFL,3)

## .....FL result printing:
FLKmeansObj

vtemp <- readline("As observed above, prinitng mimics default R behaviour.")

plot(FLKmeansObj)

vtemp <- readline("As observed above, plotting mimics default R behaviour.")


####......... FL::::Cluster Vector
FLKmeansObj$cluster

##......... FL::::Cluster Centers
FLKmeansObj$centers

##......... FL::::Clusters totss
FLKmeansObj$totss

Sys.sleep(3)

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
