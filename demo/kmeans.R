## kmeans demo AdapteR ##
options(debugSQL=FALSE)
if(!exists("connection")) {
    stop("Please run demo(connecting) to create connection object \n")
}
#############################################################

##generate data for clustering.50*2 matrix.
RmatrixObj <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                 matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))

FLMatrixObj <- as.FLMatrix(RmatrixObj)
vtemp <- readline("Above:Used casting to push matrix into database:")


head(FLMatrixObj)
vtemp <- readline("Examine FLMatrixObj using head")


RkmeansObj <- kmeans(RmatrixObj,2)

vtemp <- readline("As observed above, Default R function behaviour is retained for non-FL objects.")


FLKmeansObj <- kmeans(FLMatrixObj,2)

vtemp <- readline("Above: kmeans on FLMatrix object")


## .....R result printing:
RkmeansObj

## .....FL result printing:
FLKmeansObj

vtemp <- readline("As observed above, prinitng mimics default R behaviour.")

plot(FLKmeansObj,useData=RmatrixObj)

vtemp <- readline("Above: Plotting enabled for kmeans on FL objects")


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
