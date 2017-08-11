## apply functionality of In-Database objects ##
require(testthat)
if(!exists("connection")) {
    stop("Run demo(connecting) to create a connection object \n")
}
options(debugSQL=FALSE)
#############################################################

m <- matrix(rnorm(25),5,dimnames=list(letters[1:5],letters[6:10]))

flm <- as.FL(m)

vtemp <- readline("Above: cast matrix into database")
head(m)
head(flm)

vtemp <- readline("Above: Check if both matrices are similar ")
###########################################################

FLResultVec <- apply(flm,2,mean)
rResultVec <- apply(m,2,mean)

vtemp <- readline("Above: Using apply to parallely find column means ")

FLResultVec
rResultVec

vtemp <- readline("Above: checking Result equivalence using expect_equal")

############################################################

FLResultVec <- apply(flm,1,max)
rResultVec <- apply(m,1,max)
FLResultVec
rResultVec

vtemp <- readline("Above: Finding max across rows for matrix")

############
cat("Similarly using AdapteR several scalar functions from DB-Lytix ",
    "can be parallelly evaluated across dimensions of an in-database matrix \n ")


### END ####
### Thank You ####
