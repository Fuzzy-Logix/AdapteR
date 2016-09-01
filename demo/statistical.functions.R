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
## The demo highlights how to use
## statistical aggregation functions like mean, max
## in combination with apply.
if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}

#############################################################
## For in-database analytics the matrix is in the warehouse
## to begin with.
require(testthat)
m <- matrix(rnorm(25),5,dimnames=list(letters[1:5],letters[6:10]))

flm <- as.FL(m)

vtemp <- readline("Above: cast matrix into database")
head(m)
head(flm)

vtemp <- readline("Above: Check if both matrices are similar ")
###########################################################
## Column Means of Matrix using Apply

FLResultVec <- apply(flm,2,mean)
rResultVec <- apply(m,2,mean)

vtemp <- readline("Above: Using apply to parallely find column means ")

FLResultVec
rResultVec

vtemp <- readline("Above: checking Result equivalence using expect_equal")

############################################################
## One could similarly apply max function across dimensions 
## of an in-database matrix
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
