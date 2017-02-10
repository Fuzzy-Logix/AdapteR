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
## Random forest Classification on tblDecisionTreeMulti dataset
## to build a forest based on CART and ID3 algorithms.

### Pre-setup
oldWarn <- getOption("warn")
options(warn=-1)


if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}


#############################################################
## Create a FLTable object for tblTwitterBuzz table
## Refer ?FLTable for help on creating FLTable Objects.

## Random Forests

FLdeepTable <- FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
vtemp <- readline("Above: deep FLTable object created. \n ")

str(FLdeepTable)
vtemp <- readline("Above: str prints a summary of the table \n ")

## Using display=TRUE fetches and returns result as R object
## Recommended for Large objects
head(FLdeepTable,n=10)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to continue to build the decision tree.\n ")

## NOTE:
## You can also call the random forest function on a widetable
## where AdapteR will do the data prep automatically
## and convert the wide table to a deep table.
## The created deep table is accessible afterwards for
## further analysis.

vresFL<-randomForest(data=FLdeepTable, formula= -1~., ntree=9)
## Calling the random forest function, randomForest.

## Examine the forest details with individual trees such as parent nodes, child nodes,
## Split variables, Split values, strength of node, etc.
head(vresFL$forest[1:5])
vtemp<-readline("Above: Examining forest structure")
##

print(vresFL)
#### Printing the confusion matrix and basic forest details.
vtemp<-readline("Above: Examining the forest confusion matrix")

plot(vresFL)
#### Examining the plot for random forest
vtemp<-readline("Above: Plots for randomForest")

head(predict(vresFL),n=50,display=T)
#### Predicting the class for every observation using the already
#### created model forest. You can use a different dataset too, in this demo however,
#### since we didn't specify the new data, predict function uses the old data itself.
vtemp<-readline("Above: Examining the sample result obtained through predict function")


####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
