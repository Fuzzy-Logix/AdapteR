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
## Decision Tree Classification on tblDecisionTreeMulti dataset
## to build a tree based on CART algorithm.

### Pre-setup
oldWarn <- getOption("warn")
options(warn=-1)


if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}


#############################################################
## Create a FLTable object for tblTwitterBuzz table
## Refer ?FLTable for help on creating FLTable Objects.
?FLTable
FLdeepTable <- FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
vtemp <- readline("Above: deep FLTable object created. \n ")

str(FLdeepTable)
vtemp <- readline("Above: str prints a summary of the table \n ")

dim(FLdeepTable)
vtemp <- readline("Above: the number of rows and columns of the table \n ")

## Using display=TRUE fetches and returns result as R object
## Recommended for Large objects
head(FLdeepTable,n=10)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to continue to build the decision tree.\n ")

## NOTE:
## You can also call the decision tree function on a widetable
## where AdapteR will do the data prep automatically
## and convert the wide table to a deep table.
## The created deep table is accessible afterwards for
## further analysis.

vresFL<-rpart(data=FLdeepTable, formula= -1~.)
## Calling the decision tree function, rpart.

## Examine the tree details such as parent nodes, child nodes,
## Split variables, Split values, strength of node, etc.
head(vresFL$frame)
vtemp<-readline("Above: Examining decision tree structure details")
##

summary(vresFL)
#### Summary of the created classification tree. Similar to summary of 'rpart' object.
vtemp <- readline("Above: summary method on created tree \n ")

print(vresFL)
#### Printing the created tree in a similar manner as R.
vtemp<-readline("Above: Examining the created tree")

plot(vresFL)
#### Examining the plot for decision tree
vtemp<-readline("Above: Plot for decision tree")

head(predict(vresFL))
#### Predicting the class for every observation using the already
#### created model tree. You can use a different dataset too, in this demo however,
#### since we didn't specify the new data, predict function uses the old data itself.
vtemp<-readline("Above: Examining the sample result obtained through predict function")


####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)