## Demo randomForest AdapteR ##
oldWarn <- getOption("warn")
options(warn=-1)
options(debugSQL=FALSE)
if(!exists("connection")) {
    stop("Please run demo(connecting) to create connection object \n")
}


FLdeepTable <- FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
vtemp <- readline("Above: deep FLTable object created. \n ")

str(FLdeepTable)
vtemp <- readline("Above: str prints a summary of the table \n ")

head(FLdeepTable,n=10)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to continue to build the decision tree.\n ")


vresFL<-randomForest(data=FLdeepTable, formula= -1~., ntree=9)

head(vresFL$forest[1:5])
vtemp<-readline("Above: Examining forest structure")
##

print(vresFL)
#### Printing the confusion matrix and basic forest details.
vtemp<-readline("Above: Examining the forest confusion matrix")


head(predict(vresFL),n=50,display=T)
vtemp<-readline("Above: Examining the sample result obtained through predict function")


####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
