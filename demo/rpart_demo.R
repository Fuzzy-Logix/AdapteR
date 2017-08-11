## Demo rpart AdapteR ##
oldWarn <- getOption("warn")
options(debugSQL=FALSE)
options(warn=-1)
if(!exists("connection")) {
    stop("Please run demo(connecting) to create connection object \n")
}
#############################################################

FLdeepTable <- FLTable(getTestTableName("tblDecisionTreeMulti"),
                        "ObsID","VarID","Num_Val")
vtemp <- readline("Above: deep FLTable object created. \n ")

str(FLdeepTable)
vtemp <- readline("Above: str prints a summary of the table \n ")

head(FLdeepTable,n=10)
vtemp <- readline("Above: Head is supported to examine structure of data \n Press <enter> to continue to build the decision tree.\n ")

vresFL<-rpart(data=FLdeepTable, formula= -1~.)

head(vresFL$frame)
vtemp<-readline("Above: Examining decision tree structure details")
##

summary(vresFL)
vtemp <- readline("Above: summary method on created tree \n ")

print(vresFL)
vtemp<-readline("Above: Examining the created tree")

plot(vresFL)
vtemp<-readline("Above: Plot for decision tree")

head(predict(vresFL))
vtemp<-readline("Above: Examining the sample result obtained through predict function")


####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
