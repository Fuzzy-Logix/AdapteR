#' @export
randomForest<-function(data,formula,...){
	UseMethod("randomForest",data)
}

#' @export
randomForest.default<-function (formula,data=list(),...) {
    if (!requireNamespace("randomForest", quietly = TRUE)){
        stop("randomForest package needed for randomForest. Please install it.",
             call. = FALSE)
    }
    else return(randomForest::randomForest(formula=formula,data=data,...))
}

#' @export
randomForest.FLpreparedData<-function(data,...) randomForest.FLTable(data$deepx,...)


#' @export
randomForest.FLTable<-function(data,
                               formula,
                               ntree=25,
                               mtry=2,
                               nodesize=10,
                               maxdepth=5,
                               cp=0.95,...){ 
    control<-c()
    control<-c(control,
               minsplit=nodesize,
               maxdepth=maxdepth,
               cp=cp)
    x<-rpart.FLTable(data,formula,control,ntree=ntree,mtry=mtry,...)
    vfuncName<-"FLRandomForest"
    retobj<-sqlStoredProc(getFLConnection(),
                          vfuncName,
                          outputParameter=c(AnalysisID="a"),
                          pInputParameters=x$vinputcols)
    AnalysisID<-as.character(retobj[1,1])
    query<-paste0("SELECT * FROM fzzlRFPredByTree 
				   WHERE AnalysisID = ",fquote(AnalysisID), 
                  "ORDER BY 1, 2, 3, 4, 5")
    conmatrixquery<-paste0("SELECT * FROM fzzlRFConfusionMtx
					WHERE AnalysisID = ",fquote(AnalysisID), 
                           "ORDER BY 1, 2, 3")
    votesquery<-paste0("SELECT * FROM fzzlRandomForestPred 
				   WHERE AnalysisID = ",fquote(AnalysisID), 
                       "ORDER BY 1, 2, 3, 4, 5")
    forestquery<-paste0("SELECT * FROM fzzlDecisionTreeMNMD WHERE AnalysisID = ",fquote(AnalysisID),
                        "ORDER BY 1, 2, 3, 4, 5")
    votestable<-sqlQuery(getFLConnection(),votesquery)
    conmatrixtbl<-sqlQuery(getFLConnection(),conmatrixquery)
    foresttable<-sqlQuery(getFLConnection(),forestquery)
    m<-matrix(nrow = length(unique(conmatrixtbl$ObservedClass)), ncol=length(unique(conmatrixtbl$ObservedClass)))
    rownames(m)<-unique(conmatrixtbl$ObservedClass)
    colnames(m)<-unique(conmatrixtbl$ObservedClass)

    for(i in 1:length(conmatrixtbl$ObservedClass)){
        j<-conmatrixtbl[i,2]
        k<-conmatrixtbl[i,3]	
        m[j,k]<-conmatrixtbl[i,4]
    }
    m[is.na(m)]<-0
    predicted<-votestable$PredictedClass

	frame<-data.frame(NodeID=foresttable$NodeID,
					  n=foresttable$NodeSize,
					  prob=foresttable$PredictClassProb,
					  yval=foresttable$PredictClass,
					  var=foresttable$SplitVarID,
					  SplitVal=foresttable$SplitVal,
					  leftson=foresttable$ChildNodeLeft,
					  rightson=foresttable$ChildNodeRight,
					  treelevel=foresttable$TreeLevel,
					  parent=foresttable$ParentNodeID,
					  Leaf=foresttable$IsLeaf,
					  TreeID=foresttable$DatasetID)
	ntrees<-unique(frame$TreeID)
	trees<-list()
	for(l in 1:length(ntrees)){
		trees[[l]]<-subset(frame,TreeID==l)
		class(trees[[l]])<-"data.frame"	
	}

    retobj<-list(call=match.call(),
                 type="classification",
                 votes=data.frame(ObsID=votestable$ObsID,
                                  ObservedClass=votestable$ObservedClass,
                                  PredictedClass=votestable$PredictedClass,
                                  Votes=votestable$NumOfVotes),
                 predicted=as.factor(structure(predicted,names=votestable$ObsID)),
                 confusion=m,
                 classes=unique(conmatrixtbl$ObservedClass),
                 ntree=ntree,
                 mtry=mtry,
                 forest=trees,
                 data=x$data,
                 AnalysisID=AnalysisID,
                 RegrDataPrepSpecs=x$vprepspecs)
    class(retobj)<-"FLRandomForest"
    return(retobj)
}

#' @export
predict.FLRandomForest<-function(object,newdata=object$data,scoreTable="",
								 type="response",...){
	if(!is.FLTable(newdata)) stop("scoring allowed on FLTable only")
	newdata <- setAlias(newdata,"")
	vinputTable <- getTableNameSlot(newdata)
	if(scoreTable=="")
	scoreTable <- gen_score_table_name("RandomForestScore")
	vRegrDataPrepSpecs <- setDefaultsRegrDataPrepSpecs(x=object$RegrDataPrepSpecs,
                                                            values=list(...))
	deepx <- FLRegrDataPrep(newdata,depCol=vRegrDataPrepSpecs$depCol,
								ExcludeCols=vRegrDataPrepSpecs$excludeCols)
	newdatatable <- deepx$table
	newdatatable <- setAlias(newdatatable,"")
	tablename<- getTableNameSlot(newdatatable)
	vobsid <- getVariables(newdatatable)[["obs_id_colname"]]
	vvarid <- getVariables(newdatatable)[["var_id_colname"]]
	vvalue <- getVariables(newdatatable)[["cell_val_colname"]]

	vinputcols<-list()
	vinputcols <- c(vinputcols,
					TableName=tablename,
					ObsIDCol=vobsid,
					VarIDCol=vvarid,
					ValueCol=vvalue,
					InAnalysisID=object$AnalysisID,
					ScoreTable=scoreTable,
					Note=genNote("RandomForestPrediction"))
	vfuncName<-"FLRandomForestScore"
	AnalysisID <- sqlStoredProc(getFLConnection(),
								vfuncName,
								outputParameter=c(AnalysisID="a"),
								pInputParams=vinputcols)

	if(type %in% "prob"){
 	   val <- "NumOfVotes"
       x<-1/(object$ntree)
    } else {
	   val <- "PredictedClass"
	   x<-1
    }
   	

   	# yvector <- new("FLVector",
    #               select= new("FLSelectFrom",
    #                           table_name=scoreTable,
    #                           connectionName=getFLConnectionName(),
    #                           variables=list(ObsID=vobsid,
    #                           				 val=val),
    #                           whereconditions="",
    #                           order=vobsid),
    #               dimColumns = c("ObsID","val"),
    #               ##names=NULL,
    #               Dimnames = list(rownames(newdata),1),
    #               dims    = c(nrow(newdata),1),
    #               type       = "integer"
    #               )
   	sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n
   	                          ",vobsid," AS vectorIndexColumn,\n
    	                         ",val,"*",x," AS vectorValueColumn\n",
        	            " FROM ",scoreTable,"")
   	tblfunqueryobj <- new("FLTableFunctionQuery",
    	                   connectionName = getFLConnectionName(),
                           variables = list(
            	               obs_id_colname = "vectorIndexColumn",
                	           cell_val_colname = "vectorValueColumn"),
      	                   whereconditions="",
        	               order = "",
                           SQLquery=sqlstr)
    vrw <- nrow(newdata)
    yvector <- newFLVector(
    			   select = tblfunqueryobj,
       			   Dimnames = list(as.integer(1:vrw),
                   			      "vectorValueColumn"),
      			   dims = as.integer(c(vrw,1)),
 			       isDeep = FALSE)
   	return(yvector)
	# query<-paste0("Select * from ",scoreTable," Order By 1")
	# retobj<-sqlQuery(getFLConnection(),query)
	# return(as.factor(structure(retobj$PredictedClass,names=retobj$ObsID)))
}

#' @export
print.FLRandomForest<-function(object,...){
	cat("Call:\n")
	dput(object$call)
	cat("               Type of random forest: ",object$type)
	cat("\n                     Number of trees: ",object$ntree)
	cat("\nNo. of variables tried at each split: ",object$mtry)
	query<-paste0("Select * from fzzlRandomForestStat Where AnalysisID = ",fquote(object$AnalysisID))
	x<-sqlQuery(getFLConnection(),query)
	cat("\n\n        OOB estimate of error rate: ",x[1,5]*100," %")
	cat("\nConfusion matrix: \n")
	print(object$confusion)
}

#' @export
plot.FLRandomForest<-function(object){ #browser()
	if(!class(object)=="FLRandomForest") stop("The object class is not FLRandomForest")
	ntree<-object$ntree
	x<-ceiling(sqrt(ntree))
	old.par <- par(mfrow=c(x,ceiling(ntree/x)),
				   oma = c(0,0,0,0) + 0,
          		   mar = c(0,0,0,0) + 0)
	for(i in 1:ntree){
		plot.FLrpart(object$forest[[i]])
	}
}	
