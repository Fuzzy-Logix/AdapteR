#' @export
NULL

#' Classification and Regression with Random Forest
#'
#' Random forest is a technique for reducing
#' the variance of an estimated prediction function.
#' It takes multiple random samples(with replacement) from the training
#' data set, uses each of these samples to construct a separate model and separate predictions for test set, and
#' then averages them.
#'
#' @param data FLTable
#' @param formula formula specifying the independent and dependent variable columns
#' @param ntree	Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
#' @param mtry	Number of variables randomly sampled as candidates at each split
#' @param nodesize Minimum size of terminal nodes.
#' @param maxdepth The maximum depth to which the tree can go.
#' cp: Complexity parameter
#'
#' @return An object of class "FLRandomForest" containing the forest structure details.
#' @examples
#' flt<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
#' flobj<-randomForest(data = flt, formula = -1~., ntree=5)

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
	m<-matrix(nrow = max(conmatrixtbl$ObservedClass)-min(conmatrixtbl$ObservedClass)+1, ncol=max(conmatrixtbl$PredictedClass)-min(conmatrixtbl$PredictedClass)+1)
	rownames(m)<-min(conmatrixtbl$ObservedClass):max(conmatrixtbl$ObservedClass)
	colnames(m)<-min(conmatrixtbl$PredictedClass):max(conmatrixtbl$PredictedClass)

	for(i in 1:length(conmatrixtbl$ObservedClass)){
  		j<-conmatrixtbl[i,2]
  		k<-conmatrixtbl[i,3]	
 		m[as.character(j),as.character(k)]<-conmatrixtbl[i,4]
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
predict.FLRandomForest<-function(object,newdata=object$data,
								 scoreTable="",
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
   	   x<-1/(object$ntree)}
	else{
	   val <- "PredictedClass"
	   x<-1}
   	

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

summary.FLRandomForest<-function(object){ #browser()
	if(!class(object)=="FLRandomForest") stop("The object class is not FLRandomForest")
	x<-predict(object,type="prob")
	tablename<-x@select@table_name
	# tabler<-as.data.frame(tablex)
	# ret<-list()
	# if(!all(tabler$PredictedClass) %in% c("0","1")){
	# 	i<-unique(tabler$PredictedClass)
	# 	c<-combn(i,m=2)
	# 	for(t in 1:ncol(c)){
	# 		resv<-c[,t]
	# 		subdf1<-tabler[tabler$PredictedClass==resv[1],]
	# 		subdf2<-tabler[tabler$PredictedClass==resv[2],]
	# 		probv1<-subdf1[,5]
	# 		probv2<-subdf2[,5]
	# 		eval(parse(text=paste0("ret$roc",resv[1],resv[2],"<-roc(as.FLVector(c(rep(0,length(probv1)),rep(1,length(probv2)))),
	# 						as.FLVector(c(probv1,probv2)))")))

	# 	}
	# }
	predclass<-sqlQuery(getFLConnection(),paste0("select distinct(PredictedClass) from ",tablename))
	comb<-combn(nrow(predclass),m=2)
	if(!all(predclass) %in% c("0","1")){
		for (t in 1:ncol(comb)) {
			resv<-comb[,t]
			temptable<-genRandVarName()
			sqlstr<-paste0("Select ObsID as ObsID, 1 as Response, probability as Predictor from ",
							tablename," Where PredictedClass = ",fquote(resv[1]))		
			vres<-createTable(pTableName=temptable,
	                  	      pSelect=sqlstr,
	                  	      pTemporary=TRUE,
	                 	      pDrop=TRUE)
			sqlstr2<-paste0("Select ObsID as ObsID, 1 as Response, probability as Predictor from ",
							tablename," Where PredictedClass = ",fquote(resv[2]))
			insertIntotbl(pTableName=temptable,
						  pSelect=sqlstr2)
			flt<-FLTable(temptable,"ObsID")
			eval(parse(text= paste0("ret$roc",resv[1],resv[2],
									"<-roc(flt,formula = Response~probability)")))
		}
	}
	else {
		tablex<-FLTable(tablename,"ObsID")
		ret$roc<-roc(tablex$PredictedClass,tablex$probability)
	}
	ret$confusion=object$confusion
	return(ret)
}
