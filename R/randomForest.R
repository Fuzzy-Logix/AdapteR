#' @export
randomForest<-function(data,formula,...){
	UseMethod("randomForest",data)
}
randomForest.default<-randomForest::randomForest

randomForest.FLTable<-function(data,
							   formula,
							   ntree=25,
							   mtry=2,
							   nodesize=10,
							   maxdepth=5,
							   cp=0.95,...){ #browser()
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
		class(trees[[l]])<-"FLrpart"	
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

predict.FLRandomForest<-function(object,newdata=object$data,
								 scoreTable="",...){ browser()
	if(!is.FLTable(newdata)) stop("scoring allowed on FLTable only")
	newdata <- setAlias(newdata,"")
	vinputTable <- newdata@select@table_name
	if(scoreTable=="")
	scoreTable <- gen_score_table_name("RandomForestScore")
	vRegrDataPrepSpecs <- setDefaultsRegrDataPrepSpecs(x=object$RegrDataPrepSpecs,
                                                            values=list(...))
	deepx <- FLRegrDataPrep(newdata,depCol=vRegrDataPrepSpecs$depCol,
								ExcludeCols=vRegrDataPrepSpecs$excludeCols)
	newdatatable <- deepx[["table"]]
	newdatatable <- setAlias(newdatatable,"")
	tablename<- newdatatable@select@table_name
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
	query<-paste0("Select * from ",scoreTable," Order By 1")
	retobj<-sqlQuery(getFLConnection(),query)
}