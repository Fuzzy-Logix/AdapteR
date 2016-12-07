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
							   cp=0.95,...){ browser()
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
						  pInputParameters=x)
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
	votestable<-sqlQuery(getFLConnection(),votesquery)
	conmatrixtbl<-sqlQuery(getFLConnection(),conmatrixquery)
	
	retobj<-list(call=match.call(),
				 type="classification",
				 votes=data.frame(ObsID=votestable$ObsID,
				 				  ObservedClass=votestable$ObservedClass,
				 				  PredictedClass=votestable$PredictedClass,
				 				  Votes=votestable$NumOfVotes)
				 )
	return(retobj)
}