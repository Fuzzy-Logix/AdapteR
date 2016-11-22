## bag decision tree
bagging<-function(formula,data,...){
	UseMethod("bagging",data)
}

bagging.default<-adabag::bagging

bagging.FLTable<-function(data,
				  formula,
				  control=c(minsplit=10,
							maxdepth=5,
							cp=0.95),
				  mfinal=5){#browser()
	x<-FLrpart(data,formula,control,mfinal=mfinal)
	vfuncName<-"FLBagDecisionTree"
	retobj<-sqlStoredProc(getFLConnection(),
						  vfuncName,
						  outputParameter=c(AnalysisID="a"),
						  pInputParameters=x)
	AnalysisID<-as.character(retobj[1,1])
	sql<-paste0("SELECT * FROM fzzlDecisionTreeMNMD AS a 
					WHERE AnalysisID = ",fquote(AnalysisID)," ORDER BY 2,4")
	ret<-sqlQuery(getFLConnection(),sql)
	call<-match.call()	
	frame<-data.frame(NodeID=ret$NodeID,
					  n=ret$NodeSize,
					  prob=ret$PredictClassProb,
					  yval=ret$PredictClass,
					  var=ret$SplitVarID,
					  SplitVal=ret$SplitVal,
					  leftson=ret$ChildNodeLeft,
					  rightson=ret$ChildNodeRight,
					  treelevel=ret$TreeLevel,
					  parent=ret$ParentNodeID,
					  Leaf=ret$IsLeaf,
					  TreeID=ret$DatasetID)
	ntrees<-unique(frame$TreeID)
	trees<-list()
	for(l in 1:length(ntrees)){
		trees[[l]]<-subset(frame,TreeID==l)
		class(trees[[l]])<-"FLrpart"	
	}
	#browser()
	votes<-sqlQuery(getFLConnection(),paste0("SELECT ObsID, ObservedClass, PredictedClass, NumOfVotes
											 FROM fzzlBagDTPred WHERE AnalysisID = ",fquote(AnalysisID), 
											"ORDER BY 1, 2, 3, 4"))
	class<-votes$PredictedClass
	retobj<-list(trees=trees,
				 call=call,
				 formula=formula,
				 votes=votes,
				 class=class)
	class(retobj)<-"FLbagging"
	return(retobj)
}

# print.FLbagging<-function(object){
# 	for(i in 1:length(object$trees)){
# 		cat(object$trees[[i]])
# 	}	
# }

# setMethod("show","FLbagging",print.FLbagging)
