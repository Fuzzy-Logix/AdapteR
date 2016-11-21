## boost decision trees
boosting<-function(data,
				   formula,
				   control=c(minsplit=10,
							 maxdepth=5,
							 cp=0.95),
				   mfinal=10){ #browser()
	call<-match.call()
	x<-FLrpart(data,formula,control,mfinal=mfinal)
	vfuncName<-"FLBoostDecisionTree"
	retobj<-sqlStoredProc(getFLConnection(),
						  vfuncName,
						  outputParameter=c(AnalysisID="a"),
						  pInputParameters=x)
	AnalysisID<-as.character(retobj[1,1])
	sql<-paste0("SELECT * FROM fzzlBoostDecisionTree AS a 
					WHERE AnalysisID = ",fquote(AnalysisID)," ORDER BY 2,4")
	ret<-sqlQuery(getFLConnection(),sql)
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
					  TreeID=ret$Iter,
					  Weight=ret$Weight)
	return(list(call,
				formula,
				trees))
}