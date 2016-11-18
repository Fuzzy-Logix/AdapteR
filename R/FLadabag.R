## bag decision tree
FLadabag<-function(data,
				   formula,
				   control=c(minsplit=10,
				   			 maxdepth=5,
				   			 cp=0.95),
				   mfinal=5){
	ret<-FLrpart(data,formula,control,mfinal=mfinal)
	call<-match.call()	
	frame<-data.frame(TreeID=ret$DatasetID,
					  NodeID=ret$NodeID,
					  n=ret$NodeSize,
					  prob=ret$PredictClassProb,
					  yval=ret$PredictClass,
					  var=ret$SplitVarID,
					  SplitVal=ret$SplitVal,
					  leftson=ret$ChildNodeLeft,
					  rightson=ret$ChildNodeRight,
					  treelevel=ret$TreeLevel,
					  parent=ret$ParentNodeID)
	retobj<-list(frame=frame,
				 call=call,
				 )
	return(retobj)
}

summary.FLadabag<-function(object){
	ntrees<-unique(object$TreeID)
	for(i in 1:length(ntrees)){
		paste0("frame",i)<-
	}
}