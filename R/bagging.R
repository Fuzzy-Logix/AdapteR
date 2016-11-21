## bag decision tree
bagging<-function(data,
				  formula,
				  control=c(minsplit=10,
							maxdepth=5,
							cp=0.95),
				  mfinal=5){ #browser()
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
		subframe<-subset(frame,TreeID==l)
		newframe<-preorderDataFrame(subframe)
		term <- rep(" ", nrow(newframe))
		newsplit<-c(1:nrow(newframe))
	 	newsplit[1]<-"root"	
		for(i in 1:nrow(newframe)){
    	if(newframe$Leaf[i]==1) {
    		term[i]<-"*" 
    	}
    	else{
    		j<-newframe$leftson[i]
    		k<-newframe$rightson[i]
    		newsplit[newframe$NodeID==j]<-paste0(newframe$var[i],"<",newframe$SplitVal[i])
    		newsplit[newframe$NodeID==k]<-paste0(newframe$var[i],">=",subframe$SplitVal[i])
    		}
 		}
 		newsplit[1]<-"root"
		node <- as.numeric(row.names(newframe))
	 	depth <- newframe$treelevel
	 	spaces<-2
	 	indent <- paste(rep(" ", spaces * 32L), collapse = "")
	  	indent <- if (length(node) > 1L) {
    		indent <- substring(indent, 1L, spaces * seq(depth))
    		paste0(c("", indent[depth]), format(node), ")")}
	    yval<-newframe$yval
		prob<-newframe$prob
	 	n <-newframe$n
    	retobj <- paste(indent,newsplit, n, yval, prob, term)
		trees[[l]]<-paste0("n=", n[1L], "\n\n",
					"node), split, n, yval, (yprob)\n",
					"      * denotes terminal node\n\n",
					retobj, sep = "\n")

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
	return(retobj)
}
