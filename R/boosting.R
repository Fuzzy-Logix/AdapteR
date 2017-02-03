#' Applies the AdaBoost.M1 and SAMME algorithms to a data set
#'
#' Fits the AdaBoost.M1 (Freund and Schapire, 1996) and SAMME (Zhu et al., 2009)
#' algorithms using classification trees as single classifiers.
#'
#' @param data FLTable
#' @param formula formula specifying the independent and dependent variable columns
#' @param mfinal an integer, the number of iterations
#' for which boosting is run or the number of trees to use.
#' Defaults to mfinal=10 iterations.
#' @param control options that control details of the rpart algorithm. See rpart for more details.
#' 
#' @return An object of class "FLBoosting" which has details about the trees generated
#' and the number of votes in bag corresponding to every ObsID.
#'
#' @examples
#' flt<-FLTable("tblBoostDT","ObsID","VarID","Num_Val")
#' flobj<-boosting(flt, formula = -1~.,mfinal=mfinal)
#' @export

boosting<-function(formula,data,...){
	UseMethod("boosting",data)
}

boosting.default<-function (formula,data=list(),...) {
    if (!requireNamespace("adabag", quietly = TRUE)){
        stop("adabag package needed for boosting. Please install it.",
             call. = FALSE)
    }
    else return(adabag::boosting(formula=formula,data=data,...))
}

boosting.FLTable<-function(data,
				   formula,
				   control=c(minsplit=10,
							 maxdepth=5,
							 cp=0.95),
				   mfinal=10){ #browser()
	call<-match.call()
	x<-rpart.FLTable(data,formula,control,mfinal=mfinal)
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
	weights<-unique(frame$Weight)
	ntrees<-unique(frame$TreeID)
	trees<-list()
	for(l in 1:length(ntrees)){
		trees[[l]]<-subset(frame,TreeID==l)
		class(trees[[l]])<-"FLrpart"	
	}
	x<-sqlQuery(getFLConnection(),paste0("SELECT * FROM fzzlBoostDecisionTreePred WHERE AnalysisID = ",
											 fquote(AnalysisID), "ORDER BY 1, 2, 3"))
	class<-x$PredictedClass
	prob<-data.frame(ObsID=x$ObsID,
					 ObservedClass=x$ObservedClass,
					 PredictedClass=x$PredictedClass,
					 PredictClassProb=x$PredictClassProb)
	votes<-x$PredictClassProb*sum(weights)
	votes<-data.frame(ObsID=x$ObsID,
					  PredictedClass=x$PredictedClass,
					  Votes=votes)
	retobj<-list(trees=trees,
				 call=call,
				 formula=formula,
				 votes=votes,
				 class=class,
				 weights=weights,
				 prob=prob)
	class(retobj)<-"FLboosting"
	return(retobj)
}
