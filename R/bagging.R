#' Applies the Bagging algorithm to a dataset
#'
#' Fits the Bagging algorithm proposed by Breiman in
#' 1996 using classification trees as single classifiers.
#'
#' @param data FLTable
#' @param formula formula specifying the independent and dependent variable columns
#' @param mfinal an integer, the number of iterations
#' for which boosting is run or the number of trees to use.
#' Defaults to mfinal=5 iterations.
#' @param control options that control details of the rpart algorithm. See rpart for more details.
#' 
#' @return An object of class "FLBagging" which has details about the trees generated
#' and the number of votes in bag corresponding to every ObsID.
#'
#' @examples
#' flt<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
#' flobj<-bagging(flt, formula = -1~.,mfinal=mfinal)
#' @export
bagging<-function(formula,data,...){
	UseMethod("bagging",data)
}

#' @export
bagging.default  <- function (formula,data=list(),...) {
    if (!requireNamespace("adabag", quietly = TRUE)){
        stop("adabag package needed for bagging. Please install it.",
             call. = FALSE)
    }
    else return(adabag::bagging(formula=formula,data=data,...))
}


#' @export
bagging.FLTable<-function(data,
				  formula,
				  control=c(minsplit=10,
							maxdepth=5,
							cp=0.95),
                          mfinal=5){
    if(class(data) == "FLpreparedData")
        x<-rpart.FLpreparedData(data,formula,control,mfinal=mfinal)
    else
        x<-rpart.FLTable(data,formula,control,mfinal=mfinal)
                
    	vfuncName<-"FLBagDecisionTree"
	retobj<-sqlStoredProc(getFLConnection(),
						  vfuncName,
						  outputParameter=c(AnalysisID="a"),
						  pInputParameters=x$vinputcols)
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
		class(trees[[l]])<-"data.frame"	
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
				 class=class,
 				 RegrDataPrepSpecs=x$vprepspecs,
 				 data=x$data,
 				 AnalysisID=AnalysisID)
	class(retobj)<-"FLbagging"
	return(retobj)
}

#' @export
bagging.FLpreparedData <- bagging.FLTable


# print.FLbagging<-function(object){
# 	for(i in 1:length(object$trees)){
# 		cat(object$trees[[i]])
# 	}	
# }

# setMethod("show","FLbagging",print.FLbagging)

predict.FLbagging<-function(object,newdata=object$data,
								 scoreTable="",...){ #browser()
	if(!is.FLTable(newdata)) stop("scoring allowed on FLTable only")
	newdata <- setAlias(newdata,"")
	vinputTable <- getTableNameSlot(newdata)
	if(scoreTable=="")
	scoreTable <- gen_score_table_name("BaggingDTScore")
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
	vfuncName<-"FLBagDecisionTreeScore"
	AnalysisID <- sqlStoredProc(getFLConnection(),
								vfuncName,
								outputParameter=c(AnalysisID="a"),
								pInputParams=vinputcols)

    sqlQuery(getFLConnection(), paste0("alter table ",scoreTable,
    								   " add probability float, add matrix_id float"))
    sqlQuery(getFLConnection(), paste0("update ",scoreTable,
    		" set matrix_id = 1, probability = NumOfVotes * 1.0 /",length(object$trees)))											
    x<-sqlQuery(getFLConnection(),paste0("select ObservedClass, PredictedClass from ",scoreTable))
    m<-matrix(nrow = max(x$ObservedClass)-min(x$ObservedClass)+1, ncol=max(x$PredictedClass)-min(x$PredictedClass)+1)
	rownames(m)<-min(x$ObservedClass):max(x$ObservedClass)
	colnames(m)<-min(x$PredictedClass):max(x$PredictedClass)
	m[is.na(m)]<-0
	for(i in 1:length(x$ObservedClass)){
	  		j<-x[i,1]
	  		k<-x[i,2]	
	 		m[as.character(j),as.character(k)]<-m[as.character(j),as.character(k)]+1
	}
	warning("The probability values are only true for predicted class. The sum may not be 1.")
   	return(list(formula= object$formula,
   				votes = FLMatrix(scoreTable,1,"matrix_id",vobsid,"PredictedClass","NumOfVotes"),
   				prob = FLMatrix(scoreTable,1,"matrix_id",vobsid,"PredictedClass","probability"),
   				class=as.factor(x$PredictedClass),
   				confusion=m))
}
