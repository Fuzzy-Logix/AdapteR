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
#' flt <- FLTable(getTestTableName("tblBoostDT"),
#'              "ObsID","VarID","Num_Val")
#' flobj <- boosting(flt, formula = -1~.,mfinal=10)
#' pred <- predict(flobj, newdata= flt[1:200, 1:18])
#' pred$confusion
#' pred$class
#' @seealso \code{\link[adabag]{boosting}} for corresponding R function reference.
#' @export
boosting<-function(formula,data,...){
	UseMethod("boosting",data)
}

#' @export
boosting.default<-function (formula,data=list(),...) {
    if (!requireNamespace("adabag", quietly = TRUE)){
        stop("adabag package needed for boosting. Please install it.",
             call. = FALSE)
    }
    else return(adabag::boosting(formula=formula,data=data,...))
}

#' @export
boosting.FLTable<-function(data,
                           formula,
                           control=c(minsplit=10,
                                     maxdepth=5,
                                     cp=0.95),
                           mfinal=10){ #browser()
    call<-match.call()
    if(class(data) == "FLpreparedData")
        obj<-rpart.FLpreparedData(data,formula,control,mfinal=mfinal)
    else
        obj<-rpart.FLTable(data,formula,control,mfinal=mfinal)

    vfuncName<-"FLBoostDecisionTree"
    retobj<-sqlStoredProc(getFLConnection(),
                          vfuncName,
                          outputParameter=c(AnalysisID="a"),
                          pInputParameters=obj$vinputcols)
    AnalysisID<-as.character(retobj[1,1])
    sql<-paste0("SELECT * FROM fzzlBoostDecisionTree AS a 
					WHERE AnalysisID = ",fquote(AnalysisID),
                    " ORDER BY 2,4")
    ret<-sqlQuery(getFLConnection(),sql)

    colnames(ret) <- tolower(colnames(ret))
    frame<-data.frame(NodeID=ret$nodeid,
                      n=ret$nodesize,
                      prob=ret$predictclassprob,
                      yval=ret$predictclass,
                      var=ret$splitvarid,
                      SplitVal=ret$splitval,
                      leftson=ret$childnodeleft,
                      rightson=ret$childnoderight,
                      treelevel=ret$treelevel,
                      parent=ret$parentnodeid,
                      Leaf=ret$isleaf,
                      TreeID=ret$iter,
                      Weight=ret$weight)
    weights<-frame[frame$NodeID==1,"Weight"]
    ntrees<-unique(frame$TreeID)
    trees<-list()
    for(l in 1:length(ntrees)){
        trees[[l]]<-subset(frame,TreeID==l)
        class(trees[[l]])<-"data.frame"	
    }
    x<-sqlQuery(getFLConnection(),paste0("SELECT * FROM fzzlBoostDecisionTreePred WHERE AnalysisID = ",
                                         fquote(AnalysisID), "ORDER BY 1, 2, 3"))
    colnames(x) <- tolower(colnames(x))
    class<-x$predictedclass
    prob<-data.frame(ObsID=x$obsid,
                     ObservedClass=x$observedclass,
                     PredictedClass=x$predictedclass,
                     PredictClassProb=x$predictclassprob)
    votes<-x$predictclassprob*length(ntrees)
    votes<-data.frame(ObsID=x$obsid,
                      PredictedClass=x$predictedclass,
                      Votes=votes)
    
    retobj<-list(trees=trees,
                 call=call,
                 formula=formula,
                 votes=votes,
                 class=class,
                 weights=weights,
                 prob=prob,
                 RegrDataPrepSpecs=obj$vprepspecs,
                 data=obj$data,
                 AnalysisID=AnalysisID)
    class(retobj)<-"FLboosting"
    return(retobj)
}

#' @export
boosting.FLpreparedData <- boosting.FLTable


#' @export
predict.FLboosting<-function(object,
                          newdata=object$data,
                          scoreTable="",
                          ...){ #browser()
    if(!is.FLTable(newdata)) stop("Only allowed for FLTable")
    newdata <- setAlias(newdata,"")
    if(scoreTable=="")
	scoreTable<-gen_score_table_name(getTableNameSlot(object$data))

    if(!isDeep(newdata)){
        deepx<-FLRegrDataPrep(newdata,
                              depCol=object$prepspecs$depCol,
                              excludeCols=object$prepspecs$vexclude)
        newdata<-deepx
        newdata<-setAlias(newdata,"")
    }
    vtable <- getTableNameSlot(newdata)
    vobsid <- getVariables(newdata)[["obs_id_colname"]]
    vvarid <- getVariables(newdata)[["var_id_colname"]]
    vvalue <- getVariables(newdata)[["cell_val_colname"]]

    vinputcols <- c(TableName=getTableNameSlot(newdata),
                    ObsIDColName=vobsid,
                    VarIDColName=vvarid,
                    ValueColName=vvalue,
                    AnalysisID=object$AnalysisID,
                    ScoreTable=scoreTable,
                    Note=genNote("Score"))
    vfuncName<-"FLBoostDecisionTreeScore"
    AnalysisID<-sqlStoredProc(getFLConnection(),
                              vfuncName,
                              outputParameter=c(AnalysisID="a"),
                              pInputParams=vinputcols)
    AnalysisID <- checkSqlQueryOutput(AnalysisID)
    #query<-paste0("Select * from ",scoreTable," Order by 1")
  	x<-sqlQuery(getFLConnection(),paste0("select ObservedClass, PredictedClass from ",scoreTable))
    colnames(x) <- c("ObservedClass","PredictedClass")
    m<-matrix(nrow = length(unique(x$ObservedClass)), ncol=length(unique(x$ObservedClass)))
	rownames(m)<-1:length(unique(x$ObservedClass))
	colnames(m)<-1:length(unique(x$ObservedClass))
	m[is.na(m)]<-0
	for(i in 1:length(x$ObservedClass)){
	  		j<-x[i,1]
	  		k<-x[i,2]	
	 		m[j,k]<-m[j,k]+1
	}
   	return(list(formula=object$formula,
   		   pred=FLTable(scoreTable,"ObsID"),
   		   class=as.factor(x$PredictedClass),
   		   confusion=m))
}
