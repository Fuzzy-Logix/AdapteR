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
#' print(flobj)
#' pred <- predict(flobj, newdata= flt[1:100,])
#' pred
#' plot(flobj)
#' @seealso \code{\link[randomForest]{randomForest}} for corresponding R function reference.
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
	forestquery<-paste0("SELECT * FROM fzzlDecisionTreeMNMD WHERE AnalysisID = ",
                        fquote(AnalysisID),
						"ORDER BY 1, 2, 3, 4, 5")
	votestable<-sqlQuery(getFLConnection(),votesquery)
    colnames(votestable) <- tolower(colnames(votestable))
	conmatrixtbl<-sqlQuery(getFLConnection(),conmatrixquery)
    colnames(conmatrixtbl) <- tolower(colnames(conmatrixtbl))
	foresttable<-sqlQuery(getFLConnection(),forestquery)
    colnames(foresttable) <- tolower(colnames(foresttable))
	m<-matrix(nrow = max(conmatrixtbl$observedclass)-min(conmatrixtbl$observedclass)+1, 
            ncol=max(conmatrixtbl$predictedclass)-min(conmatrixtbl$predictedclass)+1)
	rownames(m)<-min(conmatrixtbl$observedclass):max(conmatrixtbl$observedclass)
	colnames(m)<-min(conmatrixtbl$predictedclass):max(conmatrixtbl$predictedclass)

	for(i in 1:length(conmatrixtbl$observedclass)){
  		j<-conmatrixtbl[i,2]
  		k<-conmatrixtbl[i,3]	
 		m[as.character(j),as.character(k)]<-conmatrixtbl[i,4]
	}
	m[is.na(m)]<-0
	predicted<-votestable$predictedclass

	frame<-data.frame(NodeID=foresttable$nodeid,
					  n=foresttable$nodesize,
					  prob=foresttable$predictclassprob,
					  yval=foresttable$predictclass,
					  var=foresttable$splitvarid,
					  SplitVal=foresttable$splitval,
					  leftson=foresttable$childnodeleft,
					  rightson=foresttable$childnoderight,
					  treelevel=foresttable$treelevel,
					  parent=foresttable$parentnodeid,
					  Leaf=foresttable$isleaf,
					  TreeID=foresttable$datasetid)
	ntrees<-unique(frame$TreeID)
	trees<-list()
	for(l in 1:length(ntrees)){
		trees[[l]]<-subset(frame,TreeID==l)
		class(trees[[l]])<-"FLrpart"	
	}

    retobj<-list(call=match.call(),
                 type="classification",
                 votes=data.frame(ObsID=votestable$obsid,
                                  ObservedClass=votestable$observedclass,
                                  PredictedClass=votestable$predictedclass,
                                  Votes=votestable$numofvotes),
                 predicted=as.factor(structure(predicted,names=votestable$obsid)),
                 confusion=m,
                 classes=unique(conmatrixtbl$observedclass),
                 ntree=ntree,
                 mtry=mtry,
                 forest=trees,
                 data=x$data,
                 AnalysisID=AnalysisID,
                 RegrDataPrepSpecs=x$vprepspecs,
                 deeptable=x$deeptable)
    class(retobj)<-"FLRandomForest"
    return(retobj)
}

#' @export
predict.FLRandomForest<-function(object,newdata=object$deeptable,
								 scoreTable="",
                                 type="response",...){
    
	if(!is.FLTable(newdata)) stop("scoring allowed on FLTable only")
	newdata <- setAlias(newdata,"")
	if(scoreTable=="")
	scoreTable <- gen_score_table_name("rforest")
	vRegrDataPrepSpecs <- setDefaultsRegrDataPrepSpecs(x=object$RegrDataPrepSpecs,
                                                            values=list(...))

	if(!isDeep(newdata)){
		newdata <- FLRegrDataPrep(newdata,depCol=vRegrDataPrepSpecs$depCol,
								ExcludeCols=vRegrDataPrepSpecs$excludeCols)
	}
	
	newdata <- setAlias(newdata,"")
	tablename<- getTableNameSlot(newdata)
	vobsid <- getVariables(newdata)[["obs_id_colname"]]
	vvarid <- getVariables(newdata)[["var_id_colname"]]
	vvalue <- getVariables(newdata)[["cell_val_colname"]]

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
	vval<-"PredictedClass"
	vUniquePreClasses <- sqlQuery(connection,
								paste0("SELECT DISTINCT predictedclass FROM ",scoreTable," ORDER BY predictedclass")
								)[[1]]

	vFLMatrixFLag <- FALSE
	if(type %in% "prob"){
		vsqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n",
						 			vobsid," AS rowIdColumn, \n ",
						 			"DENSE_RANK() OVER(ORDER BY predictedclass) AS colIdColumn, \n ",
						 			"NumOfVotes * 1.0 /",object$ntree, " AS valueColumn \n ",
						 " FROM ",scoreTable)
		vFLMatrixFLag <- TRUE
	    # sqlSendUpdate(getFLConnection(), paste0("alter table ",scoreTable,
	    # 								   " add probability float, add matrix_id float"))
	    # sqlSendUpdate(getFLConnection(), paste0("update ",scoreTable,
	    # 		" set matrix_id = 1, probability = NumOfVotes * 1.0 /",object$ntree))
	 #    warning("The probability values are only true for predicted class. The sum may not be 1.")
	 #    tblfunqueryobj <- new("FLTableFunctionQuery",
	 #                          connectionName = attr(connection,"name"),
	 #                          variables=list(
	 #                              Matrix_ID="MATRIX_ID",
	 #                              rowIdColumn="rowIdColumn",
	 #                              colIdColumn="colIdColumn",
	 #                              valueColumn="valueColumn"),
	 #                          whereconditions="",
	 #                          order = "",
	 #                          SQLquery=vsqlstr)
		# flm <- newFLMatrix(
	 #                   select= tblfunqueryobj,
	 #                   dims=c(nrow(newdata),length(vUniquePreClasses)),
	 #                   Dimnames=list(NULL,vUniquePreClasses),
	 #                   dimColumns=c("Matrix_ID","rowIdColumn","colIdColumn","valueColumn"),
	 #                   type="double")
		# return(flm)
	}
	else if(type %in% "votes"){
		vsqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n",
						 			vobsid," AS rowIdColumn, \n ",
						 			"DENSE_RANK() OVER(ORDER BY predictedclass) AS colIdColumn, \n ",
						 			"NumOfVotes AS valueColumn \n ",
						 " FROM ",scoreTable)
		vFLMatrixFLag <- TRUE
	    # sqlSendUpdate(getFLConnection(), paste0("alter table ",scoreTable,
	    # 								   " add probability float, add matrix_id float"))
	    # sqlSendUpdate(getFLConnection(), paste0("update ",scoreTable,
	    # 		" set matrix_id = 1, probability = NumOfVotes * 1.0 /",object$ntree))
	 #    warning("The probability values are only true for predicted class. The sum may not be 1.")
	 #    tblfunqueryobj <- new("FLTableFunctionQuery",
	 #                          connectionName = attr(connection,"name"),
	 #                          variables=list(
	 #                              Matrix_ID="MATRIX_ID",
	 #                              rowIdColumn="rowIdColumn",
	 #                              colIdColumn="colIdColumn",
	 #                              valueColumn="valueColumn"),
	 #                          whereconditions="",
	 #                          order = "",
	 #                          SQLquery=vsqlstr)
		# flm <- newFLMatrix(
	 #                   select= tblfunqueryobj,
	 #                   dims=c(nrow(newdata),length(vUniquePreClasses)),
	 #                   Dimnames=list(NULL,vUniquePreClasses),
	 #                   dimColumns=c("Matrix_ID","rowIdColumn","colIdColumn","valueColumn"),
	 #                   type="double")
		# return(flm)
		# sqlSendUpdate(getFLConnection(),paste0("alter table ",scoreTable," add matrix_id int DEFAULT 1 NOT NULL"))
		# return(FLMatrix(scoreTable,1,"matrix_id",vobsid,"PredictedClass","NumOfVotes"))
	}
	else if(type %in% "link"){
		vsqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n",
						 			vobsid," AS rowIdColumn, \n ",
						 			"DENSE_RANK() OVER(ORDER BY predictedclass) AS colIdColumn, \n ",
						 			" -log((1/(NumOfVotes * 1.0 /",object$ntree,")) - 1) AS valueColumn \n ",
						 " FROM ",scoreTable)
		vFLMatrixFLag <- TRUE

		# sqlSendUpdate(getFLConnection(), paste0("alter table ",scoreTable,
	 #    								   " add probability float, add logit float, add matrix_id int DEFAULT 1 NOT NULL"))
	 #    sqlSendUpdate(getFLConnection(), paste0("update ",scoreTable," set probability = NumOfVotes * 1.0 /",object$ntree))
	 #    sqlSendUpdate(getFLConnection(), paste0("update ",scoreTable," set logit = -log((1/probability) - 1) where probability<1"))
	 #   	return(FLMatrix(scoreTable,1,"matrix_id",vobsid,"PredictedClass","logit"))
	}

	if(vFLMatrixFLag){
		warning("The probability values are only true for predicted class. The sum may not be 1.")
	    tblfunqueryobj <- new("FLTableFunctionQuery",
	                          connectionName = attr(connection,"name"),
	                          variables=list(
	                              Matrix_ID="MATRIX_ID",
	                              rowIdColumn="rowIdColumn",
	                              colIdColumn="colIdColumn",
	                              valueColumn="valueColumn"),
	                          whereconditions="",
	                          order = "",
	                          SQLquery=vsqlstr)
		flm <- newFLMatrix(
	                   select= tblfunqueryobj,
	                   dims=c(nrow(newdata),length(vUniquePreClasses)),
	                   Dimnames=list(NULL,vUniquePreClasses),
	                   dimColumns=c("Matrix_ID","rowIdColumn","colIdColumn","valueColumn"),
	                   type="double")
		return(flm)
	}
	sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
					"ObsID"," AS vectorIndexColumn,",
 					vval," AS vectorValueColumn",
	 				" FROM ",scoreTable)

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
   	# sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n
   	#                           ",vobsid," AS vectorIndexColumn,\n
    # 	                         ",val,"*",x," AS vectorValueColumn\n",
    #     	            " FROM ",scoreTable,"")
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
		class(object$forest[[i]])<-"data.frame"
		plot.FLrpart(object$forest[[i]])
	}
}	

#' @export
summary.FLRandomForest<-function(object){
	if(!class(object)=="FLRandomForest") stop("The object class is not FLRandomForest")
	x<-predict(object,type="prob")
	# tablename<-x@select@table_name
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
	# predclass<-sqlQuery(getFLConnection(),paste0("select distinct(PredictedClass) from ",
 #                        tablename))
	predclass <- 1:ncol(x)
	vactualPredclass <- sort(colnames(x))
	# comb<-combn(nrow(predclass),m=2)
	if(length(predclass)<2) stop("The distinct predicted class for the dataset are less than 2. 
						 Hence can't calculate Roc curves. \n  .... Summary not supported ... ")
	
	comb<-combn(predclass,m=2)
	
	retobj<-list()
	# if(!all(predclass) %in% c("0","1")){
	if(TRUE){
		for (t in 1:ncol(comb)) {
			resv<-comb[,t]
			temptable<-genRandVarName()
			sqlstr<-paste0("Select a.rowIdColumn as obsid, DENSE_RANK() OVER(ORDER BY a.colIdColumn)-1 as response, a.valueColumn as predictor \n ",
							" FROM (",constructSelect(x),") a \n ",
							" Where a.colIdColumn IN( ",fquote(resv[1]),",",fquote(resv[2]),")")		
			vres<-createTable(pTableName=temptable,
	                  	      pSelect=sqlstr,
	                  	      pTemporary=TRUE,
                              pPrimaryKey="obsid")
			# sqlstr2<-paste0("Select ObsID as obsid, 1 as response, probability as predictor from ",
			# 				tablename," Where PredictedClass = ",fquote(resv[2]))
			# insertIntotbl(pTableName=temptable,
			# 			  pSelect=sqlstr2)
			flt<-FLTable(temptable,"obsid",dims=c(nrow(x),3),dimnames=list(rownames(x),c("obsid","response","predictor")))
			eval(parse(text= paste0("retobj$roc",vactualPredclass[resv[1]],vactualPredclass[resv[2]],
									"<-roc(flt,formula = response~predictor)")))
		}
	}
	# else {

	# 	tablex<-FLTable(tablename,"obsid")
	# 	retobj$roc<-roc(tablex$PredictedClass,tablex$probability)
	# }
	retobj$confusion=object$confusion
	return(retobj)
}
