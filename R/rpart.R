#' @export

rpart <- function (formula,data=list(),...) {
	UseMethod("rpart", data)
}
rpart.default<-rpart::rpart

rpart.FLTable<-function(data,
				  formula,
				  control=c(minsplit=10,
							maxdepth=5,
                            cp=0.95),
				  method="class",
				  ...){ browser()
	mfinal<-list(...)$mfinal
	call<-match.call()
	if(!class(formula)=="formula") stop("Please enter a valid formula")
	if(control["cp"]>1 || control["cp"]<0) stop("cp should be between 0 and 1")
	if(!class(formula)=="formula") stop("Please enter a valid formula")
	if(isDeep(data)){
		deepx<-data
		deepx<-setAlias(deepx,"")
		deeptablename<-getTableNameSlot(data)
		vprepspecs<-list()	
	}
	else{
		if(!isDotFormula(formula)){
			data <- setAlias(data,"")
			vcolnames<-colnames(data)		
			vexclude<-setdiff(vcolnames,all.vars(formula))
			obs<-getVariables(data)[["obs_id_colname"]]
			vexclude<-setdiff(vexclude,obs)
			vexclude<-paste0(vexclude,collapse=",")
		}
		else{
			vexclude<-NULL
		}
			depCol<-all.vars(formula)[1]
			vprepspecs<-list(depCol,vexclude)
			deep<-FLRegrDataPrep(data,depCol=depCol,ExcludeCols=vexclude)
			deepx<-deep
			deepx<- setAlias(deepx,"")
			deeptablename<-getTableNameSlot(deepx)
	}
	vobsid <- getVariables(deepx)[["obs_id_colname"]]
	vvarid <- getVariables(deepx)[["var_id_colname"]]
	vvalue <- getVariables(deepx)[["cell_val_colname"]]
	vnote<-genNote("abc")
	vcolnames<- deepx@select@variables
	if(!is.null(list(...)[["mfinal"]])){
		vinputcols<-list(INPUT_TABLE=deeptablename,
				  		OBSID=vobsid,
				  		VARID=vvarid,
				  		VALUE=vvalue,
				  		NUMOFTREES=mfinal,
				  		MINOBS=control["minsplit"],
				 	    MAXLEVEL=control["maxdepth"],
				  		PURITY=control["cp"],
				  		NOTE=vnote)
		return(vinputcols)
	}
	else if(!is.null(list(...)[["ntree"]])){
		vinputcols<-list(INPUT_TABLE=deeptablename,
				  		 OBSID=vobsid,
				  		 VARID=vvarid,
				  		 VALUE=vvalue,
				 		 NTREES=list(...)[["ntree"]],
				 		 NoOfVARS=list(...)[["mtry"]],
				 		 MINOBS=control["minsplit"],
				  		 MAXLEVEL=control["maxdepth"],
				 		 PURITY=control["cp"],
				 		 NOTE=vnote)
	return(list(vinputcols=vinputcols,
				data=deepx,
				vprepspecs=vprepspecs))
	}
	else vinputcols<-list(INPUT_TABLE=deeptablename,
				  		  OBSID=vobsid,
				  		  VARID=vvarid,
				  		  VALUE=vvalue,
				  		  MINOBS=control["minsplit"],
				  		  MAXLEVEL=control["maxdepth"],
						  PURITY=control["cp"],
				  		  NOTE=vnote)
	vfuncName<-"FLDecisionTreeMN"
	retobj<-sqlStoredProc(getFLConnection(),
						  vfuncName,
						  outputParameter=c(AnalysisID="a"),
						  pInputParameters=vinputcols)

	AnalysisID<-as.character(retobj[1,1])
	sql<-paste0("Select * from fzzlDecisionTreeMN where AnalysisID = ",fquote(AnalysisID)," Order by 1,2,3")
	ret<-sqlQuery(getFLConnection(),sql)
	frame<-data.frame(NodeID=ret$NodeID,
					  n=ret$NodeSize,
					  prob=ret$PredictClassProb,
					  yval=ret$PredictClass,
					  var=ret$SplitVarID,
					  SplitVal=ret$SplitVal,
					  leftson=ret$ChildNodeLeft,
					  rightson=ret$ChildNodeRight,
					  dev=1:length(ret$NodeSize),
					  treelevel=ret$TreeLevel,
					  parent=ret$ParentNodeID,
					  Leaf=ret$IsLeaf)

	frame$var[is.na(frame$var)]<-"<leaf>" 
	retobj<- list(frame=frame,
			 	  method=method,
			 	  control=control,
			 	  where=ret$NodeID,
				  call=call,
				  deeptable=deepx,
				  AnalysisID=AnalysisID,
				  prepspecs=vprepspecs,
				  data=deepx)
	class(retobj)<-"FLrpart"
	return(retobj)
}

## todo: implement $ operator for all names in a rpart S3 object

summary.FLrpart<-function(x,...){
    ## todo: create a rpart S3 object xrpart (populate as much as possible)
    ## print(xrpart)
	cat("Call:\n")
	dput(x$call)
	cat(" n=",x$frame$n[1],"\n\n")
	## should have cp table here which we do not get from DBLytix
	for(i in 1:length(x$frame$NodeID)){
		cat("\n\nNode number ", x$frame$NodeID[i], ": ", x$frame$n[i], " observations", sep = "")
		cat("\n predicted class=",x$frame$yval[i])
		cat("\n probability:",x$frame$prob[i])
		if(!is.null(x$frame$SplitVal[i])&&!is.na(x$frame$SplitVal[i])){
			cat("\n left son=",x$frame$leftson[i],"  right son=",x$frame$rightson[i])
			cat("\n Split:")
			cat("\n ",x$frame$var[i]," < ",x$frame$SplitVal[i])
		}
	}

}

predict.FLrpart<-function(object,
                          newdata=object$deeptable,
                          scoreTable="",type = "response",
                          ...){
    browser()
    if(!is.FLTable(newdata)) stop("Only allowed for FLTable")
    newdata <- setAlias(newdata,"")
    if(scoreTable=="")
	scoreTable<-gen_score_table_name(getTableNameSlot(object$deeptable))

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

    vinputcols <- c(INPUT_TABLE=getTableNameSlot(newdata),
                    OBSID_COL=vobsid,
                    VARID_COL=vvarid,
                    VALUE_COL=vvalue,
                    ANALYSISID=object$AnalysisID,
                    OUTPUT_TABLE=scoreTable,
                    NOTE=genNote("Score"))
    vfuncName<-"FLDecisionTreeMNScore"
    AnalysisID<-sqlStoredProc(getFLConnection(),
                              vfuncName,
                              outputParameter=c(AnalysisID="a"),
                              pInputParams=vinputcols)
    AnalysisID <- checkSqlQueryOutput(AnalysisID)
    ##query<-paste0("Select * from ",scoreTable," Order by 1")
    val <- "PredictedClass"
    if(type %in% "prob")
        val <- "PredictClassProb"
    
    sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n
                              ",vobsid," AS vectorIndexColumn,\n
                              ",val," AS vectorValueColumn\n",
                     " FROM ",scoreTable,"")
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
    
    
}

print.FLrpart<-function(object){ #browser()
	if(is.null(object$frame)) frame<-object
	else frame <- object$frame
	class(frame)<-"data.frame"
	depth<-frame$treelevel
	newsplit<-c(1:length(depth))
	term <- rep(" ", length(depth))
	newframe<-preorderDataFrame(frame)
    for(i in 1:length(depth)){
    	if(newframe$Leaf[i]==1) {
    		term[i]<-"*"
    	}
    	else{
    		j<-newframe$leftson[i]
    		k<-newframe$rightson[i]
    		newsplit[newframe$NodeID==j]<-paste0(newframe$var[i],"<",newframe$SplitVal[i])
    		newsplit[newframe$NodeID==k]<-paste0(newframe$var[i],">=",newframe$SplitVal[i])
    		}
 	}
 	newsplit[1]<-"root"
 	# t<-0
 	# for(i in 1:length(depth)){
 	# 	if(!is.na(frame$SplitVal[i])) {
 	# 		j<-frame$leftson[i]
  #   		k<-frame$rightson[i]
 	# 		newframe[i+1,]<-frame[j,]
  #   		newframe[i+2,]<-frame[k,]
 	# 	}
 	# 	else {
 	# 		if(newframe$NodeID!=i) newframe[length(depth-t),]<-frame[i]
 	# 		t<-t+1
 	# 	}
	 # }
 	#browser()
 	ylevel <- attr(x, "ylevels")
	node <- as.numeric(newframe$NodeID)
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
	#class(retobj)<-"rpart"
	cat("n=", n[1L], "\n\n")
	cat("node), split, n, yval, (yprob)\n")
	cat("      * denotes terminal node\n\n")
  	cat(retobj, sep = "\n")
}

# `$.FLrpart` <- function(object,property){
# 	return(slot(object,property))
# }
setOldClass("FLrpart")
setMethod("show","FLrpart",print.FLrpart)

preorderDataFrame <- function(df){#browser()
  ind <- c()
  stack <- c()
  curr <- getCurrent(df,1)
  while(1){
    ind <- c(ind,curr)
    left <- getLeft(df,curr)
    right <- getRight(df,curr)
    if(!is.na(right)){
      stack <- pushstack(stack,right)
    }
    if(!is.na(left))
      curr <- left
    else if(length(stack)>0){
      vlist <- popstack(stack)
      curr <- vlist[["value"]]
      stack <- vlist[["stack"]]
    }
    else break()
  }
  return(df[ind,])
}
getCurrent <- function(df,ind){
  return(df[df[,"NodeID"]==ind,1])
}
getLeft <- function(df,ind){
  return(df[df[,"NodeID"]==ind,"leftson"])
}
getRight <- function(df,ind){
  return(df[df[,"NodeID"]==ind,"rightson"])
}
pushstack <- function(stack,value){
  return(c(stack,value))
}
popstack <- function(stack){
  val <- stack[length(stack)]
  return(list(stack=stack[-length(stack)],value=val))
}

plot.FLrpart<-function(x){ #browser()
	# newframe<-preorderDataFrame(x$frame)
	# curnode<-c()
	# pxcor<-c()
	# pycor<-c()
	# xcor<-c()
	# ycor<-c()
	# inc<-as.numeric("1.5")
	# treelevel<-newframe$treelevel
	# for(i in 1:length(treelevel)){
	# 	if(treelevel[i]==0){
	# 		xcor<-c(xcor,"1.75")
	# 		ycor<-c(ycor,"1.00")
	# 		curnode<-c(curnode,"1")
	# 		pxcor<-c(pxcor,xcor)
	# 		pycor<-c(pycor,ycor)
	# 	}
	# 	else{
	# 		curnode<-c(curnode,newframe$NodeID[i])
	# 		pnode<-newframe$parent[i]
	# 		pxcor<-c(pxcor,as.numeric(xcor[curnode==pnode]))
	# 		pycor<-c(pycor,as.numeric(ycor[curnode==pnode]))
	# 		ycor<-c(ycor,as.numeric(pycor[i])-treelevel[i]/2)
	# 		if(newframe$leftson[as.numeric(newframe$NodeID)==pnode]==as.numeric(curnode[i]))
	# 			xcor<-c(xcor,as.numeric(pxcor[i])-0.25)
	# 		else xcor<-c(xcor,as.numeric(pxcor[i])+0.25)
	# 	}
	# }
	# #browser()
	# temp1<-range(xcor)
	# temp2<-range(ycor)
 # 	plot(temp1, temp2, type = "n", axes = TRUE, xlab = "", ylab = "")
 # 	parent<<-matrix(c(pxcor,pycor),ncol=2)
 # 	child<<-matrix(c(xcor,ycor),ncol=2)
 # 	#if(newframe$)
 # 	lines(parent, child)
 # 	#invisible(list(x = xcor, y = ycor))
 return(createcor(x$frame))
 }

 createcor<-function(frame){#browser()
  plot(2.5,2)
  xcor<-c("2.5")
  ycor<-c("2.5")
  for(i in 1:nrow(frame)){
    if(frame$Leaf[i]==0){
      j<-frame$leftson[i]
      k<-frame$rightson[i]
      if(i==1) {
        xcor[j]<-as.numeric(xcor[i])-0.5
        xcor[k]<-as.numeric(xcor[i])+0.5
        ycor[j]<-"2.25"
        ycor[k]<-"2.25"
        segments(as.numeric(xcor[1]),as.numeric(ycor[1]),mean(c(as.numeric(xcor[j]),as.numeric(xcor[k]))),mean(c(as.numeric(ycor[j]),as.numeric(ycor[k]))))
        segments(as.numeric(xcor[j]),as.numeric(ycor[j]),as.numeric(xcor[k]),as.numeric(ycor[k]))
        }
      else{
        xcor[j]<-as.numeric(xcor[i])-0.25/frame$treelevel[i]
        xcor[k]<-as.numeric(xcor[i])+0.25/frame$treelevel[i]
        ycor[j]<-2.5-as.numeric(frame$treelevel[j])*0.25
        ycor[k]<-2.5-as.numeric(frame$treelevel[k])*0.25
        pxcor<-as.numeric(xcor[i])
        pycor<-as.numeric(ycor[i])
        xmid<-as.numeric((as.numeric(xcor[j])+as.numeric(xcor[k]))/2)
        ymid<-as.numeric((as.numeric(ycor[j])+as.numeric(ycor[k]))/2)
        segments(pxcor,pycor,xmid,ymid)
        segments(as.numeric(xcor[j]),as.numeric(ycor[j]),as.numeric(xcor[k]),as.numeric(ycor[k]))
        }
  	}
  }
}
