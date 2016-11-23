# setClass(
# 	"FLrpart",
# 	slots=list(frame="list",
# 			   method="character",
# 			   control="list",
# 			   where="integer",
# 			   call="call",
# 			   AnalysisID="character",
# 			   deeptable="FLTable",
# 			   prepspecs="list"))

FLrpart<-function(data,
				  formula,
				  control=rpart.control(minsplit=10,
                                        maxdepth=5,
                                        cp=0.95),
				  method="class",
				  ...){#browser()
	call<-match.call()
	if(!class(formula)=="formula") stop("Please enter a valid formula")
	if(control["cp"]>1 || control["cp"]<0) stop("cp should be between 0 and 1")
	if(!class(formula)=="formula") stop("Please enter a valid formula")
	if(data@isDeep){
		deepx<-data
		deepx<-setAlias(deepx,"")	
		deeptablename<-data@select@table_name
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
			deep<-FLRegrDataPrep(data,depCol=depCol,excludeCols=vexclude)
			deepx<-deep[["table"]]
			deepx<- setAlias(deepx,"")
			deeptablename<-deepx@select@table_name
	}
	vobsid <- getVariables(deepx)[["obs_id_colname"]]
	vvarid <- getVariables(deepx)[["var_id_colname"]]
	vvalue <- getVariables(deepx)[["cell_val_colname"]]
	vnote<-genNote("abc")
	vcolnames<- deepx@select@variables
	vinputcols<-c(INPUT_TABLE=deeptablename,
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
	ret<-sqlQuery(connection,sql)
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
					  parent=ret$ParentNodeID)
	 
	retobj<- list(frame=frame,
			 	  method=method,
			 	  control=control,
			 	  where=ret$NodeID,
				  call=call,
				  deeptable=deepx,
				  AnalysisID=AnalysisID,
				  prepspecs=vprepspecs)
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
						  scoreTable="",
						  ...){#browser()
	if(!is.FLTable(newdata)) stop("Only allowed for FLTable")
	newdata <- setAlias(newdata,"")
	if(scoreTable=="")
	scoreTable<-gen_score_table_name(object$deeptable@select@table_name)

	if(!newdata@isDeep){
		deepx<-FLRegrDataPrep(newdata,
							  depCol=object$prepspecs$depCol,
							  excludeCols=object$prepspecs$vexclude)
		newdata<-deepx[["table"]]
		newdata<-setAlias(newdata,"")
	}
	vtable <- newdata@select@table_name
	vobsid <- getVariables(newdata)[["obs_id_colname"]]
	vvarid <- getVariables(newdata)[["var_id_colname"]]
	vvalue <- getVariables(newdata)[["cell_val_colname"]]

	vinputcols <- c(INPUT_TABLE=newdata@select@table_name,
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
						 	  pInputParameters=vinputcols)
	AnalysisID <- checkSqlQueryOutput(AnalysisID)
	query<-paste0("Select * from ",scoreTable," Order by 1")
	result<-sqlQuery(getFLConnection(),query)
	return(result)
}

print.FLrpart<-function(object){#browser()
	frame <- object$frame
	depth<-frame$treelevel
	newframe<-frame
	newframe[1,]<-frame[1,]
	newsplit<-c(1:length(depth))
	frame$split<-newsplit
	term <- rep(" ", length(depth))
    for(i in 1:length(depth)){
    	if(is.na(frame$SplitVal[i])) {
    		term[i]<-"*"
    	}
    	else{
    		j<-frame$leftson[i]
    		k<-frame$rightson[i]
    		newsplit[frame$NodeID==j]<-paste0(frame$var[i],"<",frame$SplitVal[i])
    		newsplit[frame$NodeID==k]<-paste0(frame$var[i],">=",frame$SplitVal[i])
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
 	newframe<-preorderDataFrame(frame)
 	ylevel <- attr(x, "ylevels")
	node <- as.numeric(row.names(newframe))
 	depth <- newframe$treelevel
 	spaces<-2
 	indent <- paste(rep(" ", spaces * 32L), collapse = "")
  	indent <- if (length(node) > 1L) {
    	indent <- substring(indent, 1L, spaces * seq(depth))
    	paste0(c("", indent[depth]), format(node), ")")}
    yval<-newframe$yval
    prob<-newframe$prob
 	newframe[length(depth),]<-frame[length(depth),]
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
setMethod("show","FLrpart",print.FLrpart)

preorderDataFrame <- function(df){
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

plot.FLrpart<-function(x){ browser()
	newframe<-preorderDataFrame(x$frame)
	curnode<-c()
	pxcor<-c()
	pycor<-c()
	xcor<-c()
	ycor<-c()
	inc<-as.numeric("1.5")
	treelevel<-newframe$treelevel
	for(i in 1:length(treelevel)){
		if(treelevel[i]==0){
			xcor<-c(xcor,"1.75")
			ycor<-c(ycor,"1.00")
			curnode<-c(curnode,"1")
			pxcor<-c(pxcor,xcor)
			pycor<-c(pycor,ycor)
		}
		else{
			curnode<-c(curnode,newframe$NodeID[i])
			pnode<-newframe$parent[i]
			pxcor<-c(pxcor,as.numeric(xcor[as.numeric(curnode[pnode])]))
			pycor<-c(pycor,as.numeric(ycor[as.numeric(curnode[pnode])]))
			ycor<-c(ycor,as.numeric(pycor[i])-treelevel[i]/2)
			if(newframe$leftson[pnode]==i) xcor<-c(xcor,as.numeric(pxcor[i])-inc)
			else xcor<-c(xcor,as.numeric(pxcor[i])+inc)
		}
	}
	temp1<-range(xcor)
	temp2<-range(ycor)
 	plot(temp1, temp2, type = "n", axes = TRUE, xlab = "", ylab = "")
 	parent<-matrix(c(pxcor,pycor),ncol=2)
 	child<-matrix(c(xcor,ycor),ncol=2)
 	#if(newframe$)
 	lines(c(parent), c(child))
 	#invisible(list(x = xcor, y = ycor))
 }