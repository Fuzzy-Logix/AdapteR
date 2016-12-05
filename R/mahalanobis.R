mahalanobis<-function(x,y,S,...){ browser()
	#if(!class(x)||!class(y)=="numeric") stop("Function only applies to numeric vectors of x and y")
	#if(!class(S)=="matrix") stop("Function works for a covariance matrix")

	t <- constructUnionSQL(pFrom=c(a=constructSelect(x),
											  b=constructSelect(y),
											  c=constructSelect(S)),
                                              pSelect=list(a=c(pGroupID=1,pID=1,pRowID=1,pColID=1:length(x),pValue="a.vectorValueColumn"),
                                                           b=c(pGroupID=1,pID=2,pRowID=1,pColID=1:length(y),pValue="b.vectorValueColumn"),
                                                           c=c(pGroupID=1,pID=0,pRowID=1:nrow(S),pColID=1:ncol(S),pValue="c.vectorValueColumn")))
	tName <- gen_unique_table_name("mahalanobis")
    p <- createTable(tName,pSelect=t)
	return(sqlQuery(getFLConnection,paste0("Select * from ",noquote(p))))
}