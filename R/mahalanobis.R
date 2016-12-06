mahalanobis<-function(x,y,S,...){
	UseMethod("mahalanobis",x)
}

mahalanobis.default<-stats::mahalanobis

mahalanobis.FLVector<-function(x,y,S,...){ #browser()
	#if(!class(x)||!class(y)=="FLVector") stop("Function only applies to numeric FLVectors of x and y")
	#if(!class(S)=="FLMatrix") stop("Function works for a covariance matrix")

	t <- constructUnionSQL(pFrom=c(a=constructSelect(x),
								   b=constructSelect(y),
								   c=constructSelect(S)),
                           pSelect=list(a=c(pGroupID=1,
                           					pID=1,
                           					pRowID=1,
                           					pColID="a.vectorIndexColumn",
                           					pValue="a.vectorValueColumn"),
                                        b=c(pGroupID=1,
                                        	pID=2,
                                        	pRowID=1,
                                        	pColID="b.vectorIndexColumn",
                                        	pValue="b.vectorValueColumn"),
                                        c=c(pGroupID=1,
                                        	pID=0,
                                        	pRowID="c.rowIdColumn",
                                        	pColID="c.colIdColumn",
                                        	pValue="c.valueColumn")))

    p <- createTable(pTableName=gen_unique_table_name("temp"),pSelect=t,pTemporary=TRUE)
	query<-constructUDTSQL(pViewColnames=c(pGroupID="pGroupID",pID="pID",pRowID="pRowID",pColID="pColID",pValue="pValue"),
						   pSelect=paste0("Select * from ",p),
						   pOutColnames="a.*",
						   pFuncName="FLMahaDistUdt",
						   pLocalOrderBy=c("pGroupID","pID"))
	tName <- gen_unique_table_name("mahalanobis")
    p <- createTable(tName,pSelect=query,pTemporary=TRUE)
    a<-sqlQuery(getFLConnection(),paste0("Select * from ",p))
    val<-as.numeric(a[2])
	return(val*val)
}