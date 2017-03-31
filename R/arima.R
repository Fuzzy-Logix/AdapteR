
#' @export
arima.FLVector<-function(object,
						 order=c(1,0,0),...){ #browser()
	if(!is.FLVector(object)) stop("The class of the input object should be FLVector")
	if(any(order)<0) stop("The p, d and q values should be positive integers")

	t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
                           pSelect = list(a = c(GroupID = 1,
                                                Num_Val = "a.vectorValueColumn",
                                                p=order[1],
                                                d=order[2],
                                                q=order[3])))
	temp1 <- createTable(pTableName=gen_unique_table_name("arima"),pSelect=t)
	pSelect<-paste0("Select * from ",temp1)
	query<-constructUDTSQL(pViewColnames=c(pGroupID="GroupID",
						   				   pNum_Val="Num_Val",
						   				   p="p",
						   				   d="d",
						   				   q="q"),
						   pSelect=pSelect,
						   pOutColnames=c("a.*"),
						   pFuncName="FLARIMAUdt",
						   pLocalOrderBy=c("pGroupID"))
	temp2 <- createTable(pTableName=gen_unique_table_name("temp"),pSelect=query)
	ret <- sqlQuery(getFLConnection(),paste0("Select * from ",temp2))
	return(ret)
}