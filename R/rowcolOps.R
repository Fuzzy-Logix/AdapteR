


rowcolOps<-function(object,margin,operation,...){

		connection<-getConnection(object)
		flag3Check(connection)
		var <- genRandVarName()

		if(!margin %in% c("1","2")) stop("Please enter 1 for row and 2 for column")

		if (operation=="Sum")
			{	if(margin==1) res<-nrow(object)
				else		  res<-ncol(object)
				div<-1
				opt<-"SUM" }
		else if(operation=="Mean")
				{  if(margin==1) {div<-ncol(object)
								  res<-nrow(object)}
					else 		 {div<-nrow(object)
								  res<-ncol(object)}
					opt<-"AVG"
				}		
	else stop("Please enter either \"Sum\" or \"Mean\"")
	
	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".",object@dimColumns[[margin]]," AS vectorIndexColumn",
			        ", (",opt,"(",var,".",object@dimColumns[[3]],"))/",div," AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".",object@dimColumns[[margin]])

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(1:res,
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
	            pInput=list(object),
	            pOperator="rowcolOps"))
}
