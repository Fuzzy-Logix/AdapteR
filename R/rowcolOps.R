rowOps<-function(object,operation,...)
{
	connection<-getConnection(object)
	flag3Check(connection)
	var <- genRandVarName()

	if (operation=="Sum")
		opt<-"SUM"
	else if(operation=="Mean")
		opt<-"AVG"

	else stop("Please enter either \"Sum\" or \"Mean\"")

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".",object@dimColumns[[1]]," AS vectorIndexColumn",
			        ", ",opt,"(",var,".",object@dimColumns[[3]],") AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".",object@dimColumns[[1]])

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
				dimnames = list(1:nrow(object),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
	            pInput=list(object),
	            pOperator="rowOps"))
}

colOps<-function(object,operation,...)
{
	connection<-getConnection(object)
	flag3Check(connection)
	var <- genRandVarName()

	if (operation=="Sum")
		opt<-"SUM"
	else if(operation=="Mean")
		opt<-"AVG"

	else stop("Please enter either \"Sum\" or \"Mean\"")

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".",object@dimColumns[[2]]," AS vectorIndexColumn",
			        ", ",opt,"(",var,".",object@dimColumns[[3]],") AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".",object@dimColumns[[2]])

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
				dimnames = list(1:nrow(object),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
	            pInput=list(object),
	            pOperator="colOps"))
}