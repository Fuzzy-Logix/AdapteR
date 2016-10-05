#' @include FLMatrix.R
NULL

#' @export
"FLMatrixArithmetic" <- function(pObj1,pObj2,pOperator)
{
    UseMethod("FLMatrixArithmetic", pObj1)
}

#' @export
FLMatrixArithmetic.default <- function(pObj1,pObj2,pOperator)
{
	if(pOperator=="**") pOperator <- "^"
	op <- .Primitive(pOperator)
	if(missing(pObj2))
        return(op(pObj1))
	op(pObj1,pObj2)
}

#' @export
FLMatrixArithmetic.FLMatrix <- function(pObj1,pObj2,pOperator)
{
    vtype <- getArtihmeticType(pObj1,pObj2,pOperator)
	vcompvector <- c("==",">","<",">=","<=","!=")
	if(missing(pObj2)){
		if(pOperator=="+") return(pObj1)
		else if(pOperator=="-") return(-1 * pObj1)
		else stop("two arguments needed for ",pOperator," \n ")
	}

	connection <- getFLConnection(pObj1)
	if(is.FLMatrix(pObj2))
	{
            ## flag1Check(connection)
		if(pOperator %in% c("+","-","%/%","%%","/","*","**",vcompvector))
            checkSameDims(pObj1,pObj2)
		else if(pOperator %in% c("%*%"))
            if(ncol(pObj1) != nrow(pObj2))
                stop("non-conformable dimensions")

		a <- genRandVarName()
		b <- genRandVarName()
		dimnames <- dimnames(pObj1)
        dims <- dim(pObj1)

		if(pOperator %in% c("*","**"))
            sqlstr <-   paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
                               a,".",pObj1@dimColumns[[2]]," AS rowIdColumn,",
                               a,".",pObj1@dimColumns[[3]]," AS colIdColumn,",
                               a,".",pObj1@dimColumns[[4]]," ",
                               pOperator," ",
                               b,".",pObj2@dimColumns[[4]]," AS valueColumn 
	            		    FROM ( ",constructSelect(pObj1),") AS ",a,
                            ",( ",constructSelect(pObj2),") AS ",b,
	            			constructWhere(c(paste0(a,".", pObj1@dimColumns[[2]]," = ", b,".",pObj2@dimColumns[[2]],""),
                                             paste0( a,".",pObj1@dimColumns[[3]]," = ", b,".",pObj2@dimColumns[[3]]," "),
                                             ifelse(pOperator=="**","",paste0(b,".",pObj2@dimColumns[[4]],"<>0")))))
        else if(pOperator %in% c("%%"))
            sqlstr <-   paste0(" SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                               a,".",pObj1@dimColumns[[2]]," AS rowIdColumn, \n ",
                               a,".",pObj1@dimColumns[[3]]," AS colIdColumn, \n ",
                               getMODSQL(pConnection=getFLConnection(pObj1),
                                        pColumn1=paste0(a,".",pObj1@dimColumns[[4]]),
                                        pColumn2=paste0(b,".",pObj2@dimColumns[[4]])),
                               " AS valueColumn \n ",
                            " FROM ( ",constructSelect(pObj1),") AS ",a,
                            ",( ",constructSelect(pObj2),") AS ",b,
                            constructWhere(c(paste0(a,".", pObj1@dimColumns[[2]]," = ", b,".",pObj2@dimColumns[[2]],""),
                                             paste0( a,".",pObj1@dimColumns[[3]]," = ", b,".",pObj2@dimColumns[[3]]," "),
                                             paste0(b,".",pObj2@dimColumns[[4]],"<>0"))))
		else if(pOperator %in% c("/"))
            sqlstr <-   paste0(" SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                               a,".",pObj1@dimColumns[[2]]," AS rowIdColumn, \n ",
                               a,".",pObj1@dimColumns[[3]]," AS colIdColumn, \n ",
                               "CAST(",a,".",pObj1@dimColumns[[4]]," AS FLOAT) ",pOperator," ",
                               b,".",pObj2@dimColumns[[4]]," AS valueColumn \n ",
                               " FROM ( ",constructSelect(pObj1),") AS ",a,
                               ",( ",constructSelect(pObj2),") AS ",b,
                               constructWhere(c(paste0(a,".", pObj1@dimColumns[[2]]," = ", b,".",pObj2@dimColumns[[2]],""),
                                                paste0( a,".",pObj1@dimColumns[[3]]," = ", b,".",pObj2@dimColumns[[3]]," "),
                                                ifelse(pOperator=="**","",paste0(b,".",pObj2@dimColumns[[4]],"<>0")))))

		else if(pOperator %in% c("%/%"))
            sqlstr <-   paste0(" SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                               a,".",pObj1@dimColumns[[2]]," AS rowIdColumn, \n ",
                               a,".",pObj1@dimColumns[[3]],"  AS colIdColumn, \n ",
                               "CASE WHEN ((",a,".",pObj1@dimColumns[[4]],"/",b,".",pObj2@dimColumns[[4]],")<0) ",
                               " THEN FLTrunc( ",a,".",pObj1@dimColumns[[4]]," / ",
                               b,".",pObj2@dimColumns[[4]],",0) - 1 ",
                               " ELSE FLTrunc( ",a,".",pObj1@dimColumns[[4]]," / ",
                               b,".",pObj2@dimColumns[[4]],",0) END AS valueColumn \n ",
                               " FROM ( ",constructSelect(pObj1),") AS ",a,
                               ",( ",constructSelect(pObj2),") AS ",b,
                               constructWhere(c(paste0(a,".",pObj1@dimColumns[[2]]," = ",b,".",pObj2@dimColumns[[2]],""),
                                                paste0(a,".",pObj1@dimColumns[[3]],"  = ", b,".",pObj2@dimColumns[[3]]," "),
                                                paste0(b,".",pObj2@dimColumns[[4]],"<>0"))))

		else if(pOperator %in% c("%*%"))
		{
			sqlstr <-paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
                            a,".",pObj1@dimColumns[[2]]," AS rowIdColumn,",
                            b,".",pObj2@dimColumns[[3]]," AS colIdColumn,
									 FLSumProd(",a,".",pObj1@dimColumns[[4]],",",b,".",pObj2@dimColumns[[4]],") AS valueColumn  
									 FROM (",constructSelect(pObj1),") AS ",a,
                            ",(",constructSelect(pObj2),") AS ",b,
	                        constructWhere(paste0( a,".",pObj1@dimColumns[[3]],"  = ",b,".",pObj2@dimColumns[[2]],"")),
	                        " GROUP BY 1,2,3")
			dimnames <- list(dimnames(pObj1)[[1]],
                             dimnames(pObj2)[[2]])
            dims <- c(dim(pObj1)[[1]],
                      dim(pObj2)[[2]])
		}

		else if(pOperator %in% c("+","-"))
		{
			sqlstr <- paste0(" SELECT\n",
                             "    '%insertIDhere%' AS MATRIX_ID,\n",
                             "    ",a,".rowIdColumn AS rowIdColumn,\n",
                             "    ",a,".colIdColumn AS colIdColumn,\n",
                             "    "," FLSum(",a,".valueColumn) AS valueColumn \n",
                             " FROM (\n",
                             "       SELECT \n",
                             "               a.",pObj1@dimColumns[[2]]," AS rowIdColumn,\n",
                             "               a.",pObj1@dimColumns[[3]]," AS colIdColumn,\n",
                             "               a.",pObj1@dimColumns[[4]]," AS valueColumn\n",
                             "       FROM(",constructSelect(pObj1),") AS a \n",
                             "       UNION ALL \n",
                             "       SELECT \n",
                             "               b.",pObj2@dimColumns[[2]]," AS rowIdColumn,\n",
                             "               b.",pObj2@dimColumns[[3]]," AS colIdColumn,\n",
                             "               b.",pObj2@dimColumns[[4]],"*(",pOperator,"1) AS valueColumn\n",
                             "       FROM(",constructSelect(pObj2),") AS b\n",
                             "       ) AS ",a,"\n",
							 " GROUP BY ",a,".rowIdColumn,", a,".colIdColumn ")
		}

		else if(pOperator %in% vcompvector){
            stop("this needs review")
			sqlstr <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                             "a.rowIdColumn AS rowIdColumn, \n ",
                             "a.colIdColumn AS colIdColumn, \n ",
                             " CASE WHEN FLSum(a.valueColumn) ",
                             ifelse(pOperator=="==","=",pOperator)," 0 ",
                             " THEN 'TRUE' ELSE 'FALSE' END AS valueColumn \n ",
                             " FROM(",constructSelect(pObj1,joinNames=FALSE),
                             " \n UNION ALL \n ",
                             " SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                             " b.rowIdColumn AS rowIdColumn, \n ",
                             " b.colIdColumn AS colIdColumn, \n ",
                             " b.valueColumn*(-1) AS valueColumn \n ",
                             " FROM(",constructSelect(pObj2),") AS b) AS a \n ",
							 " GROUP BY 1,2,3 ")
		}

		tblfunqueryobj <- new("FLTableFunctionQuery",
                              connectionName = attr(connection,"name"),
                              variables=list(
                                  rowIdColumn="rowIdColumn",
                                  colIdColumn="colIdColumn",
                                  valueColumn="valueColumn"),
                              whereconditions="",
                              order = "",
                              SQLquery=sqlstr)
		flm <- newFLMatrix(
                   select= tblfunqueryobj,
                   dims=dims,
                   Dimnames=dimnames,
                   dimColumns=c("rowIdColumn","colIdColumn","valueColumn"),
                   type=vtype)

		return(ensureQuerySize(pResult=flm,
                               pInput=list(pObj1,pObj2),
                               pOperator=pOperator))
	}
	else if(is.vector(pObj2))
    {
        if(pOperator %in% c("+","-","%/%","%%","/","*","**",vcompvector))
			pObj2 <- as.FLMatrix(matrix(pObj2,nrow(pObj1),ncol(pObj1)))
        else if(pOperator %in% c("%*%"))
        {
            if(length(pObj2)==ncol(pObj1))
				pObj2 <- as.FLMatrix(matrix(pObj2))
            else if(ncol(pObj1)==1)
				pObj2 <- as.FLMatrix(matrix(pObj2,1))
            else
				stop("non-conformable dimensions")
        }

        return(do.call(pOperator,list(pObj1,pObj2)))
    }
	else if(is.matrix(pObj2)||class(pObj2)=="dgCMatrix"||class(pObj2)=="dgeMatrix"
			||class(pObj2)=="dsCMatrix"||class(pObj2)=="dgTMatrix")
    {
        pObj2 <- as.FLMatrix(pObj2)
        return(do.call(pOperator,list(pObj1,pObj2)))
    }
	else if(is.FLVector(pObj2))
    {
                                        #browser()
        if(pOperator %in% c("+","-","%/%","%%","/","*","**",vcompvector))
			pObj2 <- as.FLMatrix(pObj2,
                                 sparse=TRUE,rows=nrow(pObj1),cols=ncol(pObj1))
        else if(pOperator %in% c("%*%"))
        {
            if(length(pObj2) == ncol(pObj1))
				pObj2 <- as.FLMatrix(pObj2)
            else if(ncol(pObj1)==1)
				pObj2 <- as.FLMatrix(pObj2,rows=1,cols=length(pObj2))
            else
				stop("non-conformable dimensions")
        }

        return(do.call(pOperator,list(pObj1,pObj2)))
    }
	else if(is.FLTable(pObj2))
	{
		if(!pObj2@isDeep)
            pObj2 <- wideToDeep(pObj2)[["table"]]
		pObj2 <- as.FLMatrix(pObj2)
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else stop("Operation Currently Not Supported")
}

#' @export
FLMatrixArithmetic.FLVector <- function(pObj1,pObj2,pOperator)
{
    vtype <- getArtihmeticType(pObj1,pObj2,pOperator)
	vcompvector <- c("==",">","<",">=","<=","<>")
	if(missing(pObj2)){
		if(pOperator=="+") return(pObj1)
		else if(pOperator=="-") return(-1 * pObj1)
		else stop("two arguments needed for ",pOperator," \n ")
	}

	connection <- getFLConnection(pObj1)
	if(is.FLMatrix(pObj2))
	{
		if(pOperator %in% c("%*%"))
            if(length(pObj1) == nrow(pObj2))
                pObj1 <- as.FLMatrix(pObj1,rows=1,cols=length(pObj1))
            else if(nrow(pObj2)==1)
                pObj1 <- as.FLMatrix(pObj1)
			else
                stop(" non-conformable dimensions ")
		else if(pOperator %in% c("+","-","%/%","%%","/","*","**",vcompvector))
            pObj1 <- as.FLMatrix(pObj1,
                                 sparse=TRUE,rows=nrow(pObj2),cols=ncol(pObj2))
		
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else if(is.vector(pObj2))
	{
		if(pOperator %in% c("%*%"))
            if(length(pObj1) != length(pObj2))
                stop("non-conformable dimensions")
            else
                pObj2 <- as.FLMatrix(matrix(pObj2))
		else if(pOperator %in% c("+","-","%/%","%%","/","*","**",vcompvector))
            pObj2 <- as.FLVector(pObj2)

		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else if(is.matrix(pObj2)||class(pObj2)=="dgCMatrix"
		    ||class(pObj2)=="dgeMatrix"||class(pObj2)=="dsCMatrix"
		    ||class(pObj2)=="dgTMatrix")
	{
		pObj2 <- as.FLMatrix(pObj2)
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else if(is.FLVector(pObj2))
	{
            ## flag3Check(connection)

		if(pOperator %in% c("%*%"))
		{
			if(length(pObj2) != length(pObj1)) stop(" non-conformable dimensions ")
			pObj1 <- as.FLMatrix(pObj1,rows=1,cols=length(pObj1))
			pObj2 <- as.FLMatrix(pObj2)
			return(pObj1 %*% pObj2)
		}
		else if(pOperator %in% c("+","-","%/%","%%","/","*","**",vcompvector))
		{
            if(checkQueryLimits(pObj1))
                pObj1 <- store(pObj1)
            if(checkQueryLimits(pObj2))
                pObj2 <- store(pObj2)


            if(ncol(pObj1)>1 && !pObj1@isDeep 
               && ncol(pObj2)>1 && !pObj2@isDeep)
            {
                vmaxlen <- max(length(pObj1),length(pObj2))
                newColnames1 <- renameDuplicates(colnames(pObj1))
                newColnames2 <- renameDuplicates(colnames(pObj2))
                if(pOperator %in% c("%/%"))
                    sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                     1:vmaxlen," AS vectorIndexColumn, \n ",
                                     "CASE WHEN (a.",newColnames1,
                                     "/ b.",newColnames2,") < 0 \n ",
                                     "THEN FLTrunc( a.",newColnames1,
                                     "/ b.",newColnames2,",0) - 1 \n ",
                                     "ELSE FLTrunc( a.",newColnames1,
                                     "/ b.",newColnames2,",0) \n ",
                                     " END AS vectorValueColumn \n ",
                                     " FROM (",constructSelect(pObj1),") AS a, \n ",
                                     "(",constructSelect(pObj2),") AS b \n ",
                                     collapse=" UNION ALL ")

                else if(pOperator %in% c("+","-","*","**"))
                    sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                     1:vmaxlen," AS vectorIndexColumn, \n ",
                                     "a.",newColnames1,
                                     " ",pOperator," ",
                                     "b.",newColnames2," AS vectorValueColumn \n ",
                                     " FROM (",constructSelect(pObj1),") AS a, \n ", 
                                     " (",constructSelect(pObj2),") AS b \n ",
                                     collapse=" UNION ALL ")
                else if(pOperator %in% c("%%"))
                    sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                     1:vmaxlen," AS vectorIndexColumn, \n ",
                                     getMODSQL(pConnection=getFLConnection(pObj1),
                                        pColumn1=paste0("a.",newColnames1),
                                        pColumn2=paste0("b.",newColnames2)),
                                     " AS vectorValueColumn \n ",
                                     " FROM (",constructSelect(pObj1),") AS a, \n ", 
                                     " (",constructSelect(pObj2),") AS b \n ",
                                     collapse=" UNION ALL ")
                else if(pOperator %in% c("/"))
                    sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                     1:vmaxlen," AS vectorIndexColumn, \n ",
                                     "CAST(a.",newColnames1," AS FLOAT)",pOperator,
                                     "b.",newColnames2," AS vectorValueColumn \n ",
                                     " FROM (",constructSelect(pObj1),") AS a, \n ", 
                                     " (",constructSelect(pObj2),") AS b \n ",
                                     collapse=" UNION ALL ")

                else if(pOperator %in% vcompvector)
                    sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                     1:vmaxlen," AS vectorIndexColumn, \n ",
                                     " CASE \n ",
                                     " WHEN (a.",newColnames1," - b.",newColnames2, ") ",
                                     ifelse(pOperator=="==","=",pOperator),
                                     " 0 THEN 'TRUE' ELSE 'FALSE' \n ",
                                     " END AS vectorValueColumn \n ",
                                     " FROM (",constructSelect(pObj1),") AS a, \n ",
                                     "(",constructSelect(pObj2),") AS b \n ",
                                     collapse=" UNION ALL ")

                dimnames <- list(1:vmaxlen,
                                 "vectorValueColumn")
            }
            else{
                if(ncol(pObj1)>1 && !pObj1@isDeep)
                    pObj1 <- store(pObj1)
                if(ncol(pObj2)>1 && !pObj2@isDeep)
                    pObj2 <- store(pObj2)

                ifelse(length(pObj1)>length(pObj2),{
                    vmaxlen <- length(pObj1);
                    vminlen <- length(pObj2);
                    vmaxref <- "a";
                    ifelse(pObj1@isDeep && length(colnames(pObj1))>1,
                           vmaxrownames <- colnames(pObj1),
                           vmaxrownames <- rownames(pObj1))
                },{
	                vmaxlen <- length(pObj2);
	                vmaxref <- "b";
	                vminlen <- length(pObj1);
	                ifelse(pObj2@isDeep && length(colnames(pObj2))>1,
                           vmaxrownames <- colnames(pObj2),
                           vmaxrownames <- rownames(pObj2))
                })

                if((pObj1@isDeep && pObj2@isDeep) 
                   ||(pObj1@isDeep && ncol(pObj2)==1)
                   ||(pObj2@isDeep && ncol(pObj1)==1)
                   ||(ncol(pObj1)==1 && ncol(pObj2)==1)){

                    if(pOperator %in% c("%/%"))
                        
                        sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                         vmaxref,".vectorIndexColumn AS vectorIndexColumn, \n ",
                                         "CASE WHEN ((a.vectorValueColumn*b.vectorValueColumn)<0) \n ",
                                         "THEN FLTrunc(a.vectorValueColumn / b.vectorValueColumn,0) -1 \n ",
                                         "ELSE FLTrunc(a.vectorValueColumn / b.vectorValueColumn,0) \n ",
                                         "END AS vectorValueColumn \n ",
                                         " FROM (",constructSelect(pObj1),") AS a, \n ",
                                         "(",constructSelect(pObj2),") AS b \n ",
                                         ## " WHERE CAST(FLMOD(a.vectorIndexColumn,",
                                         " WHERE CAST((",
                                            getMODSQL(pConnection=getFLConnection(pObj1),
                                                    pColumn1="a.vectorIndexColumn",
                                                    pColumn2=vminlen),") AS INT) = ",
                                         ## "CAST(FLMOD(b.vectorIndexColumn,",
                                         " CAST((",
                                            getMODSQL(pConnection=getFLConnection(pObj1),
                                                    pColumn1="b.vectorIndexColumn",
                                                    pColumn2=vminlen),
                                         ") AS INT) ")

                    else if(pOperator %in% c("+","-","*","**"))
                        
                        sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                         vmaxref,".vectorIndexColumn AS vectorIndexColumn, \n ",
                                         "a.vectorValueColumn ",
                                         pOperator,
                                         " b.vectorValueColumn AS vectorValueColumn \n ",
                                         " FROM (",constructSelect(pObj1),") AS a, \n ",
                                         "(",constructSelect(pObj2),") AS b \n ",
                                         ##" WHERE CAST(FLMOD(a.vectorIndexColumn,",
                                         " WHERE CAST((",
                                            getMODSQL(pConnection=getFLConnection(pObj1),
                                                    pColumn1="a.vectorIndexColumn",
                                                    pColumn2=vminlen),") AS INT) = ",
                                         ## "CAST(FLMOD(b.vectorIndexColumn,",
                                         " CAST((",
                                            getMODSQL(pConnection=getFLConnection(pObj1),
                                                    pColumn1="b.vectorIndexColumn",
                                                    pColumn2=vminlen),
                                         ") AS INT) ")
                    
                    else if(pOperator %in% c("%%"))
                        
                        sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                         vmaxref,".vectorIndexColumn AS vectorIndexColumn, \n ",
                                         getMODSQL(pConnection=getFLConnection(pObj1),
                                                    pColumn1="a.vectorValueColumn",
                                                    pColumn2="b.vectorValueColumn"),
                                         " AS vectorValueColumn \n ",
                                         " FROM (",constructSelect(pObj1),") AS a, \n ",
                                         "(",constructSelect(pObj2),") AS b \n ",
                                         ##" WHERE CAST(FLMOD(a.vectorIndexColumn,",
                                         " WHERE CAST((",
                                            getMODSQL(pConnection=getFLConnection(pObj1),
                                                    pColumn1="a.vectorIndexColumn",
                                                    pColumn2=vminlen),") AS INT) = ",
                                         ## "CAST(FLMOD(b.vectorIndexColumn,",
                                         " CAST((",
                                            getMODSQL(pConnection=getFLConnection(pObj1),
                                                    pColumn1="b.vectorIndexColumn",
                                                    pColumn2=vminlen),
                                         ") AS INT) ")

                    else if(pOperator %in% c("/"))
                        
                        sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                         vmaxref,".vectorIndexColumn AS vectorIndexColumn, \n ",
                                         "CAST(a.vectorValueColumn AS FLOAT)",pOperator,
                                         "b.vectorValueColumn AS vectorValueColumn \n ",
                                         " FROM (",constructSelect(pObj1),") AS a, \n ",
                                         "(",constructSelect(pObj2),") AS b \n ",
                                         ## " WHERE CAST(FLMOD(a.vectorIndexColumn,",
                                         " WHERE CAST((",
                                            getMODSQL(pConnection=getFLConnection(pObj1),
                                                    pColumn1="a.vectorIndexColumn",
                                                    pColumn2=vminlen),") AS INT) = ",
                                         ## "CAST(FLMOD(b.vectorIndexColumn,",
                                         " CAST((",
                                            getMODSQL(pConnection=getFLConnection(pObj1),
                                                    pColumn1="b.vectorIndexColumn",
                                                    pColumn2=vminlen),
                                         ") AS INT) ")

                    else if(pOperator %in% vcompvector)
                        sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                         vmaxref,".vectorIndexColumn AS vectorIndexColumn \n ,",
                                         "CASE \n ",
                                         " WHEN (a.vectorValueColumn - b.vectorValueColumn) ",
                                         ifelse(pOperator=="==","=",pOperator)," 0 \n ",
                                         " THEN 'TRUE' ELSE 'FALSE' END AS vectorValueColumn \n ",
                                         " FROM (",constructSelect(pObj1),") AS a, \n ",
                                         "(",constructSelect(pObj2),") AS b \n ",
                                         ## constructWhere(c(paste0(" FLMOD(a.vectorIndexColumn,",vminlen,
                                         ## ") = FLMOD(b.vectorIndexColumn,",vminlen,")"))))
                                         constructWhere(c(paste0(" (",
                                                        getMODSQL(pConnection=getFLConnection(pObj1),
                                                                pColumn1="a.vectorIndexColumn",
                                                                pColumn2=vminlen),
                                                            ") = (",
                                                        getMODSQL(pConnection=getFLConnection(pObj1),
                                                                pColumn1="b.vectorIndexColumn",
                                                                pColumn2=vminlen),
                                                            ") "))))

                    dimnames <- list(vmaxrownames,"vectorValueColumn")
                }
            }

			tblfunqueryobj <- new("FLTableFunctionQuery",
                                  connectionName = attr(connection,"name"),
                                  variables = list(
                                      obs_id_colname = "vectorIndexColumn",
                                      cell_val_colname = "vectorValueColumn"),
                                  whereconditions="",
                                  order = "",
                                  SQLquery=sqlstr)

			flv <- newFLVector(
                       select = tblfunqueryobj,
                       Dimnames = dimnames,
                       isDeep = FALSE,
                       type=vtype)

			return(ensureQuerySize(pResult=flv,
                                   pInput=list(pObj1,pObj2),
                                   pOperator=pOperator))
		}
	}
	else if(is.FLTable(pObj2))
	{
		if(!pObj2@isDeep)
            pObj2 <- wideToDeep(pObj2)[["table"]]
		pObj2 <- as.FLMatrix(pObj2)
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else cat("ERROR::Operation Currently Not Supported")
}

#' @export
FLMatrixArithmetic.matrix <- function(pObj1,pObj2,pOperator)
{
	vcompvector <- c("==",">","<",">=","<=","!=")
	if(missing(pObj2))
	{
		op <- .Primitive(pOperator)
		return(op(pObj1))
	}
	
	if(pOperator %in% c("+","-","%/%","%%","/","*","**",vcompvector))
        return(FLMatrixArithmetic.sparseMatrix(pObj1,pObj2,pOperator))
	else if(pOperator %in% c("%*%"))
	{
		if((is.FLMatrix(pObj2) && ncol(pObj1)!=nrow(pObj2))||
           (is.FLVector(pObj2) && !(length(pObj2)==ncol(pObj1) || ncol(pObj1)==1)))
            stop("non-conformable dimensions")
		else return(FLMatrixArithmetic.sparseMatrix(pObj1,pObj2,pOperator))
	}
}

#' @export
FLMatrixArithmetic.numeric <- function(pObj1,pObj2,pOperator)
{	
	vcompvector <- c("==",">","<",">=","<=","!=")
	if(missing(pObj2))
	{
		op <- .Primitive(pOperator)
		return(op(pObj1))
	}
	if(pOperator %in% c("%*%"))
	{
		if(is.FLMatrix(pObj2))
		{
			connection <- getFLConnection(pObj2)
			if(nrow(pObj2)==length(pObj1))
                pObj1 <- as.FLMatrix(matrix(pObj1,1))
			else if(nrow(pObj2)==1)
                pObj1 <- as.FLMatrix(matrix(pObj1))
			else
                stop("non-conformable dimensions")
			return(pObj1 %*% pObj2)
		}
		else if(class(pObj2)=="FLVector")
		{
			connection <- getFLConnection(pObj2)
			if(length(pObj2) != length(pObj1)) stop("non-conformable dimensions")
			pObj1 <- as.FLMatrix(matrix(pObj1,1))
			return(pObj1 %*% pObj2)
		}
		else
            return(FLMatrixArithmetic.default(pObj1,pObj2,pOperator))
	}
	else if(is.FLMatrix(pObj2) || is.FLVector(pObj2))
	{
		connection <- getFLConnection(pObj2)
		if(is.FLMatrix(pObj2))
            pObj1 <- as.FLMatrix(matrix(pObj1,nrow(pObj2),ncol(pObj2)))
		else 
            pObj1 <- as.FLVector(pObj1)
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else if(is.FLTable(pObj2))
	{
		if(!pObj2@isDeep)
            pObj2 <- wideToDeep(pObj2)[["table"]]
		pObj2 <- as.FLMatrix(pObj2)
		pObj1 <- as.FLMatrix(matrix(pObj1,nrow(pObj2),ncol(pObj2)))
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else
        return(FLMatrixArithmetic.default(pObj1,pObj2,pOperator))
}

#' @export
FLMatrixArithmetic.sparseMatrix <- function(pObj1,pObj2,pOperator)
{
	vcompvector <- c("==",">","<",">=","<=","!=")
	if(missing(pObj2)){
		if(pOperator=="+") return(pObj1)
		else if(pOperator=="-") return(-1 * pObj1)
		else stop("two arguments needed for ",pOperator," \n ")
	}
	if(is.FLMatrix(pObj2)||is.FLVector(pObj2))
	{
		pObj1 <- as.FLMatrix(pObj1)
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else if(is.FLTable(pObj2))
	{
		if(!pObj2@isDeep)
            pObj2 <- wideToDeep(pObj2)[["table"]]
		pObj2 <- as.FLMatrix(pObj2)
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else
        return(FLMatrixArithmetic.default(pObj1,pObj2,pOperator))
}

#' @export
FLMatrixArithmetic.FLTable <- function(pObj1,pObj2)
{
	if(!pObj1@isDeep)
        pObj1 <- wideToDeep(pObj1)[["table"]]
	pObj1 <- as.FLMatrix(pObj1)
	return(do.call(pOperator,list(pObj1,pObj2)))
}

#' @export
FLMatrixArithmetic.dgCMatrix <- FLMatrixArithmetic.sparseMatrix
#' @export
FLMatrixArithmetic.dgeMatrix <- FLMatrixArithmetic.sparseMatrix
#' @export
FLMatrixArithmetic.dgTMatrix <- FLMatrixArithmetic.sparseMatrix
#' @export
FLMatrixArithmetic.dsCMatrix <- FLMatrixArithmetic.sparseMatrix

NULL
#' Addition of in-database objects.
#'
#' \code{+} does the addition of in-database objects.
#'
#' The addition of in-database objects mimics the normal addition of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{+} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix + Rvector
#' @export

"+" <- function(pObj1,pObj2)
{
    UseMethod("+", pObj1)
}

#' @export
`+.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"+"))

#' @export
`+.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.FLMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"+"))

NULL
#' Subtraction of in-database objects.
#'
#' \code{-} does the subtraction of in-database objects.
#'
#' The subtraction of in-database objects mimics the normal subtraction of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{-} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 2,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix - Rvector
#' @export

"-" <- function(pObj1,pObj2)
{
    UseMethod("-", pObj1)
}

#' @export
`-.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"-"))

#' @export
`-.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.FLMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"-"))

NULL
#' Cross-Product of in-database objects.
#'
#' \code{\%*\%} does the Cross-Product of in-database objects.
#'
#' The Cross-Product of in-database objects mimics the \code{\%*\%} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%*\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %*% Rvector
#' @export

"%*%" <- function(pObj1,pObj2)
{
    UseMethod("%*%", pObj1)
}

#' @export
`%*%.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"%*%"))

#' @export
`%*%.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.numeric` <- function(pObj1,pObj2)	
    return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
crossProdFLMatrix <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.FLMatrixBind` <- crossProdFLMatrix

#' @export
`%*%.FLMatrix` <- crossProdFLMatrix

#' @export
`%*%.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

NULL
#' remainder of division on in-database objects.
#'
#' \code{\%\%} calculates the remainder of in-database object division.
#'
#' The remainder of in-database objects mimics the normal remainder of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @section Constraints: division by 0 gives inf in R, but is not supported for 
#' in-database objects
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %% Rvector
#' @export

"%%" <- function(pObj1,pObj2)
{
    UseMethod("%%", pObj1)
}

#' @export
`%%.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"%%"))

#' @export
`%%.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

NULL
#' Element-Wise Multiplication of in-database objects.
#'
#' \code{*} does the Element-wise Multiplication of in-database objects.
#'
#' The Element-wise Multiplication of in-database objects mimics the normal Element-wise Multiplication of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{*} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix * Rvector
#' @export

"*" <- function(pObj1,pObj2)
{
    UseMethod("*", pObj1)
}

#' @export
`*.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"*"))

#' @export
`*.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"*"))

NULL
#' Element-wise Division of in-database objects.
#'
#' \code{/} does the Element-wise Division of in-database objects.
#'
#' The Element-wise Division of in-database objects mimics the \code{/} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{/} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @section Constraints: division by 0 gives inf in R, but is not supported for 
#' in-database objects
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix / Rvector
#' @export

"/" <- function(pObj1,pObj2)
{
    UseMethod("/", pObj1)
}

#' @export
`/.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"/"))

#' @export
`/.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"/"))

NULL
#' Integer Division of in-database objects.
#'
#' \code{\%/\%} does the Element-wise Integer Division of in-database objects.
#'
#' The Element-wise Integer Division of in-database objects mimics the \code{\%/\%} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%/\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @section Constraints: division by 0 gives inf in R, but is not supported for 
#' in-database objects
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %/% Rvector
#' @export

"%/%" <- function(pObj1,pObj2)
{
    UseMethod("%/%", pObj1)
}

#' @export
`%/%.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"%/%"))

#' @export
`%/%.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.FLMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

NULL
#' Element-Wise power of in-database objects.
#'
#' \code{**} does the Element-wise power of in-database objects.
#'
#' 
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{**} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix ** Rvector
#' @export

"**" <- function(pObj1,pObj2)
{
    UseMethod("**", pObj1)
}

#' @export
`**.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"**"))

#' @export
`**.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`**.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`**.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`**.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`**.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`**.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`**.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`**.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

NULL
#' Element-Wise power of in-database objects.
#'
#' \code{^} does the Element-wise power of in-database objects.
#'
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{^} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix ** Rvector
#' @export

"^" <- function(pObj1,pObj2)
{
    UseMethod("^", pObj1)
}

#' @export
`^.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"**"))

#' @export
`^.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`^.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`^.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`^.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`^.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`^.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`^.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
`^.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"**"))

#' @export
">=" <- function(pObj1,pObj2)
{
    UseMethod(">=", pObj1)
}

#' @export
`>=.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,">="))

#' @export
`>=.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">="))

#' @export
`>=.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">="))

#' @export
`>=.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">="))

#' @export
`>=.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">="))

#' @export
`>=.FLTable` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">="))

#' @export
`>=.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">="))

#' @export
`>=.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">="))

#' @export
`>=.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">="))

#' @export
`>=.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">="))

#' @export
"<=" <- function(pObj1,pObj2)
{
    UseMethod("<=", pObj1)
}

#' @export
`<=.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"<="))

#' @export
`<=.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<="))

#' @export
`<=.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<="))

#' @export
`<=.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<="))

#' @export
`<=.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<="))

#' @export
`<=.FLTable` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<="))

#' @export
`<=.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<="))

#' @export
`<=.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<="))

#' @export
`<=.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<="))

#' @export
`<=.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<="))

#' @export
">" <- function(pObj1,pObj2)
{
    UseMethod(">", pObj1)
}

#' @export
`>.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,">"))

#' @export
`>.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">"))

#' @export
`>.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">"))

#' @export
`>.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">"))

#' @export
`>.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">"))

#' @export
`>.FLTable` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">"))

#' @export
`>.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">"))

#' @export
`>.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">"))

#' @export
`>.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">"))

#' @export
`>.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,">"))

#' @export
"<" <- function(pObj1,pObj2)
{
    UseMethod("<", pObj1)
}

#' @export
`<.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"<"))

#' @export
`<.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<"))

#' @export
`<.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<"))

#' @export
`<.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<"))

#' @export
`<.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<"))

#' @export
`<.FLTable` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<"))

#' @export
`<.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<"))

#' @export
`<.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<"))

#' @export
`<.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<"))

#' @export
`<.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"<"))

#' @export
"!=" <- function(pObj1,pObj2)
{
    UseMethod("!=", pObj1)
}

#' @export
`!=.default` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic.default(pObj1,pObj2,"!="))

#' @export
`!=.matrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"!="))

#' @export
`!=.numeric` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"!="))

#' @export
`!=.FLMatrix` <- function(pObj1, pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"!="))

#' @export
`!=.FLVector` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"!="))

#' @export
`!=.FLTable` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"!="))

#' @export
`!=.dgCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"!="))

#' @export
`!=.dgeMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"!="))

#' @export
`!=.dgTMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"!="))

#' @export
`!=.dsCMatrix` <- function(pObj1,pObj2)
    return(FLMatrixArithmetic(pObj1,pObj2,"!="))

## This is not working for FLMatrix,FLVector case.
## Refer FLIdentical for this implementation.
NULL
#' Equality of in-database objects.
#'
#' \code{==} checks the equality of in-database objects.
#'
#' The equality of in-database objects mimics the normal addition of R data types.
#' One can check equality of FLMatrices, FLMatrix - R matrices, FLVectors and
#' FLVector - RVector.
#' @param pObj1 can be an in-database object like FLMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{==} returns a logical TRUE or FALSE matrix similar to R output
#' @section Constraints:
#' Currently only \code{dgCMatrix},\code{dgeMatrix},\code{dsCMatrix},
#' \code{dgTMatrix},\code{matrix},\code{Matrix},\code{vector} R types
#' are supported. Comparision of FLMatrix with FLVector is not currently Supported.
#' In case of FLVector and Rvector comparision use FLVector==RVector in place of 
#' RVector==FLVector
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' flvector <- as.FLVector(1:5)
#' Result <- flmatrix == flmatrix
#' Result <- flvector==flvector
#' Result <- flvector==1:5
# #' @export
# "==" <- function(pObj1,pObj2)
# {
#     UseMethod("==", pObj1)
# }

# #' @export
# `==.default` <- function(pObj1,pObj2)
# return(FLMatrixArithmetic.default(pObj1,pObj2,"=="))

# #' @export
# `==.matrix` <- function(pObj1,pObj2)
# return(FLMatrixArithmetic(pObj1,pObj2,"=="))

# #' @export
# `==.numeric` <- function(pObj1,pObj2)
# return(FLMatrixArithmetic(pObj1,pObj2,"=="))

# #' @export
# `==.FLMatrix` <- function(pObj1, pObj2){
# 	browser()
# 	return(FLMatrixArithmetic(pObj1,pObj2,"=="))
# }

# #' @export
# `==.FLVector` <- function(pObj1,pObj2)
# return(FLMatrixArithmetic(pObj1,pObj2,"=="))

# #' @export
# `==.FLTable` <- function(pObj1,pObj2)
# return(FLMatrixArithmetic(pObj1,pObj2,"=="))

# #' @export
# `==.dgCMatrix` <- function(pObj1,pObj2)
# return(FLMatrixArithmetic(pObj1,pObj2,"=="))

# #' @export
# `==.dgeMatrix` <- function(pObj1,pObj2)
# return(FLMatrixArithmetic(pObj1,pObj2,"=="))

# #' @export
# `==.dgTMatrix` <- function(pObj1,pObj2)
# return(FLMatrixArithmetic(pObj1,pObj2,"=="))

# #' @export
# `==.dsCMatrix` <- function(pObj1,pObj2)
# return(FLMatrixArithmetic(pObj1,pObj2,"=="))
