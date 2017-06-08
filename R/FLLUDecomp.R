#' @include FLMatrix.R
NULL

#' An S4 class to represent LU Decomposition
#' @slot x object of class FLVector
#' @slot perm object of class FLVector
#' @slot Dim object of class FLVector
#' @slot lower object of class FLMatrix
#' @slot upper object of class FLMatrix
#' @slot data_perm object of class FLMatrix
#' @export
setClass(
	"FLLU",
	slots=list(
		x="FLVector",
		perm="FLVector",
		Dim="vector",
		lower="FLMatrix",
		upper="FLMatrix",
		data_perm="FLMatrix",
        Dimnames="list"
	)
)



#' LU Decomposition.
#'
#' The LU decomposition involves factorizing a matrix as the product of a lower
#' triangular matrix L and an upper triangular matrix U. Permutation matrix is also provided in the output.
#' If permutation matrix is not used in the decomposition, the output of permutation matrix is an identity matrix.
#'
#' \code{lu} replicates the equivalent lu() generic function.\cr
#' \code{expand} decomposes the compact form to a list of matrix factors.\cr
#' The expand method returns L,U and P factors as a list of FLMatrices.\cr
#'
#' The decomposition is of the form A = P L U where typically all matrices are of size (n x n),
#' and the matrix P is a permutation matrix, L is lower triangular and U is upper triangular.
#' @method lu FLMatrix
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be with maximum dimension limitations
#' of (1000 x 1000).
#' @return
#' \item{x}{the FLVector form of "L" (unit lower triangular) and "U" (upper triangular) factors of the original matrix}
#' \item{perm}{FLVector that describes the permutation applied to the rows of the original matrix}
#' \item{Dim}{FLVector that gives the dimension of the original matrix}
#' \item{lower}{FLMatrix representing the lower triangular matrix}
#' \item{upper}{FLMatrix representing the upper triangular matrix}
#' \item{data_perm}{FLMatrix representing the permutation matrix}
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' FLLUobject <- lu(flmatrix)
#' listresult <- expand(FLLUobject)
#' listresult$L
#' listresult$U
#' listresult$P
#' @export
setGeneric("lu", function(object,...) {
    standardGeneric("lu")
})

setMethod("lu", signature(object = "matrix"),
          function(object,...)
             Matrix::lu(object,...))
setMethod("lu", signature(object = "dgeMatrix"),
          function(object,...)
             Matrix::lu(object,...))
setMethod("lu", signature(object = "dgCMatrix"),
          function(object,...)
             Matrix::lu(object,...))
setMethod("lu", signature(object = "FLMatrix"),
          function(object,...)
             lu.FLMatrix(object,...))
# #' @export
# lu<-function(object, ...){
# 	UseMethod("lu",object)
# }

#' @export
lu.default <- Matrix::lu

#' @export
lu.FLMatrix<-function(object,...)
{
	connection<-getFLConnection()
	## flag3Check(connection)
	## flag1Check(connection)
	
    MID1 <- getMaxMatrixId(connection)

    # sqlstr <- paste0(
    #                  viewSelectMatrix(object, "a","z"),
    #                  outputSelectMatrix("FLLUDecompUdt",viewName="z",localName="a",
    #                 	outColNames=list("OutputMatrixID","OutputRowNum",
    #                 		"OutputColNum","OutputValL","OutputValU","OutputPermut"),
    #                 	whereClause="")
    #                )
    
    sqlstr <- constructMatrixUDTSQL(pObject=object,
                                 pFuncName="FLLUDecompUdt",
                                 pdims=getDimsSlot(object),
                                 pdimnames=dimnames(object),
                                 pReturnQuery=TRUE
                                 )
    sqlstr <- gsub("'%insertIDhere%'",MID1,sqlstr)

    sqlstr <- ensureQuerySize(pResult=sqlstr,
	            pInput=list(object),
	            pOperator="lu")

    tempResultTable <- cacheDecompResults(pFuncName="FLLUDecompUdt",
                                          pQuery=sqlstr)

	# tempResultTable <- createTable(pTableName=gen_unique_table_name("LU"),
 #                                   pSelect=sqlstr)

	# calculating LU matrix

	sqlstrLU <-paste0(" SELECT ",MID1," AS MATRIX_ID, \n ",
					          "OutputRowNum AS rowIdColumn, \n ",
					          "OutputColNum AS colIdColumn, \n ",
					          "CAST(OutputValL AS FLOAT) AS valueColumn \n ",
					  	" FROM ",tempResultTable,
					 	" WHERE OutputRowNum > OutputColNum \n ",
				   		" AND OutputValL IS NOT NULL \n ",
				   		" UNION ALL \n ",
				   		" SELECT ",MID1," AS MATRIX_ID, \n ",
					          " OutputRowNum AS rowIdColumn, \n ",
					          " OutputColNum AS colIdColumn, \n ",
					          " CAST(OutputValU AS FLOAT) AS valueColumn \n ", 
					  	" FROM ",tempResultTable,
					 	" WHERE OutputRowNum <= OutputColNum \n ",
				   		" AND OutputValU IS NOT NULL ")

    ##@Phani: insert into select with union all not working in hadoop
    vtblName <- gen_unique_table_name("LU")
    vLUTable <- createTable(pTableName=vtblName,
                           pColNames=c("MATRIX_ID","rowIdColumn",
                                        "colIdColumn","valueColumn"),
                           pColTypes=c(rep("INT",3),rep("FLOAT",1)),
                           pPrimaryKey=c("MATRIX_ID","rowIdColumn",
                                        "colIdColumn")
                            )

    vLUTable <- insertIntotbl(pTableName=vtblName,
                            pSelect=sqlstrLU)
    
    vLUTable <- vtblName

    flm <- FLMatrix(
            connection = getFLConnection(object),
            table_name = vLUTable, 
            matrix_id_value = MID1,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "rowIdColumn", 
            col_id_colname = "colIdColumn", 
            cell_val_colname = "valueColumn",
            dims=dim(object),
            dimnames=dimnames(object),
            type=typeof(object)
            )
	# tblfunqueryobj <- new("FLTableFunctionQuery",
 #                        connectionName = attr(connection,"name"),
 #                        variables=list(
 #                            rowIdColumn="rowIdColumn",
 #                            colIdColumn="colIdColumn",
 #                            valueColumn="valueColumn"),
 #                        whereconditions="",
 #                        order = "",
 #                        SQLquery=sqlstrLU)

	# flm <- newFLMatrix(
 #               select= tblfunqueryobj,
 #               dims=dim(object),
	#             Dimnames=dimnames(object))

  	# LUMatrix <- store(object=flm)
    LUMatrix <- flm

	# calculating Permutation FLMatrix
    data_perm <- FLMatrix(connection = connection, 
			       table_name = tempResultTable, 
				   matrix_id_value = "",
				   matrix_id_colname = "", 
				   row_id_colname = "OutputRowNum", 
				   col_id_colname = "OutputColNum", 
				   cell_val_colname = "OutputPermut",
				   whereconditions=paste0("mtrx.OutputPermut IS NOT NULL "))


	# calculating l FLmatrix
    l<-FLMatrix(connection = connection, 
	       table_name = tempResultTable, 
		   matrix_id_value = "",
		   matrix_id_colname = "", 
		   row_id_colname = "OutputRowNum", 
		   col_id_colname = "OutputColNum", 
		   cell_val_colname = "OutputValL",
		   whereconditions=paste0("mtrx.OutputValL IS NOT NULL "))


	# calculating U FLmatrix
    u<-FLMatrix(connection = connection, 
	       table_name = tempResultTable, 
		   matrix_id_value = "",
		   matrix_id_colname = "", 
		   row_id_colname = "OutputRowNum", 
		   col_id_colname = "OutputColNum", 
		   cell_val_colname = "OutputValU",
		   whereconditions=paste0("mtrx.OutputValU IS NOT NULL "))

	# calculating perm FLVector
	table <- FLTable(tempResultTable,
		             "OutputColNum",
		             whereconditions=paste0(tempResultTable,".OutputPermut = 1 ")
		             )

	perm <- table[,"OutputRowNum"]

	# calculating x FLVector
	VID2 <- getMaxVectorId(connection)
	
	sqlstrX <-paste0("SELECT ",VID2," AS vectorIdColumn",
							",ROW_NUMBER() OVER(ORDER BY ",
                                getVariables(LUMatrix)$colId,",",
                                getVariables(LUMatrix)$rowId,") AS vectorIndexColumn
	                   		, ",getVariables(LUMatrix)$value," AS vectorValueColumn 
					  FROM ",tableAndAlias(LUMatrix),
					 constructWhere(constraintsSQL(LUMatrix)))

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstrX)

	flv <- newFLVector(
				select = tblfunqueryobj,
				Dimnames = list(1:length(LUMatrix),
								"vectorValueColumn"),
				isDeep = FALSE)

	# x <- store(object=flv)
    x <- flv

	# calculating Dim vector
	Dim<- dim(data_perm)

	a<-new("FLLU",
		x=x,
		perm=perm,
		Dim=Dim,
		lower=l,
		upper=u,
		data_perm = data_perm,
        Dimnames=dimnames(object)
	)
	class(a)<-"FLLU"

	#sqlSendUpdate(connection,paste0(" DROP TABLE ",getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable)))
	return(a)
}

#' @export
print.FLLU<-function(object){
	note1<-length(object@x)
	note2<-length(object@perm)
	note3<-length(object@Dim)
	cat("'Matrix Factorization' of Formal class 'denseLU' [package Matrix] with 3 slots\n") #"Matrix"
	cat("..@x	: num[1:",note1,"]")
	print(object@x)
	cat("..@perm	: int[1:",note2,"]")
	print(object@perm)
	cat("..@Dim	: int[1:",note3,"]")
	print(object@Dim)
}

#' @export
setMethod("show","FLLU",print.FLLU)

#' @export
expand<-function(object, ...){
	UseMethod("expand",object)
}

#' @export
expand.default <- Matrix::expand

#' @export
expand.FLLU <- function(object,...)
{
	return(list(L=object@lower,
				U=object@upper,
				P=object@data_perm))
}

#' @export
`$.FLLU`<-function(object,property){
	if(property=="L"){
		object@lower
	}
	else if(property=="U"){
		object@upper
	}
	else if(property=="P"){
		object@data_perm
	}
	else "That's not a valid property"
}
