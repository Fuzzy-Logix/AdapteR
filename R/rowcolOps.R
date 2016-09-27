#' @include FLMatrix.R
NULL

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
           opt<-"SUM"
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

	flv <- newFLVector(
				select = tblfunqueryobj,
				dimnames = list(1:res,
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
	            pInput=list(object),
	            pOperator="rowcolOps"))
}

#' column sums of a FLMatrix.
#'
#' \code{colSums} computes the column-wise sums of FLMatrix objects.
#'
#' @param object is of class FLMatrix.
#' @return \code{colSums} returns a FLVector object representing the col-wise sums.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- colSums(flmatrix)
#' @export
colSums <- function (object, ...){
  UseMethod("colSums", object)
}

#' @export
colSums.default <- base::colSums

#' @export
colSums.FLMatrix <- function(object,...) rowcolOps(object,2,"Sum")



#' column means of a FLMatrix.
#'
#' \code{colMeans} computes the column-wise average of FLMatrix objects.
#'
#' @param object is of class FLMatrix.
#' @param ... any additional arguments.
#' @return \code{colMeans} returns a FLVector object representing the column-wise Means.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- colMeans(flmatrix)
#' @export
colMeans <- function (object, ...){
  UseMethod("colMeans", object)
}

#' @export
colMeans.default <- base::colMeans

#' @export
colMeans.FLMatrix<-function(object,...) rowcolOps(object,2,"Mean")


#' row sums of a FLMatrix.
#'
#' \code{rowSums} computes the row-wise sums of FLMatrix objects.
#'
#' @param object is of class FLMatrix.
#' @param ... any additional arguments
#' @return \code{rowSums} returns a FLVector object representing the row-wise sums.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- rowSums(flmatrix)
#' @export
rowSums <- function (object, ...){
  UseMethod("rowSums", object)
}

#' @export
rowSums.default <- base::rowSums

#' @export
rowSums.FLMatrix<-function(object,...) rowcolOps(object,1,"Sum")

#' row means of a FLMatrix.
#'
#' \code{rowMeans} computes the row-wise average of FLMatrix objects.
#'
#' @param object is of class FLMatrix.
#' @param ... any additional arguments
#' @return \code{rowMeans} returns a FLVector object representing the row-wise Means.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- rowMeans(flmatrix)
#' @export
rowMeans <- function (object, ...){
  UseMethod("rowMeans", object)
}

#' @export
rowMeans.default <- base::rowMeans

#' @export
rowMeans.FLMatrix<-function(object,...) rowcolOps(object,1,"Mean")
