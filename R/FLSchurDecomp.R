#' flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"), 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultList <- Schur(flmatrix)
#' 
#' @export
setClass("FLSchur",
         slots = list(data = "FLMatrix",
                      call = "call",
                      results = "list"
                      ))
#' @export
Schur<-function(object, ...){
    UseMethod("Schur",object)
}

#' @export
Schur.default<-Matrix::Schur


## TO-DO : Add dims so as to remove extra data fetching.
#' @export
Schur.FLMatrix<-function(object,...){
    vcallObject <- match.call()
    sqlstr <- constructMatrixUDTSQL(pObject=object,
                                    pFuncName="FLSchurDecompUdt",
                                    pdims=getDimsSlot(object),
                                    pdimnames=dimnames(object),
                                    pReturnQuery=TRUE
                                    )
    val <- as.numeric(gsub(pattern = ".*=", "", object@select@whereconditions))
    sqlstr <- gsub("'%insertIDhere%'",val,sqlstr)
    ##    sqlstr <- gsub(paste0("select ",val," FROM "),"SELECT a.* FROM",sqlstr,ignore.case = TRUE)
    
    tempResultTable <- cacheDecompResults(pFuncName="FLSchurDecompUdt",
                                          pQuery=sqlstr)
    UMatrix <- FLMatrix(connection = connection, 
                        table_name = tempResultTable, 
                        matrix_id_value = "",
                        matrix_id_colname = "", 
                        row_id_colname = "OutputRowNum", 
                        col_id_colname = "OutputColNum", 
                        cell_val_colname = "OutUVal"
                        ##whereconditions=paste0(tempResultTable,".OutUVal IS NOT NULL ")
                        )

    QMatrix <- FLMatrix(connection = connection, 
                        table_name = tempResultTable, 
                        matrix_id_value = "",
                        matrix_id_colname = "", 
                        row_id_colname = "OutputRowNum", 
                        col_id_colname = "OutputColNum", 
                        cell_val_colname = "OutQVal"
                        ##whereconditions=paste0(tempResultTable,".OutUVal IS NOT NULL ")
                        )
    vclass <- "FLSchur"
    return(new(vclass,
               data = object,
               call = vcallObject,
               results = list(Q = QMatrix,
                              T = UMatrix))) }



#' @export
`$.FLSchur`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property == "T"){
        return(object$T)
    }

    if(property == "Q"){
        return(object$Q)
    }
    if(property == "call"){
        return(object$call)
    }}

#' @export
setMethod("names", signature("FLSchur"), function(x) c("call","T", "Q"))


#' @export
print.FLSchur <- function(object,...){
    print(object@results)
}

#' @export
setMethod("show", signature("FLSchur"), function(object) {
    return(print.FLSchur(object))})


