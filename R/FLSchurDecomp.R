#' flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"), 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultList <- Schur(flmatrix)
#' 

#' @export
Schur<-function(object, ...){
	UseMethod("Schur",object)
}

#' @export
Schur.default<-Matrix::Schur

#' @export
Schur.FLMatrix<-function(object,...){
    browser()
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


    

    return(list(QMat = QMatrix,
                UMat = UMatrix)) }
