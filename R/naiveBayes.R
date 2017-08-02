#' @export
setClass(
    "FLnaiveBayes",
    slots=list(call = "call",
               table = "FLTable",
               deeptbl="FLTable",
               AnalysisID = "character",
               results="list"
               ))

#' @export
naiveBayes <- function (formula,data=list(),...) {
    UseMethod("naiveBayes", data)
}

#' @export
naiveBayes.default <- function (formula,data=list(),...) {
    if (!requireNamespace("rpart", quietly = TRUE)){
        stop("e1071 package needed for naiveBayes. Please install it.",
             call. = FALSE)
    }
    else return(e1071::naiveBayes(formula,data,...))
}

#' fltbl <- FLTable("tblNBData", "ObsID", "VarID", "NUM_VAL")
#' flmod <-naiveBayes(object = fltbl, formula = ~.)
#' rtbl <- iris
#' rtbl$Species <- sample(x = 2, size = length(rtbl$Species), replace = TRUE)-1
#' rtbl$Species <- as.numeric(rtbl$Species)
#' colnames(rtbl) <- tolower(colnames(rtbl))
#' fliris <- as.FLTable(rtbl,tableName = getOption("TestTempTableName"),temporary=F, drop = TRUE)
#' flmod <-naiveBayes.FLTable(data = fliris, formula = species~., laplace = 1)
#' rmod <-naiveBayes(data = rtbl, formula = species~.)
#' rtbl <- as.data.frame(Titanic)
#' rtbl <- rtbl[,-5]
#' rtbl <- as.data.frame(lapply(rtbl, function(i)as.numeric(i)-1))
#' colnames(rtbl)[[1]] <- "vcl"
#' rmod <-  naiveBayes(Survived~.,rtbl)
#' fltbl <- as.FL(rtbl)
#' flmod <- naiveBayes.FLTable(formula = Survived~., data = fltbl)
#' @export
naiveBayes.FLTable <- function(formula,data,laplace=0,...){
    vcallObject <- match.call()
    deeptblname <- gen_unique_table_name("naiveb")
    vdeeptbl <- data
    if(!isDeep(vdeeptbl))
    {
        FLdeep <- prepareData(formula         = formula ,
                              data            = vdeeptbl,
                              outDeepTable    = deeptblname,
                              makeDataSparse  = 1,
                              performVarReduc = 0,
                              minStdDev       = .01,
                              perfromNorm = 1,     
                              maxCorrel       = .8,
                              fetchIDs        = FALSE
                              )
        vdeeptbl <- FLdeep$deepx
        vTableName =  FLdeep$deepx@select@table_name[[1]]
        vObsIDColName = FLdeep$deepx@select@variables$obs_id_colname
        vVarIDColName = FLdeep$deepx@select@variables$var_id_colname
        vValueColName= FLdeep$deepx@select@variables$cell_val_colname
        vmapping <- FLdeep$vmapping
    }
    else
    {
        vTableName =  vdeeptbl@select@table_name[[1]]
        vObsIDColName = gsub("flt.", "",data@select@variables$obs_id_colname)
        vVarIDColName = gsub("flt.", "",data@select@variables$var_id_colname)
        vValueColName= gsub("flt.", "",data@select@variables$cell_val_colname)
        vmapping <- colnames(vdeeptbl)
        names(vmapping) <- paste0("Var",vmapping)
    }

    vinputcols<-list(TableName=vTableName,
                     ObsIDCol=vObsIDColName,
                     VarIDCol=vVarIDColName,
                     ValueCol=vValueColName,
                     LaplacianCorrection=as.integer(laplace),
                     Note="Naive Bayes model")
    vfuncName<-"FLNaiveBayesModel"
    AnalysisID <- sqlStoredProc(getFLConnection(),
                                vfuncName,
                                outputParameter=c(AnalysisID="a"),
                                pInputParams=vinputcols)
    
    frame <- sqlQuery(getFLConnection(),paste0("Select * from fzzlNaiveBayesModel
									 where AnalysisID = ",fquote(AnalysisID[[1]])," order by 2,3,4"))
    colnames(frame) <- c("AnalysisID","VarID","VarValue","ClassValue","ClassVarCount")
    vars <- unique(frame$VarID)
    levels <- unique(frame$ClassValue)
    tables<-list()
    for(i in vars){
        subframe<-frame[frame$VarID==i,]
        obj<-table(subframe$VarValue,subframe$ClassValue)
        for(j in 1:nrow(obj)){
            obj[j,2]<-subframe[2*j,"ClassVarCount"]/(subframe[2*j,"ClassVarCount"]+subframe[2*j - 1,"ClassVarCount"])
            obj[j,1]<-subframe[2*j -1,"ClassVarCount"]/(subframe[2*j,"ClassVarCount"]+subframe[2*j - 1,"ClassVarCount"])
        }
        obj <- t(obj)
        vname <- names(vmapping[match(i,vmapping)])
        names(dimnames(obj))<-c("Y",vname)
        eval(parse(text=paste0("tables$",vname,"<-obj")))
    }
    apriori<-sqlQuery(connection,paste0("select ",vValueColName,", count(*) from ",
					vdeeptbl@select@table_name," where ",vVarIDColName," = -1 group by ",vValueColName))
    apriori <- as.data.frame(t(apriori[order(apriori[[1]]),]))
    rownames(apriori) <- NULL
    colnames(apriori) <- NULL

    vclass<-"FLnaiveBayes"
    return(new(vclass,
               table = data,
               call = vcallObject,
               deeptbl = vdeeptbl,
               AnalysisID = as.character(AnalysisID[[1]]),
               results = list(apriori=apriori,
                              tables=tables,
                              levels=levels,
                              formula = formula)))}

#' @export
predict.FLnaiveBayes<-function(object,newdata = object@deeptbl ,scoreTable="",...){
    deeptblname <- gen_unique_table_name("naiveb")
    if(!is.FLTable(newdata)) stop("scoring allowed on FLTable only")
    if(scoreTable=="")
	scoreTable <- gen_score_table_name("NaiveBayes")
    
    if(newdata@select@table_name == object@deeptbl@select@table_name ||
       newdata@select@table_name == object@table@select@table_name){
        newdata <- object@deeptbl
        vTableName =  newdata@select@table_name[[1]]
        vObsIDColName = gsub("flt.", "",newdata@select@variables$obs_id_colname)
        vVarIDColName = gsub("flt.", "",newdata@select@variables$var_id_colname)
        vValueColName= gsub("flt.", "",newdata@select@variables$cell_val_colname)}
    else
    {
        
        FLdeep <- prepareData(formula         = object@results$formula,
                              data            = newdata,
                              outDeepTable    = deeptblname,
                              makeDataSparse  = 1,
                              performVarReduc = 0,
                              minStdDev       = .01,
                              perfromNorm = 1,     
                              maxCorrel       = .8,
                              fetchIDs        = FALSE
                              )
        vdeeptbl <- FLdeep$deepx
        vTableName =  FLdeep$deepx@select@table_name[[1]]
        vObsIDColName = FLdeep$deepx@select@variables$obs_id_colname
        vVarIDColName = FLdeep$deepx@select@variables$var_id_colname
        vValueColName= FLdeep$deepx@select@variables$cell_val_colname }
    
    vinputcols <- c( TableName=vTableName,
                    ObsIDCol=vObsIDColName,
                    VarIDCol=vVarIDColName,
                    ValueCol=vValueColName,
                    AnalysisID=object@AnalysisID,
                    PredictTable=scoreTable,
                    Note="NaiveBayes predict")
    vfuncName<-"FLNaiveBayesPredict"
    AnalysisID <- sqlStoredProc(getFLConnection(),
                                vfuncName,
                                outputParameter=c(AnalysisID="a"),
                                pInputParams=vinputcols)
                                        # sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                                        # 				"ObsID"," AS vectorIndexColumn,",
                                        # 					vval," AS vectorValueColumn",
                                        #  				" FROM ",scoreTable)
    # sqlSendUpdate(getFLConnection(),paste0("alter table ",scoreTable,
    #                                        " add matrix_id int default 1 not null"))
    sqlstr <- paste0("SELECT '%insertIDhere%' AS Matrix_ID, \n ",
                            "ObsID AS rowIdColumn, \n ",
                            "DENSE_RANK() OVER(ORDER BY ClassValue) AS colIdColumn, \n ",
                            "Prob AS valueColumn \n ",
                    " FROM ",scoreTable)
    tblfunqueryobj <- new("FLTableFunctionQuery",
                              connectionName = attr(connection,"name"),
                              variables=list(
                                  Matrix_ID="Matrix_ID",
                                  rowIdColumn="rowIdColumn",
                                  colIdColumn="colIdColumn",
                                  valueColumn="valueColumn"),
                              whereconditions="",
                              order = "",
                              SQLquery=sqlstr)
    flm <- newFLMatrix(
                   select= tblfunqueryobj,
                   dims=c(nrow(newdata),length(object$levels)),
                   Dimnames=list(rownames(newdata),sort(object$levels)),
                   dimColumns=c("Matrix_ID","rowIdColumn","colIdColumn","valueColumn"),
                   type="double")
    # return(FLMatrix(scoreTable,1,"matrix_id","ObsID","ClassValue","Prob"))
    return(flm)
}




#' @export
print.FLnaiveBayes <- function(object){
    print(object@call)
    cat("A-priori probablities \n ")
    if(!is.null(object@results$tables$INTERCEPT))
      print(t(object@results$tables$INTERCEPT))
    else print(object@results$apriori)
    cat(" \n Conditional probablities \n ")
    print(object@results$table)
    return()
}

#' @export
setMethod("show", signature("FLnaiveBayes"), function(object)
    print.FLnaiveBayes(object))





## move to file lm.R
#' @export
`$.FLnaiveBayes`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property == "levels"){
        return(object@results$levels)
 }

    if(property == "call") {
        return(object@call)
    }

    if(property == "apriori"){
        return(object@results$apriori)
    }
    
    if(property == "tables")
    {
        return(object@results$tables)
    } }



#' @export
setMethod("names", signature("FLnaiveBayes"), function(x) c("call","levels","tables", "apriori"))





