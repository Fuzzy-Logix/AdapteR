setGeneric("survdiff",
    function(formula, data,
            subset=TRUE, 
            na.action=getOption("na.action"), 
            rho = 0,
            ...)
        standardGeneric("survdiff"))

setMethod("survdiff",
        signature(formula="formula", 
                  data="ANY"),
        function(formula, data,
                subset=TRUE, 
                na.action=getOption("na.action"), 
                rho = 0,
                ...){
                    return(survival::survdiff(formula=formula,
                                              data=data,
                                              subset=subset,
                                              na.action=na.action,
                                              rho=rho,
                                              ...))
                })

setMethod("survdiff",
        signature(formula="formula", 
                  data="FLTable"),
        function(formula, data,
                subset=NULL, 
                na.action=getOption("na.action"), 
                rho = 0,
                ...){
                    rho <- rho[1]
                    vRhoMap <- c("0"="LogRank",
                                 "1"="PetoPrentice",
                                 "LOGRANK"="LogRank",
                                 "PETO-PRENTICE"="PetoPrentice",
                                 "MODIFIED PETO-PRENTICE"="ModPetoPrent",
                                 "WILCOXON"="Wilcoxon",
                                 "TARON-WARE"="TaronWare")
                    vRho <- vRhoMap[toupper(as.character(rho))]
                    if(is.na(vRho))
                        stop("Allowed rho values are:",unique(c(vRhoMap,0,1))," \n ")
                    data <- setAlias(data,"")
                    connection <- getOption("connectionFL")
                    if(data@isDeep)
                        stop("input table must be wide \n ")
                    vtemp <- prepareSurvivalFormula(data=data,
                                                    formula=formula)
                    for(i in names(vtemp))
                    assign(i,vtemp[[i]])

                    if(!length(vIndepVars)==1)
                        stop("Invalid formula:check function documentation for constraints on formula \n ")

                    vAlpha <- 0.05
                    if("conf.int" %in% names(list(...)))
                        vAlpha <- (1-list(...)[["conf.int"]])
                    vcall <- match.call()
                    vobsIDCol <- getVariables(data)[["obs_id_colname"]]
                    vgroupCols <- unique(c(vobsIDCol,list(...)[["GroupBy"]]))
                    if(any(!setdiff(vgroupCols,vobsIDCol) %in% colnames(data)))
                        stop("columns specified in GroupBy not in data \n ")
                    vgrp <- paste0(vgroupCols,collapse=",")

                    ret <- sqlStoredProc(connection,
                                         "FLKMHypoTest",
                                         TableName = getTableNameSlot(data),
                                         TimeColName= vTimeVal,
                                         StatusColName = vStatus,
                                         SampleIDColname = vIndepVars,
                                         Alpha = vAlpha,
                                         WhereClause = list(...)[["whereconditions"]],
                                         GroupBy = vgrp,
                                         TableOutput = 1,
                                         outputParameter = c(ResultTable = 'a')
                                        )
                    ret <- as.character(ret[1,1])

                    VarID <- c(vIndepVars,
                                "Obs","NumEvents",
                                "Expected","ChiSqApprox",
                                "ChiSq","Prob"
                               )
                    vres <- sqlQuery(connection,
                                        paste0("SELECT DENSE_RANK()OVER(ORDER BY ",vgrp,") AS groupID, \n ",
                                                paste0(VarID,collapse=",")," \n ",
                                               " FROM ",ret," \n ",
                                               " WHERE TestType IN(",fquote(vRho),")",
                                                " ORDER BY groupID,",vIndepVars
                                            )
                                    )
                    colnames(vres) <- c("groupID",VarID)
                    vresList <- dlply(vres,"groupID",
                                    function(x){
                                        n <- x[["Obs"]]
                                        names(n) <- paste0(vIndepVars,"=",
                                                            x[[vIndepVars]])
                                        vtemp <- list(n=as.table(n),
                                                      obs=x[["NumEvents"]],
                                                      exp=x[["Expected"]],
                                                      chisq=unique(x[["ChiSq"]]),
                                                      call=vcall,
                                                      p.value=unique(x[["Prob"]]),
                                                      chisqApprox=x[["ChiSqApprox"]],
                                                      var=matrix(NA,length(n),length(n))
                                                      )
                                        class(vtemp) <- "survdiff"
                                        return(vtemp)
                                    })
                    names(vresList) <- 1:length(vresList)
                    if(length(vresList)==1)
                        vresList <- vresList[[1]]
                    return(vresList)
})