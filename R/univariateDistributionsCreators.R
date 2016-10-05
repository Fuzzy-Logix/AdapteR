
#' Function to generate S4 methods for univariate distributions code.
#'
#' Univariate distribution functions are defined in data/univariate.RFL.
#' Uses R-like syntax to define function and argument mappings efficiently.
#' This code is parsed programmatically:
#' function definitions like are created programmatically.
#'
#' stats::pweibull            <- FLCDFWeibull(q,location=0,scale,shape)
#'
#' for executing the R function (left of " <- ") with
#' in-dabase with DB Lytix function (right of " <- ").
FLcreateUnivariateMethodCode <- function(definition){
    if(grepl("^ *$",definition)) return(NULL)
    if(grepl("^ *#.*",definition)) return(NULL)
    funNameFull <- gsub(" *<-.*","",definition)
    funName <- gsub("^.*::","",funNameFull)
    FLfunName <- gsub("\\(.*","",gsub(".*<- *","",definition))
    FLfargs <- unlist(strsplit(gsub("^.*\\(","",gsub("\\)$","",definition)),","))
    ##browser()
    if(grepl("^AdapteR::",funNameFull)){
        fargs <- list(gsub("^.*=","",FLfargs))
        fargs[!grepl("=",FLfargs)] <- ""
        names(fargs) <- gsub("=.*$","",FLfargs)
        sig <- gsub("=$","",gsub("=,",",",paste0(FLfargs, collapse=", ")))
        RMethod <- ""
    } else {
        fargs <- eval(parse(text=paste0("as.list(formals(",funNameFull,"))")))
        sig <- gsub("=$","",gsub("=,",",",paste0(names(fargs),"=",fargs, collapse=", ")))
        RMethod <- paste0("setMethod('",funName,"', signature(",names(fargs)[[1]],"='integer'),
       function(",sig,")\n               ",
       funNameFull,"(",paste0(names(fargs), collapse=", "),"))\n")
    }
    generic <- paste0("setGeneric('",funName,"', function(",
                      sig,
                      ") { standardGeneric('",funName,"') })\n")
    
    FLMethod <- paste0("setMethod('",funName,"', signature(",names(fargs)[[1]],"='FLSimpleVector'),
       function(",sig,")\n{\n",
       "  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_",funName,"'\n",
       "  n@select@variables[[1]] <- paste0('",FLfunName,"(',\n",
       "                                    n@select@variables[[1]],',',\n",
       "                                    ",
       paste0(FLfargs[-1], collapse=",',',"),",')')\n",
       "  n\n",
       "})\n")
    paste0("\n\n#' @export\n",generic,RMethod,FLMethod)
}


#' Function to generate S4 methods for univariate distributions code.
#'
#' Univariate distribution functions are defined in data/univariate.RFL.
#' Uses R-like syntax to define function and argument mappings efficiently.
#' This code is parsed programmatically:
#' function definitions like are created programmatically.
#'
#' stats::pweibull            <- FLCDFWeibull(q,location=0,scale,shape)
#'
#' for executing the R function (left of " <- ") with
#' in-dabase with DB Lytix function (right of " <- ").
#' @export
FLcreateUnivariateMethodsFile <- function(definitions='data/univariate.RFL', genFile="AdapteR/R/univariate.R"){
    defs <- readLines(system.file(definitions, package='AdapteR'))
    cat("## This file is in most parts automatically generated.\n",
        file=genFile)
    cat("## Manual changes need to be commited to git and then reintroduced after regenerating the file.\n",
        file=genFile, append=TRUE)
    for(definition in defs){
        methodDef <- FLcreateUnivariateMethodCode(definition)
        cat(methodDef,
            file=genFile, append=TRUE)
    }
}
