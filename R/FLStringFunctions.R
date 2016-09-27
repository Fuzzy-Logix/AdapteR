#' @include FLMatrix.R
NULL

## move to file stringdist.R
setGeneric("FLStringDist", function(functionName,
                                    xsource,
                                    targets,
                                    vlength=3,
                                    matchWeight=1,
                                    mismatchWeight=-1,
                                    gapPenalty=-1,
                                    caseFlag=0,
                                    asMatrix=FALSE)
    standardGeneric("FLStringDist"))

## move to file stringdist.R
setMethod("FLStringDist",
          signature(functionName="character",
            xsource="character",
            targets="FLVector"),
          function(functionName,
                  xsource,
                  targets,
                  vlength=3,
                  matchWeight=1,
                  mismatchWeight=-1,
                  gapPenalty=-1,
                  caseFlag=0,
                  asMatrix=FALSE)
          {
            if(length(xsource)>1 || asMatrix==TRUE)
            {
              xsource <- as.FLVector(xsource)
              return(FLStringDist(functionName=functionName,
                                  xsource=xsource,
                                  targets=targets,
                                  vlength=vlength,
                                  matchWeight=matchWeight,
                                  mismatchWeight=mismatchWeight,
                                  gapPenalty=gapPenalty,
                                  caseFlag=caseFlag,
                                  asMatrix=asMatrix))
            }
            xsource <- xsource[1]
            a <- genRandVarName()
            if(is.logical(caseFlag)) caseFlag<-as.numeric(caseFlag)
            else if(is.numeric(caseFlag) && (caseFlag==0 ||caseFlag==1)) caseFlag<-caseFlag
            else stop("caseFlag must be numeric 0,1 or logical")

            if(length(targets@dimnames[[2]])>1 && targets@isDeep==FALSE)
            #targets <- store(targets)
            stop("row Vectors not supported for string operations")

            if(functionName=="FLNeedleManWunschDist")
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                             a,".vectorIndexColumn AS vectorIndexColumn,",
                              functionName,"('",xsource,"',",a,".vectorValueColumn,",
                                matchWeight,",",mismatchWeight,",",
                                gapPenalty,",",caseFlag,") AS vectorValueColumn ",
                             " FROM(",constructSelect(targets),") AS ",a)

            else if(functionName=="FLHammingDist")
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                             a,".vectorIndexColumn AS vectorIndexColumn,",
                              functionName,"('",xsource,"',",a,".vectorValueColumn,",
                                caseFlag,",",vlength,") AS vectorValueColumn ",
                             " FROM(",constructSelect(targets),") AS ",a)

            else if(functionName %in% c("FLJaroDist","FLJaccardIndex","FLJaroWinklerDist"))
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                             a,".vectorIndexColumn AS vectorIndexColumn, 1-(",
                             functionName,"('",xsource,"',",a,".vectorValueColumn,",
                             caseFlag,")) AS vectorValueColumn ",
                             " FROM(",constructSelect(targets),") AS ",a)
            else
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                             a,".vectorIndexColumn AS vectorIndexColumn,",
                             functionName,"('",xsource,"',",a,".vectorValueColumn,",
                             caseFlag,") AS vectorValueColumn ",
                             " FROM(",constructSelect(targets),") AS ",a)

            tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = getOption("connectionFL"),
                        variables = list(
                      obs_id_colname = "vectorIndexColumn",
                      cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

            resultvec <- newFLVector(
                            select = tblfunqueryobj,
                            dimnames = list(targets@dimnames[[1]],
                                          "vectorValueColumn"),
                            isDeep = FALSE)
            return(resultvec)
          })

## move to file stringdist.R
setMethod("FLStringDist",
          signature(functionName="character",
            xsource="FLVector",
            targets="FLVector"),
          function(functionName,
                  xsource,
                  targets,
                  vlength=3,
                  matchWeight=1,
                  mismatchWeight=-1,
                  gapPenalty=-1,
                  caseFlag=0,
                  asMatrix=FALSE)
          {
            # if(length(xsource)>1)
            #browser()
            # xsource <- xsource[1]
            a <- genRandVarName()
            b <- genRandVarName()
            if(is.logical(caseFlag)) caseFlag<-as.numeric(caseFlag)
            else if(is.numeric(caseFlag) && (caseFlag==0 ||caseFlag==1)) caseFlag<-caseFlag
            else stop("caseFlag must be numeric 0,1 or logical")

            if(length(targets@dimnames[[2]])>1 && targets@isDeep==FALSE)
            #targets <- store(targets)
            stop("row Vectors are not supported for string operations")
            if(length(xsource@dimnames[[2]])>1 && xsource@isDeep==FALSE)
            #targets <- store(targets)
            stop("row Vectors are not supported for string operations")

            matchWeightFlag <- mismatchWeightFlag <- gapPenaltyFlag <- vlengthFlag <- ""
            caseFlag <- paste0(",",caseFlag)
            if(functionName=="FLNeedleManWunschDist"){
              matchWeightFlag <- paste0(",",matchWeight)
              mismatchWeightFlag <- paste0(",",mismatchWeight)
              gapPenaltyFlag <- paste0(",",gapPenalty)
            }
            if(functionName=="FLHammingDist")
            vlengthFlag <- paste0(",",vlength)

            if(asMatrix){
              if(functionName %in% c("FLJaroDist","FLJaccardIndex","FLJaroWinklerDist"))
              sqlstr <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                              "a.vectorIndexColumn AS rowIdColumn, \n ",
                              "b.vectorIndexColumn AS colIdColumn, 1-( \n ",
                              functionName,"(a.vectorValueColumn,b.vectorValueColumn ",
                                matchWeightFlag,mismatchWeightFlag,
                                gapPenaltyFlag,caseFlag,vlengthFlag,")) AS valueColumn \n ",
                             " FROM(",constructSelect(xsource),") AS a, \n (",
                                      constructSelect(targets),") AS b ")
              else
              sqlstr <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                              "a.vectorIndexColumn AS rowIdColumn, \n ",
                              "b.vectorIndexColumn AS colIdColumn, \n ",
                              functionName,"(a.vectorValueColumn,b.vectorValueColumn ",
                                matchWeightFlag,mismatchWeightFlag,
                                gapPenaltyFlag,caseFlag,vlengthFlag,") AS valueColumn \n ",
                             " FROM(",constructSelect(xsource),") AS a, \n (",
                                      constructSelect(targets),") AS b ")

              tblfunqueryobj <- new("FLTableFunctionQuery",
                    connection = getOption("connectionFL"),
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

              flm <- newFLMatrix(
                               select= tblfunqueryobj,
                               dim=c(length(xsource),
                                     length(targets)),
                               dimnames = list(
                                   names(xsource),
                                   names(targets)))
              return(flm)
            }
            else{

                ifelse(length(xsource)>length(targets),{
                vmaxlen <- length(xsource);
                vminlen <- length(targets);
                vmaxref <- "a";
                ifelse(xsource@isDeep && length(colnames(xsource))>1,
                vmaxrownames <- colnames(xsource),
                vmaxrownames <- rownames(xsource))
                },{
                    vmaxlen <- length(targets);
                    vmaxref <- "b";
                    vminlen <- length(xsource);
                    ifelse(targets@isDeep && length(colnames(targets))>1,
                    vmaxrownames <- colnames(targets),
                    vmaxrownames <- rownames(targets))
                })
               if(functionName %in% c("FLJaroDist","FLJaccardIndex","FLJaroWinklerDist"))
               sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                              vmaxref,".vectorIndexColumn AS vectorIndexColumn, 1-( \n ",
                              functionName,"(a.vectorValueColumn,b.vectorValueColumn ",
                                matchWeightFlag,mismatchWeightFlag,
                                gapPenaltyFlag,caseFlag,vlengthFlag,")) AS vectorValueColumn \n ",
                             " FROM(",constructSelect(xsource),") AS a, \n (",
                                      constructSelect(targets),") AS b \n ",
                             ## " WHERE CAST(FLMOD(a.vectorIndexColumn,",
                             " WHERE CAST((a.vectorIndexColumn MOD ",
                                      vminlen,") AS INT) = ",
                             ## "CAST(FLMOD(b.vectorIndexColumn,",
                                    "CAST((b.vectorIndexColumn MOD ",
                                    vminlen,") AS INT)")
               else
               sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                              vmaxref,".vectorIndexColumn AS vectorIndexColumn, \n ",
                              functionName,"(a.vectorValueColumn,b.vectorValueColumn ",
                                matchWeightFlag,mismatchWeightFlag,
                                gapPenaltyFlag,caseFlag,vlengthFlag,") AS vectorValueColumn \n ",
                             " FROM(",constructSelect(xsource),") AS a, \n (",
                                      constructSelect(targets),") AS b \n ",
                             ## " WHERE CAST(FLMOD(a.vectorIndexColumn,",
                             " WHERE CAST((a.vectorIndexColumn MOD ",
                                      vminlen,") AS INT) = ",
                             ## "CAST(FLMOD(b.vectorIndexColumn,",
                                    "CAST((b.vectorIndexColumn MOD ",
                                    vminlen,") AS INT)")
               tblfunqueryobj <- new("FLTableFunctionQuery",
                      connection = getOption("connectionFL"),
                      variables = list(
                      obs_id_colname = "vectorIndexColumn",
                      cell_val_colname = "vectorValueColumn"),
                      whereconditions="",
                      order = "",
                      SQLquery=sqlstr)

                return(newFLVector(
                          select = tblfunqueryobj,
                          dimnames =list(vmaxrownames,"vectorValueColumn"),
                          isDeep = FALSE))
            }
          })

## move to file stringdist.R
##Assumed that function is symmetric wrt two strings
setMethod("FLStringDist",
          signature(functionName="character",
            xsource="FLVector",
            targets="character"),
          function(functionName,
                  xsource,
                  targets,
                  vlength=3,
                  matchWeight=1,
                  mismatchWeight=-1,
                  gapPenalty=-1,
                  caseFlag=0,
                  asMatrix=FALSE)
          {
            if(length(targets)>1 || asMatrix==TRUE)
            {
              targets <- as.FLVector(targets)
              return(FLStringDist(functionName=functionName,
                                  xsource=xsource,
                                  targets=targets,
                                  vlength=vlength,
                                  matchWeight=matchWeight,
                                  mismatchWeight=mismatchWeight,
                                  gapPenalty=gapPenalty,
                                  caseFlag=caseFlag,
                                  asMatrix=asMatrix))
            }
            else if(length(targets)==1)
            FLStringDist(functionName=functionName,
                                  xsource=targets,
                                  targets=xsource,
                                  vlength=vlength,
                                  matchWeight=matchWeight,
                                  mismatchWeight=mismatchWeight,
                                  gapPenalty=gapPenalty,
                                  caseFlag=caseFlag,
                                  asMatrix=asMatrix)
          })

## move to file stringdist.R
setMethod("FLStringDist",
          signature(functionName="character",
            xsource="character",
            targets="character"),
          function(functionName,
                  xsource,
                  targets,
                  vlength=3,
                  matchWeight=1,
                  mismatchWeight=-1,
                  gapPenalty=-1,
                  caseFlag=0,
                  asMatrix=FALSE)
          {
            if(length(xsource)>1)
            xsource <- xsource[1]
            if(length(targets)>1)
            targets <- targets[1]
            if(is.logical(caseFlag)) caseFlag<-as.numeric(caseFlag)
            else if(is.numeric(caseFlag) && (caseFlag==0 ||caseFlag==1)) caseFlag<-caseFlag
            else stop("caseFlag must be numeric 0,1 or logical")

            if(functionName=="FLNeedleManWunschDist")
            sqlstr <- paste0(" SELECT ",
                              functionName,"('",xsource,"','",targets,"',",
                                matchWeight,",",mismatchWeight,",",
                                gapPenalty,",",caseFlag,") AS vresult ")

            else if(functionName=="FLHammingDist")
            sqlstr <- paste0(" SELECT ",
                              functionName,"('",xsource,"','",targets,"',",
                                caseFlag,",",vlength,") AS vresult ")

            else
            sqlstr <- paste0(" SELECT ",
                              functionName,"('",xsource,"','",targets,"',",caseFlag,") AS vresult")

            resultvec <- sqlQuery(connection,sqlstr)[["vresult"]]
            return(resultvec)
          })

## move to file stringdist.R
#' stringdist
#'
#' compute distance metrics between strings.
#'
#' This function computes pairwise string distances between elements of 
#' a and b, where the argument with less elements is recycled.
#'
#' The following distance metrics are supported:
#' lv:  Levenshtein, calling FLLevenshteinDist;
#' dl: Levenshtein-Damerau, calling FLDLevenshteinDist;
#' hamming: Hamming, calling FLHammingDist;
#' jaccard: Jaccard, calling FLHammingDist;
#' j, p==0: Jaro, calling FLJaroDist; 
#' j, p>0: Jaro-Winkler, calling FLJaroWinklerDist;
#' nmw: Needleman-Wunsch, calling FLNeedleManWunschDist.
#' 
#' @seealso \code{\link[stringdist]{stringdist}} for R function reference
#' implementation.
#'
#' @param a character or FLVector of characters
#' @param b character or FLVector of characters
#' @param method can be \code{c("lv","dl","hamming","jaccard","jw","nmw")}
#' where lv - Levenshtein, dl - Levenshtein-Damerau,
#' jw - Jaro-Winkler, nmw - NeedleManWunsch. Default is "lv"
#' @param weight for method=nmw, weights and penalties for match, mismatch and gaps,
#' integer weights for matching sequential(d), nonmatching non-sequential characters(i)
#' between the strings, and integer penality for gaps(s) (ideally negative).
#' @param caseFlag logical or 0/1 indicating
#' if comparision should be case sensitive
#' @param p penality factor for jaro-winkler
#' if p==0 jaro distance is computed
#' @param vlength optional, length of strings to compare
#' used for hamming
#' @param ... 
#' @return FLVector if any \code{a} or \code{b}
#' is R character of length 1. Otherwise returns a FLMatrix.
#' @section Constraints:
#' row vectors are not supported currently.
#' Output is slightly different from stringdist::stringdist.
#' Refer to \code{@return} section.
#' @examples 
#' widetable  <- FLTable("iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- stringdist("xyz",flv)
#' resultflvector <- stringdist("xyz",flv,method="lv",caseFlag=1)
#' resultflvector <- stringdist("xyz",flv,method="hamming",vlength=4)
#' resultflmatrix <- stringdist(flv,flv,method="jw",p=1)
#' resultflmatrix <- stringdist(c("xyz","poli"),flv,method="jw")
#' @export
setGeneric("stringdist", function(a,b,
                                  method="osa",
                                  useBytes=FALSE,
                                  weight = c(d = 1, i = 1, s = 1, t = 1),
                                  maxDist = Inf, 
                                  q = 1, 
                                  p = 0, 
                                  nthread = getOption("sd_num_thread"),
                                  caseFlag=0,
                                  vlength=3,
                                  ...)
    standardGeneric("stringdist"))

## move to file stringdist.R
setMethod("stringdist",
          signature(a="character",
                    b="character"),
          function(a,b,
                  method="osa",
                  useBytes=FALSE,
                  weight=c(d = 1, i = 1, s = 1, t = 1),
                  maxDist = Inf, 
                  q = 1, 
                  p = 0, 
                  nthread = getOption("sd_num_thread"),
                  caseFlag=0,
                  vlength=3,...){
            return(stringdist::stringdist(a,b,method=method,
                        useBytes=useBytes,
                        weight=weight,
                        maxDist = maxDist, 
                        q = q, 
                        p = p, 
                        nthread = nthread,
                        ...))
          })

## move to file stringdist.R
setMethod("stringdist",
          signature(a="ANY", b="ANY"),
          function(a,b,
                  method=c("osa"),
                  useBytes=FALSE,
                  weight=c(d = 1, i = 1, s = 1, t = 1),
                  maxDist = Inf, 
                  q = 1, 
                  p = 0, 
                  nthread = getOption("sd_num_thread"),
                  caseFlag=0,
                  vlength=3,...){
            
            if(!is.FL(a) && !is.FL(b))
            return(stringdist::stringdist(a,b,method=method,
                    useBytes=useBytes,
                    weight=weight,
                    maxDist = maxDist, 
                    q = q, 
                    p = p, 
                    nthread = nthread,
                    ...))

            FLStringDistFunctionsClassCheck(a,b)
            
            if(!(method %in% c("lv","dl","hamming","jaccard","jw","nmw")))
            if(method == "osa"){
              cat("osa not supported for FLTypes...Using dl instead \n ")
              method <- "dl"
            }
            else{
              stop("method not supported for FLTypes \n ")
            }

            if(method=="lv")
              return(FLStringDist("FLLevenshteinDist",
                                  a,b,caseFlag=caseFlag))
            else if(method=="dl")
              return(FLStringDist("FLDLevenshteinDist",
                                  a,b,caseFlag=caseFlag))
            else if(method=="hamming")
              return(FLStringDist("FLHammingDist",
                                  a,b,caseFlag=caseFlag,
                                  vlength=vlength))
            else if(method=="jaccard")
              return(FLStringDist("FLJaccardIndex",
                                  a,b,caseFlag=caseFlag))
            else if(method=="jw"){
              if(p==0)
                return(FLStringDist("FLJaroDist",
                                    a,b,caseFlag=caseFlag))
              else
              return(FLStringDist("FLJaroWinklerDist",
                      a,b,caseFlag=caseFlag))
            }
            else if(method=="nmw")
            return(FLStringDist("FLNeedleManWunschDist",
                                 a,b,
                                 matchWeight=ifelse(is.na(weight["d"]),1,weight[["d"]]),
                                 mismatchWeight=ifelse(is.na(weight["i"]),-1,weight[["i"]]),
                                 gapPenalty=ifelse(is.na(weight["s"]),-1,weight[["s"]]),
                                 caseFlag=caseFlag))
          })

## move to file stringdist.R
#' stringdistmatrix
#'
#' compute distance metrics between strings.
#'
#' stringdistmatrix computes the string distance matrix with rows
#' according to a and columns according to b.
#'
#' @seealso \code{\link[stringdist]{stringdist}} for R function reference
#' implementation.
#'
#' @param a character or FLVector of characters
#' @param b character or FLVector of characters
#' @param method can be \code{c("lv","dl","hamming","jaccard","jw")}
#' where lv - levenshtein, dl - levenshtein.damerau
#' jw - jaro-winkler. Default is "lv"
#' @param caseFlag logical or 0/1 indicating
#' if comparision should be case sensitive
#' @param p penality factor for jaro-winkler
#' if p==0 jaro distance is computed
#' @param vlength optional, length of strings to compare
#' used for hamming
#' @return FLMatrix of string distances
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflmatrix <- stringdistmatrix("xyz",flv)
#' resultflmatrix <- stringdistmatrix(c("xyz","abc"),flv,method="lv",caseFlag=1)
#' resultflmatrix <- stringdistmatrix("xyz",flv,method="hamming",vlength=4)
#' resultflmatrix <- stringdistmatrix(flv,flv,method="jw",p=1)
#' resultflmatrix <- stringdistmatrix(flv,c("xyz","abc"),method="jw")
#' @export
setGeneric("stringdistmatrix", 
          function(a,b,method="osa",
                  useBytes=FALSE,
                  weight = c(d = 1, i = 1, s = 1, t = 1),
                  maxDist = Inf, 
                  q = 1, 
                  p = 0,
                  useNames = c("none", "strings", "names"),
                  ncores = 1,
                  cluster = NULL,
                  nthread = getOption("sd_num_thread"),
                  caseFlag=0,
                  vlength=3,
                  asMatrix=TRUE,
                  ...)
    standardGeneric("stringdistmatrix"))

## move to file stringdist.R
setMethod("stringdistmatrix",
          signature(a="character",
            b="character"),
          function(a,b,method="osa",
                  useBytes=FALSE,
                  weight = c(d = 1, i = 1, s = 1, t = 1),
                  maxDist = Inf, 
                  q = 1, 
                  p = 0,
                  useNames = c("none", "strings", "names"),
                  ncores = 1,
                  cluster = NULL,
                  nthread = getOption("sd_num_thread"),
                  caseFlag=0,
                  vlength=3,
                  asMatrix=TRUE,
                  ...)
          stringdist::stringdistmatrix(a, b,
                            method=method,
                            useBytes=useBytes,
                            weight=weight,
                            maxDist = maxDist, 
                            q = q, 
                            p = p,
                            useNames = useNames,
                            ncores = ncores,
                            cluster = cluster,
                            nthread = nthread,
                            ...)
          )

## move to file stringdist.R
setMethod("stringdistmatrix",
          signature(a="ANY",
            b="ANY"),
          function(a,b,
                  method="osa",
                  useBytes=FALSE,
                  weight = c(d = 1, i = 1, s = 1, t = 1),
                  maxDist = Inf, 
                  q = 1, 
                  p = 0,
                  useNames = c("none", "strings", "names"),
                  ncores = 1,
                  cluster = NULL,
                  nthread = getOption("sd_num_thread"),
                  caseFlag=0,
                  vlength=3,
                  asMatrix=TRUE,
                  ...)
          {
            if((!is.FL(a) && missing(b)) || (!is.FL(a) && !is.FL(b)))
            return(stringdist::stringdistmatrix(a,b,method=method,
                            useBytes=useBytes,
                            weight=weight,
                            maxDist = maxDist, 
                            q = q, 
                            p = p,
                            useNames = useNames,
                            ncores = ncores,
                            cluster = cluster,
                            nthread = nthread,
                            ...))
            if(missing(b))
            b <- a
            FLStringDistFunctionsClassCheck(a,b)

            if(!(method %in% c("lv","dl","hamming","jaccard","jw","nmw")))
            if(method == "osa"){
              warning("osa not supported for FLTypes...Using dl instead.")
              method <- "dl"
            }
            else{
              stop("method not supported for FLTypes \n ")
            }

            if(method=="lv")
            return(FLStringDist("FLLevenshteinDist",
                      a,b,caseFlag=caseFlag,
                      asMatrix=TRUE))
            else if(method=="dl")
            return(FLStringDist("FLDLevenshteinDist",
                      a,b,caseFlag=caseFlag,
                      asMatrix=TRUE))
            else if(method=="hamming")
            return(FLStringDist("FLHammingDist",
                      a,b,caseFlag=caseFlag,
                      vlength=vlength,
                      asMatrix=TRUE))
            else if(method=="jaccard")
            return(FLStringDist("FLJaccardIndex",
                      a,b,caseFlag=caseFlag,
                      asMatrix=TRUE))
            else if(method=="jw")
            {
              if(p==0)
              return(FLStringDist("FLJaroDist",
                      a,b,caseFlag=caseFlag,
                      asMatrix=TRUE))
              else
              return(FLStringDist("FLJaroWinklerDist",
                      a,b,caseFlag=caseFlag,
                      asMatrix=TRUE))
            }
             else if(method=="nmw")
            return(FLStringDist("FLNeedleManWunschDist",
                                 a,b,
                                 matchWeight=ifelse(is.na(weight["d"]),1,weight[["d"]]),
                                 mismatchWeight=ifelse(is.na(weight["i"]),-1,weight[["i"]]),
                                 gapPenalty=ifelse(is.na(weight["s"]),-1,weight[["s"]]),
                                 caseFlag=caseFlag,
                                 asMatrix=TRUE))
          })

## move to file stringdist.R
FLStringDistFunctionsClassCheck <- function(a,b)
{
  if(!(class(a)=="FLVector" || is.character(a)))
  stop(" a should be FLVector or character ")
  if(!(class(b)=="FLVector" || is.character(b)))
  stop(" a should be FLVector or character ")
}

################################################################################
setGeneric("FLStrCommon", 
          function(functionName,
                  object,
                  delimiter="",
                  stringpos=1,
                  ...)
    standardGeneric("FLStrCommon"))

setMethod("FLStrCommon",
        signature(object="FLVector"),
        function(functionName,
                object,
                delimiter="",
                stringpos=1,
                ...)
        {
          if("type" %in% names(list(...)))
            vtype <- list(...)$type
          else vtype <- "character"
          a <- genRandVarName()
          if(length(object@dimnames[[2]])>1 && object@isDeep==FALSE)
          stop("row Vectors not supported for string operations")

          if(is.null(delimiter)||is.na(delimiter)||length(delimiter)==0)
          stop("invalid delimiter argument")
          delimiter <- delimiter[1]
      
          if(functionName=="FLExtractStr" || functionName=="FLReplaceChar")
          {
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                             a,".vectorIndexColumn AS vectorIndexColumn,",
                              functionName,"(",a,".vectorValueColumn,",
                                fquote(delimiter),",",stringpos,") AS vectorValueColumn ",
                             " FROM(",constructSelect(object),") AS ",a)
          }
          else if(functionName=="FLInstr")
          {
            vfun <- paste0(functionName,"(0,",a,".vectorValueColumn,",fquote(delimiter),")")
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                             a,".vectorIndexColumn AS vectorIndexColumn,",
                              "CASE WHEN ",vfun,"= -1 THEN -1 ELSE ",
                              vfun," + 1 END AS vectorValueColumn ",
                             " FROM(",constructSelect(object),") AS ",a)
          }
          else if(functionName=="FLIsHex" || functionName=="FLIsNumeric"
                  || functionName=="FLCleanStr" || functionName=="FLSqueezeSpace")
          {
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                             a,".vectorIndexColumn AS vectorIndexColumn,",
                              functionName,"(",a,".vectorValueColumn) AS vectorValueColumn ",
                             " FROM(",constructSelect(object),") AS ",a)
          }
          tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = getOption("connectionFL"),
                        variables = list(
                      obs_id_colname = "vectorIndexColumn",
                      cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

            resultvec <- newFLVector(
                            select = tblfunqueryobj,
                            dimnames = list(object@dimnames[[1]],
                                          "vectorValueColumn"),
                            isDeep = FALSE,
                            type=vtype)
            return(resultvec)
          })
################################################################################

##paste0 is already working for FLVectors,but fetches data.
## only single char as delimiter used in DB-Lytix!

## move to file FLConcatString.R
#' Concatenate elements of vector
#'
#' Concatenate elements of FLVector with
#' a delimiter as collapse value.
#'
#' The DB Lytix function called is FLConcatString.
#' The concat string function is an aggregate that joins the values of a string 
#' column from a table(or a vector of characters) into an output string using a 
#' user supplied delimiter to separate the fields.
#'
#' @seealso \code{\link[base]{paste0}} for R function reference
#' implementation.
#'
#' @param object FLVector of characters
#' @param delimiter character
#' @return FLVector of length 1 with result string
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLConcatString(flv,",")
#' @export
setGeneric("FLConcatString",function(object,delimiter)
    standardGeneric("FLConcatString"))

## move to file FLConcatString.R
setMethod("FLConcatString",
          signature(object="FLVector",
                    delimiter="character"),
          function(object,delimiter){
            if(is.null(delimiter)||is.na(delimiter)||length(delimiter)==0)
            stop("invalid delimiter argument")
            if(delimiter=="") stop("delimiter cannot be empty currently")
            a <- genRandVarName()
            b <- genRandVarName()

            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                                      "1 AS vectorIndexColumn,",
                                  "FLConcatString(",b,".vectorValueColumn,",
                                    fquote(delimiter),") AS vectorValueColumn ",
                             " FROM(",
                                  " SELECT ROW_NUMBER()OVER(ORDER BY ",a,".vectorIndexColumn) AS vectorIndexColumn,",
                                                a,".vectorValueColumn AS vectorValueColumn ",
                                  " FROM(",constructSelect(object),") AS ",a,") AS ",b,
                             " GROUP BY 1,2")

            tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = getOption("connectionFL"),
                        variables = list(
                      obs_id_colname = "vectorIndexColumn",
                      cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

            resultvec <- newFLVector(
                            select = tblfunqueryobj,
                            dimnames = list(1,
                                          "vectorValueColumn"),
                            isDeep = FALSE)
            return(resultvec)
            })
################################################################################

## move to file FLCleanStr.R
#' Clean string
#'
#' Remove non-printable characters from each
#' element of FLVector of strings.
#'
#' The DB lytix function called is FLCleanStr.
#' The clean string function is a scaler that removes all non-printable
#' characters from a string(vector of characters) and outputs a formatted string.
#'
#' @param object FLVector of characters
#' @return a clean FLVector 
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLCleanStr(flv)
#' @export
setGeneric("FLCleanStr",function(object)
    standardGeneric("FLCleanStr"))

## move to file FL StringFunctions.R
setMethod("FLCleanStr",
          signature(object="FLVector"),
          function(object){
            return(FLStrCommon(functionName="FLCleanStr",
                            object=object))
            })

## move to file FLIsHex.R
#' Check if HexaDecimal
#'
#' Check if an element is hexadecimal number
#'
#' The DB Lytix function called is FLIsHex.
#' FLIsHex function determines if the input string is a valid Hexadecimal number.
#' FLIsHex function does not require a leading 0 for an expression to be
#' evaluated as a hexadecimal.
#'
#' @param object FLVector
#' @return FLVector with 1 for TRUE and 0 for FALSE
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLIsHex(flv)
#' @export
setGeneric("FLIsHex",function(object)
    standardGeneric("FLIsHex"))

## move to file FLIsHex.R
setMethod("FLIsHex",
          signature(object="FLVector"),
          function(object){
            return(FLStrCommon(functionName="FLIsHex",
                            object=object,
                            type="logical"))
            })
## move to file FLIsNumeric.R
#' Check if Numeric
#'
#' Check if an element is numeric
#'
#' The DB Lytix function called is FLIsNumeric.FLIsNumeric function determines if the 
#' input string is a valid decimal number.
#'
#' @seealso \code{\link[base]{numeric}} for R function reference
#' implementation.
#'
#' @param object FLVector
#' @return FLVector with 1 for TRUE and 0 for FALSE
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLIsNumeric(flv)
#' @export
setGeneric("FLIsNumeric",function(object)
    standardGeneric("FLIsNumeric"))

## move to file FLIsNumeric.R
setMethod("FLIsNumeric",
          signature(object="FLVector"),
          function(object){
            return(FLStrCommon(functionName="FLIsNumeric",
                            object=object,
                            type="logical"))
            })

## move to file FLSqueezeSpace.R
#' Remove extra spaces in strings
#'
#' Removes extra spaces from elements
#'
#' The DB Lytix function called is FLSqueezeSpace.The squeeze space function is a scalar 
#' that removes extra spaces within a string and outputs a properly formatted string.
#' 
#' @seealso \code{\link[base]{gsub}} for base aproach,  \code{\link[stringr]{str_replace_all}} 
#' and \code{\link[stringr]{str_trim}} for stringr approach, 
#'  \code{\link[stringi]{stri_replace_all_charclass}} and \code{\link[stringi]{stri_trim}}  
#'  for stringi approach in R function reference implementation.
#'
#' @param object FLVector of characters
#' @return FLVector with extra spaces removed
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLSqueezeSpace(flv)
#' @export
setGeneric("FLSqueezeSpace",function(object)
    standardGeneric("FLSqueezeSpace"))

## move to file FLSqueezeSpace.R
setMethod("FLSqueezeSpace",
          signature(object="FLVector"),
          function(object){
            return(FLStrCommon(functionName="FLSqueezeSpace",
                            object=object))
            })


## No point in overloading strsplit because list
## output is not possible and only single char taken from
## delimiter in DB-Lytix

## move to file FLExtractStr.R
#' Extract parts of strings
#'
#' Extract sub-strings separated by a
#' delimiter and identified by their position.
#'
#' The DB lytix function called is FlExtractStr.The extract string function is a scaler that
#' extracts a segment from a string concatenated with delimiter. The position parameter
#' indicates the location of the delimiter.If the string input doesn't have the delimiter 
#' indicated, a null is returned. If the last segment doesn't have a trailing delimiter 
#' but the position is indicated, then the last segment is returned.
#'
#' @seealso \code{\link[base]{substr}} , \code{\link[base]{strsplit}} for R function reference
#' implementation.
#'
#' @param object FLVector of characters
#' @param delimiter character
#' @param stringpos identifier to reference the
#' sub-string given by its position
#' @return FLVector
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLExtractStr(flv,"A",1)
#' @export
setGeneric("FLExtractStr",function(object,delimiter,stringpos)
    standardGeneric("FLExtractStr"))

## move to file FLExtractStr.R
setMethod("FLExtractStr",
          signature(object="FLVector",
                    delimiter="character",
                    stringpos="numeric"),
          function(object,delimiter,stringpos){
            return(FLStrCommon(functionName="FLExtractStr",
                            object=object,
                            delimiter=delimiter,
                            stringpos=as.integer(stringpos[1])))
            })


## move to file regexpr.R
#' Pattern Matching
#'
#' Match \code{pattern} in each element of \code{text}  
#' 
#' The DB Lytix function called is FLInstr.This function returns the position of 
#' the first occurrence of one string within another starting from the
#' search position indicated.
#'
#' @seealso \code{\link[base]{regexpr}} for R function reference implementation.
#'
#' @param pattern string to search for
#' @param text FLVector of characters or R vector to be searched.
#' where matches are sought
#' @param ignore.case logical indicating case-sensitivity.
#' Currently always FALSE for FLVectors
#' @param perl logical. Should perl-compatible regexps be used?
#' Always FALSE for FLVectors
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#' For FLVectors, regualar expressions are not supported
#' @param useBytes If TRUE the matching is done byte-by-byte
#' rather than character-by-character. Always FALSE for FLVector
#' @param startpos integer(numeric) specifying starting position for each search
#' @return FLVector with position of first match
#' or -1 for no match or R Vector
#' @section Constraints:
#' row FLVectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- regexpr("A",flv)
#' @export


## RV: StartPos argument in DB Lytix for FLInStr had not been implemented here in regexpr though it is calling FLInStr. 
## TODO: implement passing of startpos argument(default = 1)
## RV: StartPos argument need only be included in regexpr since only that is calling DB Lytix FLInStr right?
##     Others(gregexpr,grep,sub,......) dont though they essentially perform similar functions; so we dont need startpos there right???

setGeneric("regexpr", function(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE,startpos = 1)
    standardGeneric("regexpr"))

## move to file regexpr.R
setMethod("regexpr",
          signature(
            text="FLVector"),
          function(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE,startpos = 1)
          {
            if(is.null(pattern)||is.na(pattern)||length(pattern)==0)
            pattern <- "NULL"
            return(FLStrCommon("FLInstr",
                      text,delimiter=pattern,stringpos=startpos,
                      type="integer"))
          })

## move to file regexpr.R
setMethod("regexpr",
          signature(
            text="ANY"),
          function(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE,startpos = 1)
          base::regexpr(pattern,text,ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes)
          )

## move to file gregexpr.R
#' Pattern Matching
#'
#' Match \code{pattern} in each element of \code{text}
#'
#' @seealso \code{\link[base]{gregexpr}} for R function reference implementation.
#' @param pattern string to search for
#' @param text FLVector of characters or R vector
#' where matches are sought
#' @param ignore.case logical indicating case-sensitivity.
#' Currently always FALSE for FLVectors
#' @param perl logical. Should perl-compatible regexps be used?
#' Always FALSE for FLVectors
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#' For FLVectors, regualar expressions are not supported
#' @param useBytes If TRUE the matching is done byte-by-byte
#' rather than character-by-character. Always FALSE for FLVector
#' @return FLVector with position of first match
#' or -1 for no match or R Vector
#' @section Constraints:
#' row FLVectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- gregexpr("A",flv)
#' @export
setGeneric("gregexpr", function(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)
    standardGeneric("gregexpr"))

## move to file gregexpr.R
setMethod("gregexpr",
          signature(
            text="ANY"),
          function(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)
          {
            base::gregexpr(pattern, text, 
            ignore.case = ignore.case, perl = perl,
            fixed = fixed, useBytes = useBytes)
          })

## move to file grep.R
#' Pattern Matching
#'
#' Match \code{pattern} in each element of \code{x}
#'
#' @seealso \code{\link[base]{grep}} for R function reference implementation.
#' @param pattern string to search for
#' @param x FLVector of characters or R vector
#' where matches are sought
#' @param ignore.case logical indicating case-sensitivity.
#' Currently always FALSE for FLVectors
#' @param perl logical. Should perl-compatible regexps be used?
#' Always FALSE for FLVectors
#' @param value if TRUE value of element is returned rather than indices.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#' For FLVectors, regualar expressions are not supported
#' @param useBytes If TRUE the matching is done byte-by-byte
#' rather than character-by-character. Always FALSE for FLVector
#' @param invert  If TRUE return indices or values for elements that do not match.
#' @return FLVector or R vector with indices or values where match is found
#' @section Constraints:
#' row FLVectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' flvector <- grep("A",flv,value=TRUE)
#' flvector <- grep("A",flv,invert=TRUE)
#' flvector <- grep("A",flv,invert=TRUE,value=TRUE)
#' @export

setGeneric("grep", function(pattern,x,ignore.case=FALSE,
                            perl=FALSE,value=FALSE,
                            fixed=FALSE,useBytes=FALSE,invert=FALSE)
    standardGeneric("grep"))

setMethod("grep",
          signature(
            x="FLVector"),
          function(pattern,x,ignore.case=FALSE,
                  perl=FALSE,value=FALSE,
                  fixed=FALSE,useBytes=FALSE,invert=FALSE)
          {
            if(is.null(pattern)||is.na(pattern)||length(pattern)==0)
            pattern <- ""
            a <- genRandVarName()
            b <- genRandVarName()
            object <- x
            if(length(object@dimnames[[2]])>1 && object@isDeep==FALSE)
            stop("row Vectors not supported for string operations")

            sqlstr <- paste0("SELECT COUNT(",b,".vectorIndexColumn) AS vlength",
                            " FROM(SELECT '%insertIDhere%' AS vectorIdColumn,",
                                     a,".vectorIndexColumn AS vectorIndexColumn,",
                                    "FLInstr(0,",a,".vectorValueColumn,",fquote(pattern),") AS vectorValueColumn ",
                                     " FROM(",constructSelect(object),") AS ",a,") AS ",b,
                            " WHERE ",b,".vectorValueColumn IS NOT NULL AND ",
                                      b,".vectorValueColumn ",ifelse(invert,"=","<>")," -1")

            vlength <- sqlQuery(getOption("connectionFL"),sqlstr)[1,1]
            sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
                                    "ROW_NUMBER()OVER(ORDER BY CAST(",b,".vectorIndexColumn AS INT)) AS vectorIndexColumn,",
                                    b,ifelse(value,".vectorIdColumn",".vectorIndexColumn")," AS vectorValueColumn",
                            " FROM(SELECT ",a,".vectorValueColumn AS vectorIdColumn,",
                                     a,".vectorIndexColumn AS vectorIndexColumn,",
                                    "FLInstr(0,",a,".vectorValueColumn,",fquote(pattern),") AS vectorValueColumn ",
                                     " FROM(",constructSelect(object),") AS ",a,") AS ",b,
                            " WHERE ",b,".vectorValueColumn IS NOT NULL AND ",
                                      b,".vectorValueColumn ",ifelse(invert,"=","<>")," -1")
            tblfunqueryobj <- new("FLTableFunctionQuery",
                                connection = getOption("connectionFL"),
                                variables = list(
                                obs_id_colname = "vectorIndexColumn",
                                cell_val_colname = "vectorValueColumn"),
                                whereconditions="",
                                order = "",
                                SQLquery=sqlstr)

            resultvec <- newFLVector(
                            select = tblfunqueryobj,
                            dimnames = list(1:vlength,
                                          "vectorValueColumn"),
                            isDeep = FALSE,
                            type="integer")
            return(resultvec)
          })

setMethod("grep",
          signature(
            x="ANY"),
          function(pattern,x,ignore.case=FALSE,
                  perl=FALSE,value=FALSE,
                  fixed=FALSE,useBytes=FALSE,invert=FALSE)
          base::grep(pattern,x, 
            ignore.case = ignore.case, perl = perl,
            value=value,
            fixed = fixed, useBytes = useBytes,
            invert=invert)
          )

## move to file grepl.R
#' Pattern Matching
#'
#' Match \code{pattern} in each element of \code{x}
#'
#' @seealso \code{\link[base]{grepl}} for R function reference implementation.
#' @param pattern string to search for
#' @param x FLVector of characters or R vector
#' where matches are sought
#' @param ignore.case logical indicating case-sensitivity.
#' Currently always FALSE for FLVectors
#' @param perl logical. Should perl-compatible regexps be used?
#' Always FALSE for FLVectors
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#' For FLVectors, regualar expressions are not supported
#' @param useBytes If TRUE the matching is done byte-by-byte
#' rather than character-by-character. Always FALSE for FLVector
#' @return for FLVector input as x, FLVector with 1 for found
#' and 0 for no match is returned.Else R Vector as in base::grepl
#' is returned.
#' @section Constraints:
#' row FLVectors are not supported currently.
#' Output slightly differs from base::grepl. See \code{return}
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' flvector <- grepl("A",flv)
#' @export
setGeneric("grepl", function(pattern, x, ignore.case = FALSE, perl = FALSE,
      fixed = FALSE, useBytes = FALSE)
    standardGeneric("grepl"))

## move to file grepl.R
setMethod("grepl",
          signature(
            x="FLVector"),
          function(pattern, x, ignore.case = FALSE, perl = FALSE,
      fixed = FALSE, useBytes = FALSE)
          {
            if(is.null(pattern)||is.na(pattern)||length(pattern)==0)
            pattern <- ""
            a <- genRandVarName()
            b <- genRandVarName()
            object <- x
            if(length(object@dimnames[[2]])>1 && object@isDeep==FALSE)
            stop("row Vectors not supported for string operations")

            sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
                                    b,".vectorIndexColumn AS vectorIndexColumn,",
                                    "CASE WHEN ",b,".vectorValueColumn <> -1 THEN 'TRUE' ELSE 'FALSE' END AS vectorValueColumn",
                            " FROM(SELECT ",a,".vectorValueColumn AS vectorIdColumn,",
                                     a,".vectorIndexColumn AS vectorIndexColumn,",
                                    "FLInstr(0,",a,".vectorValueColumn,",fquote(pattern),") AS vectorValueColumn ",
                                     " FROM(",constructSelect(object),") AS ",a,") AS ",b,
                            " WHERE ",b,".vectorValueColumn IS NOT NULL ")

            tblfunqueryobj <- new("FLTableFunctionQuery",
                                connection = getOption("connectionFL"),
                                variables = list(
                                obs_id_colname = "vectorIndexColumn",
                                cell_val_colname = "vectorValueColumn"),
                                whereconditions="",
                                order = "",
                                SQLquery=sqlstr)

            resultvec <- newFLVector(
                            select = tblfunqueryobj,
                            dimnames = list(object@dimnames[[1]],
                                          "vectorValueColumn"),
                            isDeep = FALSE,
                            type="logical")
            return(resultvec)
          })

setMethod("grepl",
          signature(
            x="ANY"),
          function(pattern, x, ignore.case = FALSE, perl = FALSE,
      fixed = FALSE, useBytes = FALSE)
          base::grepl(pattern,x, 
            ignore.case = ignore.case, perl = perl,
            fixed = fixed, useBytes = useBytes)
          )

## Only one char taken from replacement
## in DB-Lytix!

## move to file sub.R
#' Pattern Matching and Replacement
#'
#' Replace \code{pattern} in each element of \code{x}
#' with \code{replacement}
#'
#' The DB Lytix function called is FLReplaceChar.The replace character function is a
#' scaler that replaces a character in a string with a another character.
#'
#' @seealso \code{\link[base]{sub}} for R function reference implementation.
#' @param pattern string to search for
#' @param the replacement character
#' @param x FLVector of characters or R vector
#' where matches are sought
#' @param ignore.case logical indicating case-sensitivity.
#' Currently always FALSE for FLVectors
#' @param perl logical. Should perl-compatible regexps be used?
#' Always FALSE for FLVectors
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#' For FLVectors, regualar expressions are not supported
#' @param useBytes If TRUE the matching is done byte-by-byte
#' rather than character-by-character. Always FALSE for FLVector
#' @return FLVector or R vector after replacement
#' @section Constraints:
#' row FLVectors are not supported currently.
#' Currently only one character is used for replacement.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' flvector <- sub("A","X",flv)
#' @export
setGeneric("sub", function(pattern,replacement,x, 
                    ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE)
    standardGeneric("sub"))

## move to file sub.R
setMethod("sub",
          signature(
            x="FLVector"),
          function(pattern, replacement, x, 
            ignore.case = FALSE, perl = FALSE,
            fixed = FALSE, useBytes = FALSE)
          {
            if(is.null(pattern)||is.na(pattern)||length(pattern)==0)
            stop("invalid pattern argument")
            if(is.null(replacement)||is.na(replacement)||length(replacement)==0)
            stop("invalid replacement argument")
            return(FLStrCommon("FLReplaceChar",
                      x,delimiter=pattern[1],
                      stringpos=fquote(replacement[1])))
          })

setMethod("sub",
          signature(
            x="ANY"),
          function(pattern, replacement,x, 
            ignore.case = FALSE, perl = FALSE,
            fixed = FALSE, useBytes = FALSE)
          base::sub(pattern, replacement,x, 
            ignore.case = ignore.case, perl = perl,
            fixed = fixed, useBytes = useBytes)
          )

## move to file gsub.R
#' Pattern Matching and Replacement
#'
#' Replace \code{pattern} in each element of \code{x}
#' with \code{replacement}
#'
#' The DB Lytix function called is FLReplaceChar.The replace character function is a
#' scaler that replaces a character in a string with a another character.
#'
#' @seealso \code{\link[base]{gsub}} for R function reference implementation.
#' @param pattern string to search for
#' @param the replacement character
#' @param x FLVector of characters or R vector
#' where matches are sought
#' @param ignore.case logical indicating case-sensitivity.
#' Currently always FALSE for FLVectors
#' @param perl logical. Should perl-compatible regexps be used?
#' Always FALSE for FLVectors
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#' For FLVectors, regualar expressions are not supported
#' @param useBytes If TRUE the matching is done byte-by-byte
#' rather than character-by-character. Always FALSE for FLVector
#' @return FLVector or R vector after replacement
#' @section Constraints:
#' row FLVectors are not supported currently.
#' Currently only one character is used for replacement.
#' @examples 
#' widetable  <- FLTable("tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' flvector <- gsub("A","X",flv)
#' @export
setGeneric("gsub", function(pattern,replacement,x, 
                    ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE)
    standardGeneric("gsub"))

## move to file gsub.R
setMethod("gsub",
          signature(
            x="FLVector"),
          function(pattern, replacement, x, 
            ignore.case = FALSE, perl = FALSE,
            fixed = FALSE, useBytes = FALSE)
          {
            if(is.null(pattern)||is.na(pattern)||length(pattern)==0)
            stop("invalid pattern argument")
            if(is.null(replacement)||is.na(replacement)||length(replacement)==0)
            stop("invalid replacement argument")
            return(FLStrCommon("FLReplaceChar",
                      x,delimiter=pattern[1],
                      stringpos=fquote(replacement[1])))
          })

## move to file gsub.R
setMethod("gsub",
          signature(
            x="ANY"),
          function(pattern, replacement,x, 
            ignore.case = FALSE, perl = FALSE,
            fixed = FALSE, useBytes = FALSE)
          base::gsub(pattern, replacement,x, 
            ignore.case = ignore.case, perl = perl,
            fixed = fixed, useBytes = useBytes)
          )


#################################################################################

## move to file FLParseXML.R
#' Parse XML files
#'
#' Parse XML files stored in-database
#' as elements of FLVector.
#'
#' The DB Lytix function called is FLParseXMLUdt.An XML parser is a utility that goes through 
#' text documents containing XML trees and allows the information in the 
#' hierarchy to be extracted to replicate the tree structure in a columnar layout.
#'
#' @seealso  \code{\link[XML]{xmlParse}} 
#' @param object FLVector of characters
#' @return dataframe with parsed XML
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' wtd <- FLTable("tblXMLTest","GroupID")
#' flv <- wtd[,"pXML"]
#' resultdataframe <- FLParseXML(flv)
#' @export
setGeneric("FLParseXML", function(object)
    standardGeneric("FLParseXML"))

## move to file FLParseXML.R
setMethod("FLParseXML",
          signature(
            object="FLVector"),
          function(object)
          {
            if(length(object@dimnames[[2]])>1 && object@isDeep==FALSE)
            stop("row Vectors not supported for string operations")
            sqlstr <- paste0("WITH tw (GroupID, pXML)
                              AS (
                              SELECT a.vectorIndexColumn AS GroupID,",
                                    " a.vectorValueColumn AS pXML ",
                              " FROM(",constructSelect(object),") AS a)",
                              " SELECT d.*
                              FROM TABLE (
                              FLParseXMLUdt(tw.GroupID, tw.pXML)
                              HASH BY tw.GroupID
                              LOCAL ORDER BY tw.GroupID
                              ) AS d
                              ORDER BY 1,2;")

            return(sqlQuery(getOption("connectionFL"),sqlstr))
        })
