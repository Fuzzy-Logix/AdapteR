## Features to add-
## input can be character vector -> create separate result table for character vectors!
## verify the assumption that func is symmetric
## function.neighbours for hamming and levenshtein

## Examples 
## widetable  <- FLTable("FL_DEMO", "tblAbaloneWide", "ObsID")
## flv <- widetable[2:5,"Sex"] This gives error... indexes should be continous
## flv <- widetable[1:5,"Sex"]
## levenshtein.damerau.distance("xyz",flv)
## levenshtein.damerau.distance(flv,flv,caseFLag=1)
## levenshtein.damerau.distance(flv,"xyz",caseFLag=1)
## levenshtein.distance("xyz",flv)
## levenshtein.distance(flv,flv,caseFLag=1)
## levenshtein.distance(flv,"xyz",caseFLag=1)
## hamming.distance("xyz",flv)
## hamming.distance(flv,flv,caseFLag=1,vlength=4)
## hamming.distance(flv,"xyz",vlength=5)
## stringdist("xyz",flv)
## stringdist("xyz",flv,method="lv",caseFLag=1)
## stringdist("xyz",flv,method="hamming",vlength=4)
## stringdist(flv,flv,method="jw",p=1)
## stringdist("xyz",flv,method="jw")
## FLNeedleManWunschDist("xyz",flv)
## FLNeedleManWunschDist("xyz",flv,2,-2,-1,caseFlag=0)
## FLNeedleManWunschDist(flv,flv,2)

setGeneric("FLStringDist", function(functionName,xsource,targets,caseFlag=0,...)
    standardGeneric("FLStringDist"))

setMethod("FLStringDist",
          signature(functionName="character",
            xsource="character",
            targets="FLVector",
            caseFlag="ANY"),
          function(functionName,
                  xsource,
                  targets,
                  caseFlag=0,
                  vlength=3,
                  matchWeight=1,
                  mismatchWeight=-1,
                  gapPenalty=-1)
          {
            if(length(xsource)>1)
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

            else
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                             a,".vectorIndexColumn AS vectorIndexColumn,",
                              functionName,"('",xsource,"',",a,".vectorValueColumn,",caseFlag,") AS vectorValueColumn ",
                             " FROM(",constructSelect(targets),") AS ",a)

            tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = getOption("connectionFL"),
                        variables = list(
                      obs_id_colname = "vectorIndexColumn",
                      cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

            resultvec <- new("FLVector",
                            select = tblfunqueryobj,
                            dimnames = list(targets@dimnames[[1]],
                                          "vectorValueColumn"),
                            isDeep = FALSE)
            return(resultvec)
          })

setMethod("FLStringDist",
          signature(functionName="character",
            xsource="FLVector",
            targets="FLVector",
            caseFlag="ANY"),
          function(functionName,
                  xsource,
                  targets,
                  caseFlag=0,
                  vlength=3,
                  matchWeight=1,
                  mismatchWeight=-1,
                  gapPenalty=-1)
          {
            # if(length(xsource)>1)
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

            if(functionName=="FLNeedleManWunschDist")
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
                             a,".vectorIndexColumn AS rowIdColumn,",
                             b,".vectorIndexColumn AS colIdColumn,",
                              functionName,"(",a,".vectorValueColumn,",b,".vectorValueColumn,",
                                matchWeight,",",mismatchWeight,",",
                                gapPenalty,",",caseFlag,") AS valueColumn ",
                             " FROM(",constructSelect(xsource),") AS ",a,",(",
                                      constructSelect(targets),") AS ",b)

            else if(functionName=="FLHammingDist")
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
                             a,".vectorIndexColumn AS rowIdColumn,",
                             b,".vectorIndexColumn AS colIdColumn,",
                              functionName,"(",a,".vectorValueColumn,",b,".vectorValueColumn,",caseFlag,",",vlength,") AS valueColumn ",
                             " FROM(",constructSelect(xsource),") AS ",a,",(",
                                      constructSelect(targets),") AS ",b)

            else
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
                             a,".vectorIndexColumn AS rowIdColumn,",
                             b,".vectorIndexColumn AS colIdColumn,",
                              functionName,"(",a,".vectorValueColumn,",b,".vectorValueColumn,",caseFlag,") AS valueColumn ",
                             " FROM(",constructSelect(xsource),") AS ",a,",(",
                                      constructSelect(targets),") AS ",b)

            tblfunqueryobj <- new("FLTableFunctionQuery",
                    connection = getOption("connectionFL"),
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

            flm <- new("FLMatrix",
                             select= tblfunqueryobj,
                             dim=c(length(xsource@dimnames[[1]]),
                                  length(targets@dimnames[[1]])),
                             dimnames = list(
                                 xsource@dimnames[[1]],
                                 targets@dimnames[[1]]))
            return(flm)
          })

##Assumed that function is symmetric wrt two strings
setMethod("FLStringDist",
          signature(functionName="character",
            xsource="FLVector",
            targets="character",
            caseFlag="ANY"),
          function(functionName,xsource,targets,caseFlag=0,...)
          FLStringDist(functionName,targets,xsource,caseFlag,...))

setMethod("FLStringDist",
          signature(functionName="character",
            xsource="character",
            targets="character",
            caseFlag="ANY"),
          function(functionName,
                  xsource,
                  targets,
                  caseFlag=0,
                  vlength=3,
                  matchWeight=1,
                  mismatchWeight=-1,
                  gapPenalty=-1)
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

#' levenshtein.damerau.distance
#'
#' computes the levenshtein-damerau distance between strings
#' @param xsource character or FLVector of characters
#' @param targets character or FLVector of characters
#' @param caseFLag logical or 0/1 indicating 
#' if comparision should be case sensitive
#' @return FLMatrix if both \code{xsource} and \code{targets}
#' are FLVectors. Otherwise returns a FLVector
#' @section Constraints:
#' row vectors are not supported currently 
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- levenshtein.damerau.distance("xyz",flv)
#' resultflmatrix <- levenshtein.damerau.distance(flv,flv,caseFLag=1)
#' resultflvector <- levenshtein.damerau.distance(flv,"xyz",caseFLag=1)
#' @export
setGeneric("levenshtein.damerau.distance", function(xsource,targets,caseFlag=0,...)
    standardGeneric("levenshtein.damerau.distance"))

setMethod("levenshtein.damerau.distance",
          signature(xsource="character",
            targets="character"),
          function(xsource,targets,caseFlag)
          vwr::levenshtein.damerau.distance(xsource, targets)
          )

setMethod("levenshtein.damerau.distance",
          signature(xsource="ANY",
            targets="ANY"),
          function(xsource,targets,caseFlag=0,...)
          {
            FLStringDistFunctionsClassCheck(xsource,targets)
            FLStringDist("FLDLevenshteinDist",
                      xsource,targets,caseFlag)
          })

#' levenshtein.distance
#'
#' computes the levenshtein distance between strings
#' @param xsource character or FLVector of characters
#' @param targets character or FLVector of characters
#' @param caseFLag logical or 0/1 indicating 
#' if comparision should be case sensitive
#' @return FLMatrix if both \code{xsource} and \code{targets}
#' are FLVectors. Otherwise returns a FLVector
#' @section Constraints:
#' row vectors are not supported currently
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- levenshtein.distance("xyz",flv)
#' resultflmatrix <- levenshtein.distance(flv,flv,caseFLag=1)
#' resultflvector <- levenshtein.distance(flv,"xyz",caseFLag=1)
#' @export
setGeneric("levenshtein.distance", function(xsource,targets,caseFlag=0,...)
    standardGeneric("levenshtein.distance"))

setMethod("levenshtein.distance",
          signature(xsource="character",
            targets="character"),
          function(xsource,targets,caseFlag=0,...)
          vwr::levenshtein.distance(xsource, targets)
          )

setMethod("levenshtein.distance",
          signature(xsource="ANY",
            targets="ANY"),
          function(xsource,targets,caseFlag=0,...)
          {
            FLStringDistFunctionsClassCheck(xsource,targets)
            FLStringDist("FLLevenshteinDist",
                      xsource,targets,caseFlag)
          })

#' hamming.distance
#'
#' computes the hamming distance between strings
#' @param xsource character or FLVector of characters
#' @param targets character or FLVector of characters
#' @param caseFLag logical or 0/1 indicating
#' if comparision should be case sensitive
#' @param vlength optional, length of strings to compare
#' @return FLMatrix if both \code{xsource} and \code{targets}
#' are FLVectors. Otherwise returns a FLVector
#' @section Constraints:
#' row vectors are not supported currently
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- hamming.distance("xyz",flv)
#' resultflmatrix <- hamming.distance(flv,flv,caseFLag=1)
#' resultflvector <- hamming.distance(flv,"xyz",caseFLag=1)
#' @export
setGeneric("hamming.distance", function(xsource,targets,caseFlag=0,vlength=3,...)
    standardGeneric("hamming.distance"))

setMethod("hamming.distance",
          signature(xsource="character",
            targets="character"),
          function(xsource,targets,caseFlag,vlength,...)
          vwr::hamming.distance(xsource, targets)
          )

setMethod("hamming.distance",
          signature(xsource="ANY",
            targets="ANY"),
          function(xsource,targets,caseFlag=0,vlength=3,...)
          {
            FLStringDistFunctionsClassCheck(xsource,targets)
            if(!is.numeric(vlength))
            stop("vlength should be numeric")
            else
            vlength <- as.integer(vlength)
            FLStringDist("FLHammingDist",
                      xsource,targets,caseFlag,vlength=vlength)
          })

#' stringdist
#'
#' compute distance metrics between strings
#' @param a character or FLVector of characters
#' @param b character or FLVector of characters
#' @param method can be \code{c("lv","dl","hamming","jaccard","jw")}
#' where lv - levenshtein, dl - levenshtein.damerau
#' jw - jaro-winkler. Default is "lv"
#' @param caseFLag logical or 0/1 indicating
#' if comparision should be case sensitive
#' @param p penality factor for jaro-winkler
#' if p==0 jaro distance is computed
#' @param vlength optional, length of strings to compare
#' used for hamming
#' @return FLMatrix if both \code{a} and \code{b}
#' are FLVectors. Otherwise returns a FLVector
#' @section Constraints:
#' row vectors are not supported currently
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- stringdist("xyz",flv)
#' resultflvector <- stringdist("xyz",flv,method="lv",caseFLag=1)
#' resultflvector <- stringdist("xyz",flv,method="hamming",vlength=4)
#' resultflmatrix <- stringdist(flv,flv,method="jw",p=1)
#' resultflvector <- stringdist("xyz",flv,method="jw")
#' @export
setGeneric("stringdist", function(a,b,method="dl",caseFlag=0,p=0,vlength=3,...)
    standardGeneric("stringdist"))

setMethod("stringdist",
          signature(a="character",
            b="character"),
          function(a,b,method="osa",...)
          stringdist::stringdist(a, b,method,...)
          )

setMethod("stringdist",
          signature(a="ANY",
            b="ANY"),
          function(a,b,method="dl",caseFlag=0,p=0,
                  vlength=3,...)
          {
            FLStringDistFunctionsClassCheck(a,b)

            if(!(method %in% c("lv","dl","hamming","jaccard","jw")))
            stop(" method not supported ")
            if(method=="lv")
            return(FLStringDist("FLLevenshteinDist",
                      a,b,caseFlag))
            else if(method=="dl")
            return(FLStringDist("FLDLevenshteinDist",
                      a,b,caseFlag))
            else if(method=="hamming")
            return(FLStringDist("FLHammingDist",
                      a,b,caseFlag,vlength=vlength))
            else if(method=="jaccard")
            return(FLStringDist("FLJaccardIndex",
                      a,b,caseFlag))
            else if(method=="jw")
            {
              if(p==0)
              return(FLStringDist("FLJaroDist",
                      a,b,caseFlag))
              else
              return(FLStringDist("FLJaroWinklerDist",
                      a,b,caseFlag))
            }
          })

#' FLNeedleManWunschDist
#'
#' compute NeedleManWunsch distance between strings
#' @param a character or FLVector of characters
#' @param b character or FLVector of characters
#' @param matchWeight integer weight for having
#' matching sequential characters between
#' the strings
#' @param mismatchWeight integer Weight
#' for having nonmatching or non-sequential characters
#' between the strings
#' @param gapPenalty integer penality for gaps
#' @param caseFLag logical or 0/1 indicating 
#' if comparision should be case sensitive
#' @return FLMatrix if both \code{a} and \code{b}
#' are FLVectors. Otherwise returns a FLVector
#' @section Constraints:
#' row vectors are not supported currently
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- FLNeedleManWunschDist("xyz",flv)
#' resultflvector <- FLNeedleManWunschDist("xyz",flv,method="lv",caseFLag=1)
#' resultflvector <- FLNeedleManWunschDist("xyz",flv,method="hamming",vlength=4)
#' resultflmatrix <- FLNeedleManWunschDist(flv,flv,method="jw",p=1)
#' resultflvector <- FLNeedleManWunschDist("xyz",flv,method="jw")
#' @export
setGeneric("FLNeedleManWunschDist", function(a,b,matchWeight=1,
                                          mismatchWeight=-1,
                                          gapPenalty=-1,
                                          caseFlag=0,...)
    standardGeneric("FLNeedleManWunschDist"))

setMethod("FLNeedleManWunschDist",
          signature(a="character",
            b="character"),
          function(a,b,matchWeight=1,mismatchWeight=-1,gapPenalty=-1,caseFlag=0,...)
          FLStringDist("FLNeedleManWunschDist",a,b,matchWeight=matchWeight,
                      mismatchWeight=mismatchWeight,
                      gapPenalty=gapPenalty,
                      caseFlag=caseFlag))

setMethod("FLNeedleManWunschDist",
          signature(a="ANY",
            b="ANY"),
          function(a,b,matchWeight=1,mismatchWeight=-1,gapPenalty=-1,caseFlag=0,...)
          {
            FLStringDistFunctionsClassCheck(a,b)
            FLStringDist("FLNeedleManWunschDist",a,b,matchWeight=matchWeight,
                      mismatchWeight=mismatchWeight,
                      gapPenalty=gapPenalty,
                      caseFlag=caseFlag)
          })

FLStringDistFunctionsClassCheck <- function(a,b,...)
{
  if(!(class(a)=="FLVector" || is.character(a)))
  stop(" a should be FLVector or character ")
  if(!(class(b)=="FLVector" || is.character(b)))
  stop(" a should be FLVector or character ")
}