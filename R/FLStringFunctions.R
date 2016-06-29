## move to file stringdist.R
setGeneric("FLStringDist", function(functionName,
                                    xsource,
                                    targets,
                                    caseFlag=0,
                                    vlength=3,
                                    matchWeight=1,
                                    mismatchWeight=-1,
                                    gapPenalty=-1,
                                    asMatrix=FALSE,...)
    standardGeneric("FLStringDist"))

## move to file stringdist.R
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
                  gapPenalty=-1,
                  asMatrix=FALSE)
          {
            if(length(xsource)>1 || asMatrix==TRUE)
            {
              xsource <- as.FLVector(xsource)
              return(FLStringDist(functionName=functionName,
                                  xsource=xsource,
                                  targets=targets,
                                  caseFlag=caseFlag,
                                  vlength=vlength,
                                  matchWeight=matchWeight,
                                  mismatchWeight=mismatchWeight,
                                  gapPenalty=gapPenalty,
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

            resultvec <- new("FLVector",
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
            targets="FLVector",
            caseFlag="ANY"),
          function(functionName,
                  xsource,
                  targets,
                  caseFlag=0,
                  vlength=3,
                  matchWeight=1,
                  mismatchWeight=-1,
                  gapPenalty=-1,
                  asMatrix=FALSE)
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

## move to file stringdist.R
##Assumed that function is symmetric wrt two strings
setMethod("FLStringDist",
          signature(functionName="character",
            xsource="FLVector",
            targets="character",
            caseFlag="ANY"),
          function(functionName,
                  xsource,
                  targets,
                  caseFlag=0,
                  vlength=3,
                  matchWeight=1,
                  mismatchWeight=-1,
                  gapPenalty=-1,
                  asMatrix=FALSE)
          {
            if(length(targets)>1 || asMatrix==TRUE)
            {
              targets <- as.FLVector(targets)
              return(FLStringDist(functionName=functionName,
                                  xsource=xsource,
                                  targets=targets,
                                  caseFlag=caseFlag,
                                  vlength=vlength,
                                  matchWeight=matchWeight,
                                  mismatchWeight=mismatchWeight,
                                  gapPenalty=gapPenalty,
                                  asMatrix=asMatrix))
            }
            else if(length(targets)==1)
            FLStringDist(functionName=functionName,
                                  xsource=targets,
                                  targets=xsource,
                                  caseFlag=caseFlag,
                                  vlength=vlength,
                                  matchWeight=matchWeight,
                                  mismatchWeight=mismatchWeight,
                                  gapPenalty=gapPenalty,
                                  asMatrix=asMatrix)
          })

## move to file stringdist.R
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

## move to file vwr.R
#' levenshtein.damerau.distance
#'
#' computes the levenshtein-damerau distance between strings.
#'
#' The DB Lytix function called is FLDLevenshteinDist.
#' This function computes the levenshtein-damerau distance between the 
#' two char string arguments (the minimal number of insertions, deletions 
#' replacements,or transpositions required to transform one string into the other).
#' 
#' @seealso \code{\link[vwr]{levenshtein.damerau.distance}} for R function reference
#' implementation.
#'
#' @param xsource character or FLVector of characters
#' @param targets character or FLVector of characters
#' @param caseFLag logical or 0/1 indicating 
#' if comparision should be case sensitive
#' @return FLVector if any \code{xsource} or \code{targets}
#' is R character of length 1. Otherwise returns a FLMatrix.
#' @section Constraints:
#' row vectors are not supported currently.
#' Output is slightly different from vwr::levenshtein.damerau.distance.
#' Refer to \code{@return} section.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- levenshtein.damerau.distance("xyz",flv)
#' resultflmatrix <- levenshtein.damerau.distance(flv,flv,caseFLag=1)
#' resultflmatrix <- levenshtein.damerau.distance(flv,c("xyz","bghy"),caseFLag=1)
#' @export
setGeneric("levenshtein.damerau.distance", function(xsource,targets,caseFlag=0,...)
    standardGeneric("levenshtein.damerau.distance"))

setMethod("levenshtein.damerau.distance",
          signature(xsource="character",
            targets="character"),
          function(xsource,targets,caseFlag=0)
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

## move to file vwr.R
#' levenshtein.distance
#'
#' computes the levenshtein distance between strings.
#' 
#' The DB Lytix function called is FLLevenshteinDist.
#' This function computes the levenshtein distance between the 
#' first two char string arguments(the minimal number of insertions, deletions  
#' or replacements required to transform one string into the other). 
#'
#' @seealso \code{\link[vwr]{levenshtein.distance}} for R function reference
#' implementation.
#'
#' @param xsource character or FLVector of characters
#' @param targets character or FLVector of characters
#' @param caseFLag logical or 0/1 indicating 
#' if comparision should be case sensitive
#' @return FLVector if any \code{xsource} or \code{targets}
#' is R character of length 1. Otherwise returns a FLMatrix.
#' @section Constraints:
#' row vectors are not supported currently.
#' Output is slightly different from vwr::levenshtein.distance.
#' Refer to \code{@return} section.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- levenshtein.distance("xyz",flv)
#' resultflmatrix <- levenshtein.distance(flv,flv,caseFLag=1)
#' resultflmatrix <- levenshtein.distance(flv,c("xyz","poli"),caseFLag=1)
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

## move to file vwr.R
#' hamming.distance
#'
#' computes the hamming distance between strings.
#'
#' The DB Lytix function called is FLHammingDist.
#' This function computes the hamming distance between the 
#' two char string arguments(the number of non-overlapping characters). 
#' The DB Lytix function called is FLHammingDist.
#'
#' @seealso \code{\link[vwr]{hamming.distance}} for R function reference
#' implementation.
#'
#' @param xsource character or FLVector of characters
#' @param targets character or FLVector of characters
#' @param caseFLag logical or 0/1 indicating
#' if comparision should be case sensitive
#' @param vlength optional, length of strings to compare
#' @return FLVector if any \code{xsource} or \code{targets}
#' is R character of length 1. Otherwise returns a FLMatrix.
#' @section Constraints:
#' row vectors are not supported currently.
#' Output is slightly different from vwr::hamming.
#' Refer to \code{@return} section.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- hamming.distance("xyz",flv)
#' resultflmatrix <- hamming.distance(flv,flv,caseFLag=1)
#' resultflmatrix <- hamming.distance(flv,c("xyz","poli"),caseFLag=1)
#' @export
setGeneric("hamming.distance", function(xsource,targets,caseFlag=0,vlength=3,...)
    standardGeneric("hamming.distance"))

setMethod("hamming.distance",
          signature(xsource="character",
            targets="character"),
          function(xsource,targets,caseFlag,vlength=3,...)
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
#' integer weights for matching sequential, nonmatching non-sequential characters between the strings,
#' and integer penality for gaps (ideally negative).
#' @param caseFLag logical or 0/1 indicating
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
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- stringdist("xyz",flv)
#' resultflvector <- stringdist("xyz",flv,method="lv",caseFLag=1)
#' resultflvector <- stringdist("xyz",flv,method="hamming",vlength=4)
#' resultflmatrix <- stringdist(flv,flv,method="jw",p=1)
#' resultflmatrix <- stringdist(c("xyz","poli"),flv,method="jw")
#' @export
setGeneric("stringdist", function(a,b,
                                  method=c("lv","dl","hamming","jaccard","jw","nmw"),
                                  weight=c(match=1, mismatch=1, gap=-1),
                                  caseFlag=0,
                                  p=0,
                                  vlength=3,
                                  ...)
    standardGeneric("stringdist"))

## move to file stringdist.R
setMethod("stringdist",
          signature(a="character",
                    b="character"),
          function(a,b,method="osa",...)
          stringdist::stringdist(a, b,method,...)
          )

## move to file stringdist.R
setMethod("stringdist",
          signature(a="ANY", b="ANY"),
          function(a,b,
                   method=c("lv","dl","hamming","jaccard","jw","nmw"),
                   weight=c(match=1, mismatch=1, gap=-1),
                   caseFlag=0,
                   p=0,
                   vlength=3,
                   ...){
            method <- match.arg(method)
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
            else if(method=="jw"){
              if(p==0)
                return(FLStringDist("FLJaroDist",
                                    a,b,caseFlag))
              else
              return(FLStringDist("FLJaroWinklerDist",
                      a,b,caseFlag))
            }
          })

## move to file FLStringFunctions.R
#' FLNeedleManWunschDist
#'
#' compute NeedleManWunsch distance between strings.
#'
#' The DB Lytix function called is FLNeedlemanWunschDist.
#' This function performs global sequence alignment between two
#' sequences and finds out structural and functional similarity between them
#' and returns a score which indicates the best alignment between two  
#' sequences by searching the highest scores in the similarity matrix.
#'
#' @seealso \code{\link[Biostrings]{pairwiseAlignment}} for R function 
#' reference implementation in BioConductor.
#' 
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
#' @return FLVector if any \code{a} or \code{b}
#' is R character of length 1. Otherwise returns a FLMatrix.
#' @section Constraints:
#' row vectors are not supported currently.
#' Refer to \code{@return} section.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflvector <- FLNeedleManWunschDist("xyz",flv)
#' resultflvector <- FLNeedleManWunschDist("xyz",flv,method="lv",caseFLag=1)
#' resultflvector <- FLNeedleManWunschDist("xyz",flv,method="hamming",vlength=4)
#' resultflmatrix <- FLNeedleManWunschDist(flv,flv,method="jw",p=1)
#' resultflmatrix <- FLNeedleManWunschDist(c("xyz","juio"),flv,method="jw")
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
                return(FLStringDist("FLJaroWinklerDist",
                                    a,b,caseFlag))
            } else if(method=="nmw")
              return(FLStringDist("FLNeedleManWunschDist",
                                  a,b,
                                  matchWeight=weight$match,
                                  mismatchWeight=weight$mismatch,
                                  gapPenalty=weight$gap,...))

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
#' @param caseFLag logical or 0/1 indicating
#' if comparision should be case sensitive
#' @param p penality factor for jaro-winkler
#' if p==0 jaro distance is computed
#' @param vlength optional, length of strings to compare
#' used for hamming
#' @return FLMatrix of string distances
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' flv <- widetable[1:10,"Species"]
#' resultflmatrix <- stringdistmatrix("xyz",flv)
#' resultflmatrix <- stringdistmatrix(c("xyz","abc"),flv,method="lv",caseFLag=1)
#' resultflmatrix <- stringdistmatrix("xyz",flv,method="hamming",vlength=4)
#' resultflmatrix <- stringdistmatrix(flv,flv,method="jw",p=1)
#' resultflmatrix <- stringdistmatrix(flv,c("xyz","abc"),method="jw")
#' @export
setGeneric("stringdistmatrix", function(a,b,method="dl",caseFlag=0,p=0,vlength=3,asMatrix=TRUE,...)
    standardGeneric("stringdistmatrix"))

## move to file stringdist.R
setMethod("stringdistmatrix",
          signature(a="character",
            b="character"),
          function(a,b,method="osa",caseFlag=0,p=0,vlength=3,asMatrix=TRUE,...)
          stringdist::stringdistmatrix(a, b,method,...)
          )

## move to file stringdist.R
setMethod("stringdistmatrix",
          signature(a="ANY",
            b="ANY"),
          function(a,b,method="dl",caseFlag=0,p=0,
                  vlength=3,asMatrix=TRUE,...)
          {
            FLStringDistFunctionsClassCheck(a,b)

            if(!(method %in% c("lv","dl","hamming","jaccard","jw")))
            stop(" method not supported ")
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
          })

## move to file stringdist.R
FLStringDistFunctionsClassCheck <- function(a,b,...)
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
                  stringpos=1,...)
    standardGeneric("FLStrCommon"))

setMethod("FLStrCommon",
        signature(object="FLVector"),
        function(functionName,
                object,
                delimiter="",
                stringpos=1)
        {
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
                              functionName,"(",a,".vectorValueColumn,",fquote(delimiter),",",stringpos,") AS vectorValueColumn ",
                             " FROM(",constructSelect(object),") AS ",a)
          }
          else if(functionName=="FLInstr")
          {
            vfun <- paste0(functionName,"(0,",a,".vectorValueColumn,",fquote(delimiter),")")
            sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                             a,".vectorIndexColumn AS vectorIndexColumn,",
                              "CASE WHEN ",vfun,"= -1 THEN -1 ELSE ",vfun," + 1 END AS vectorValueColumn ",
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

            resultvec <- new("FLVector",
                            select = tblfunqueryobj,
                            dimnames = list(object@dimnames[[1]],
                                          "vectorValueColumn"),
                            isDeep = FALSE)
            return(resultvec)
          })


##paste0 is already working for FLVectors,but fetches data.
## only single char as delimiter used in DB-Lytix!

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
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLConcatString(flv,",")
#' @export
setGeneric("FLConcatString",function(object,delimiter)
    standardGeneric("FLConcatString"))

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
                                  "FLConcatString(",b,".vectorValueColumn,",fquote(delimiter),") AS vectorValueColumn ",
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

            resultvec <- new("FLVector",
                            select = tblfunqueryobj,
                            dimnames = list(1,
                                          "vectorValueColumn"),
                            isDeep = FALSE)
            return(resultvec)
            })

## move to file FL StringFunctions.R
#' Clean string
#'
#' Remove non-printable characters from each
#' element of FLVector of strings.
#'
#' The DB lytix function called is FLCleanStr.
#' The clean string function is a scaler that removes all non-printable
#' characters from a string(vector of characters) and outputs a formatted string.
#'
#' ######@seealso \code{\link[base]{paste0}} for R function reference
#' ######implementation.
#'
#' @param object FLVector of characters
#' @return a clean FLVector 
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLCleanStr(flv)
#' @export
setGeneric("FLCleanStr",function(object)
    standardGeneric("FLCleanStr"))

setMethod("FLCleanStr",
          signature(object="FLVector"),
          function(object){
            return(FLStrCommon(functionName="FLCleanStr",
                            object=object))
            })

#' Check if HexaDecimal
#'
#' Check if an element is hexadecimal number
#' @param object FLVector
#' @return FLVector with 1 for TRUE and 0 for FALSE
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLIsHex(flv)
#' @export
setGeneric("FLIsHex",function(object)
    standardGeneric("FLIsHex"))

setMethod("FLIsHex",
          signature(object="FLVector"),
          function(object){
            return(FLStrCommon(functionName="FLIsHex",
                            object=object))
            })

#' Check if Numeric
#'
#' Check if an element is numeric
#' @param object FLVector
#' @return FLVector with 1 for TRUE and 0 for FALSE
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLIsNumeric(flv)
#' @export
setGeneric("FLIsNumeric",function(object)
    standardGeneric("FLIsNumeric"))

setMethod("FLIsNumeric",
          signature(object="FLVector"),
          function(object){
            return(FLStrCommon(functionName="FLIsNumeric",
                            object=object))
            })

#' Remove extra spaces in strings
#'
#' Removes extra spaces from elements
#' @param object FLVector of characters
#' @return FLVector with extra spaces removed
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLSqueezeSpace(flv)
#' @export
setGeneric("FLSqueezeSpace",function(object)
    standardGeneric("FLSqueezeSpace"))

setMethod("FLSqueezeSpace",
          signature(object="FLVector"),
          function(object){
            return(FLStrCommon(functionName="FLSqueezeSpace",
                            object=object))
            })
## No point in overloading strsplit because list
## output is not possible and only single char taken from
## delimiter in DB-Lytix

#' Extract parts of strings
#'
#' Extract sub-strings separated by a
#' delimiter and identified by their position
#' @param object FLVector of characters
#' @param delimiter character
#' @param stringpos identifier to reference the
#' sub-string given by its position
#' @return FLVector
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- FLExtractStr(flv,"A",1)
#' @export
setGeneric("FLExtractStr",function(object,delimiter,stringpos)
    standardGeneric("FLExtractStr"))

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

#' Pattern Matching
#'
#' Match \code{patern} in each element of \code{text}
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
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- regexpr("A",flv)
#' @export
setGeneric("regexpr", function(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)
    standardGeneric("regexpr"))

setMethod("regexpr",
          signature(
            text="FLVector"),
          function(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)
          {
            if(is.null(pattern)||is.na(pattern)||length(pattern)==0)
            pattern <- "NULL"
            return(FLStrCommon("FLInstr",
                      text,delimiter=pattern))
          })

setMethod("regexpr",
          signature(
            text="ANY"),
          function(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)
          base::regexpr(pattern,text,ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes)
          )

#' Pattern Matching
#'
#' Match \code{patern} in each element of \code{text}
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
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' resultflvector <- gregexpr("A",flv)
#' @export
setGeneric("gregexpr", function(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)
    standardGeneric("gregexpr"))

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

#' Pattern Matching
#'
#' Match \code{patern} in each element of \code{x}
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
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
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

            resultvec <- new("FLVector",
                            select = tblfunqueryobj,
                            dimnames = list(1:vlength,
                                          "vectorValueColumn"),
                            isDeep = FALSE)
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

#' Pattern Matching
#'
#' Match \code{patern} in each element of \code{x}
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
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' flvector <- grepl("A",flv)
#' @export
setGeneric("grepl", function(pattern, x, ignore.case = FALSE, perl = FALSE,
      fixed = FALSE, useBytes = FALSE)
    standardGeneric("grepl"))

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
                                    "CASE WHEN ",b,".vectorValueColumn <> -1 THEN 1 ELSE 0 END AS vectorValueColumn",
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

            resultvec <- new("FLVector",
                            select = tblfunqueryobj,
                            dimnames = list(object@dimnames[[1]],
                                          "vectorValueColumn"),
                            isDeep = FALSE)
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

#' Pattern Matching and Replacement
#'
#' Replace \code{patern} in each element of \code{x}
#' with \code{replacement}
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
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' flvector <- sub("A","X",flv)
#' @export
setGeneric("sub", function(pattern,replacement,x, 
                    ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE)
    standardGeneric("sub"))

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

#' Pattern Matching and Replacement
#'
#' Replace \code{patern} in each element of \code{x}
#' with \code{replacement}
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
#' widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
#' flv <- widetable[1:6,"string"]
#' flvector <- gsub("A","X",flv)
#' @export
setGeneric("gsub", function(pattern,replacement,x, 
                    ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE)
    standardGeneric("gsub"))

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
#' Parse XML files
#'
#' Parse XML files stored in-database
#' as elements of FLVector.
#' @param object FLVector of characters
#' @return dataframe with parsed XML
#' @section Constraints:
#' row vectors are not supported currently.
#' @examples 
#' wtd <- FLTable("FL_DEMO","tblXMLTest","GroupID")
#' flv <- wtd[,"pXML"]
#' resultdataframe <- FLParseXML(flv)
#' @export
setGeneric("FLParseXML", function(object)
    standardGeneric("FLParseXML"))

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
