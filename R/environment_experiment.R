#' @include utilities.R

#' @export
setGeneric("FLexpect_equal",
           function(object,expected,...)
               standardGeneric("FLexpect_equal"))
setMethod("FLexpect_equal",
          signature(object="FLMatrix",expected="ANY"),
          function(object,expected,...)
              testthat::expect_equal(as.matrix(object),
                                     expected,...))
setMethod("FLexpect_equal",
          signature(object="FLMatrix",expected="FLMatrix"),
          function(object,expected,...)
              testthat::expect_equal(as.matrix(object),
                                     as.matrix(expected),...))
setMethod("FLexpect_equal",
          signature(object="ANY",expected="FLMatrix"),
          function(object,expected,...)
              testthat::expect_equal(object,
                                     as.matrix(expected),...))
setMethod("FLexpect_equal",
          signature(object="FLVector",expected="vector"),
          function(object,expected,...)
              testthat::expect_equal(as.vector(object),
                                     expected,...))
setMethod("FLexpect_equal",
          signature(object="FLVector",expected="FLVector"),
          function(object,expected,...)
              testthat::expect_equal(as.vector(object),
                                     as.vector(expected),...))
setMethod("FLexpect_equal",signature(object="list",expected="list"),
          function(object,expected,...)
              llply(names(object),
                    function(i)
                        FLexpect_equal(object[[i]],
                                       expected[[i]],...)))
setMethod("FLexpect_equal",
          signature(object="ANY",expected="ANY"),
          function(object,expected,...)
              testthat::expect_equal(object,
                                     expected,...))

#' @export
expect_eval_equal <- function(initF,FLcomputationF,RcomputationF,benchmark=FALSE,...)
{
  I <- initF(...)
  if(!is.list(I$FL))
  I <- list(FL=list(I$FL),R=list(I$R))
    FLexpect_equal(do.call(FLcomputationF,I$FL),
                 do.call(RcomputationF,I$R),
                 check.attributes=FALSE)
}

#' @export
expect_flequal <- function(a,b,...){
    if(is.list(a))
        for(i in 1:length(a))
            expect_flequal(a[[i]],b[[i]],...)

    FLexpect_equal(a,b,...)
}

initF.FLVector <- function(n,isRowVec=FALSE)
{
  vmaxId <- getMaxVectorId()
  sqlSendUpdate(getOption("connectionFL"),
                      c(paste0("INSERT INTO ",getOption("ResultDatabaseFL"),
                                            ".",getOption("ResultVectorTableFL")," \n ",
                          " SELECT ",vmaxId," AS VECTOR_ID,a.serialval AS VECTOR_INDEX,
                            CAST(RANDOM(0,100) AS FLOAT)AS VECTOR_VALUE  
                          FROM ", getOption("ResultDatabaseFL"),".fzzlserial a 
                          WHERE a.serialval < ",ifelse(isRowVec,2,n+1))))

  table <- FLTable(connection=getOption("connectionFL"),
                 getOption("ResultDatabaseFL"),
                 getOption("ResultVectorTableFL"),
                 "vectorIndexColumn",
                 whereconditions=paste0(getOption("ResultDatabaseFL"),".",
                                  getOption("ResultVectorTableFL"),".vectorIdColumn = ",vmaxId)
                 )

  if(isRowVec)
  flv <- table[1,base::sample(c("vectorValueColumn","vectorIndexColumn"),n,replace=TRUE)]
  else
  flv <- table[1:n,"vectorValueColumn"]
  return(FL=flv)
}

## Increase the value of n to increase the dimensions of FLMatrix returned.
## Returns n*n or n*(n-1) based on isSquare.
#' @export
initF.FLMatrix <- function(n,isSquare=FALSE)
{
  vmaxId <- getMaxMatrixId()
  sqlSendUpdate(getOption("connectionFL"),
                        paste0("INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultMatrixTableFL")," \n ",
                          " SELECT ",vmaxId," AS MATRIX_ID,a.serialval AS ROW_ID,
                            b.serialval AS COL_ID,CAST(random(0,100) AS FLOAT)AS CELL_VAL 
                          FROM ",getOption("ResultDatabaseFL"),".fzzlserial a,",getOption("ResultDatabaseFL"),".fzzlserial b
                          WHERE a.serialval < ",n+1," and b.serialval < ",ifelse(isSquare,n+1,n)))
  flm <- FLMatrix(
      database          = getOption("ResultDatabaseFL"),
      table_name = getOption("ResultMatrixTableFL"),
      matrix_id_value   = vmaxId,
      matrix_id_colname = "Matrix_ID",
      row_id_colname    = "rowIdColumn",
      col_id_colname    = "colIdColumn",
      cell_val_colname  = "valueColumn",
      connection=getOption("connectionFL"))
  return(FL=flm)
}

#' @export
initF.FLTable <- function(rows,cols)
{
  WideTable <- FLTable(connection=getOption("connectionFL"),
                      getOption("ResultDatabaseFL"),
                      "fzzlserial",
                      "serialval",
                      whereconditions=paste0(getOption("ResultDatabaseFL"),".fzzlserial.serialval<100"))
  return(WideTable[1:rows,base::sample(c("randval","serialval"),cols,replace=TRUE)])
}


initF.numeric <- initF.FLVector
initF.data.frame <- initF.FLTable
initF.matrix <- initF.FLMatrix

##' initF.default helps to return a list of list.
##' Can be used for comparing results of R and FL functions which require two objects.



initFgeneric<- function(specs=list(numberattribute =5,featureattribute = TRUE),
                        class = "FLMatrix"){
  #browser()
  if(class%in%c("FLVector","FLMatrix","FLTable")){
    obj<-do.call(paste0("initF.",class),specs)
  }
  else{
    obj<-do.call(paste0("initF.",class),specs)
    if(class == "numeric")
    obj<-do.call("as.vector",list(obj))
    else
    obj<-do.call(paste0("as.",class),list(obj))
  }
  return(obj) 
}

as.Renvironment<-function(FLenv){
  Renv<-new.env()
  for(n in ls(FLenv)){
    objectFL <- get(n,envir = FLenv)
    #browser()
    if(is.FLMatrix(objectFL)){
      obj2<-as.matrix(objectFL)
      assign(n,obj2,envir = Renv)
    }
    else if(is.FLVector(objectFL)){
      obj2<-as.vector(objectFL)
      assign(n,obj2,envir = Renv)
    }
    else if(is.FLTable(objectFL)){
      obj2<-as.data.frame(objectFL)
      assign(n,obj2,envir = Renv)
    }
    else 
    assign(n,objectFL,envir= Renv)
  }
  return(Renv)
}


FL_test_generic<-function(specs=list(list(n=5,isSquare = TRUE),list(n =5,isRowVec = FALSE)),
                          classes = c("FLMatrix","FLVector"),operator = "+"){
    
  FLenv<-new.env()
  #browser()
  lapply(1:length(classes),function(i){
    obj<-initFgeneric(specs[[i]],classes[i])
    x=i
    assign(paste0("a",x),obj,envir = FLenv)
  })
  Renv<-as.Renvironment(FLenv)
  obj1<-do.call(operator,lapply(ls(FLenv),function(x)do.call("$",list(FLenv,paste0(x)))))
  obj2<-do.call(operator,lapply(ls(Renv),function(x)do.call("$",list(Renv,paste0(x)))))
  
  FLexpect_equal(obj1,obj2,check.attributes =FALSE)
}

expect_equal_RMatrix_FLMatrix <- function(a){
    # browser()
    debugOld <- getOption("debugSQL")
    options(debugSQL=FALSE)
    b <- as.FLMatrix(a)
    a <- Matrix(a)
    options(debugSQL=debugOld)
    expect_equal_Matrix(a,b,
                        "cast Matrix equal")

    test_Matrix_Subsetting(a,b,"as.FLMatrix")
}

##' converts FLMatrix to r matrix and checks if
##' recursive identical subsetting results in identical
##' matrices.
##'
##' @param b FLMatrix
##' @author  Gregor Kappler <g.kappler@@gmx.net>
##' @export
test_equal_FLMatrix_RMatrix<- function(b){
    # browser()
    debugOld <- getOption("debugSQL")
    options(debugSQL=FALSE)
    a <- as.matrix(b)
    options(debugSQL=debugOld)
    expect_equal_Matrix(a,b,
                        "cast Matrix equal")

    test_Matrix_Subsetting(a,b,"as.FLMatrix")
}


##' tests matrix subsetting by names and by index recursively.
##' 
##' @param a 
##' @param b 
##' @param desc 
##' @author  Gregor Kappler <g.kappler@@gmx.net>
##' @export
test_Matrix_Subsetting <- function(a,b, desc=""){
    if(nrow(a)<3) return()
    nr <- nrow(a) -2 ##%/% 2
    nc <- ncol(a) -2 ## %/% 2
    rowi <- sample(1:nrow(a),nr)
    coli <- sample(1:ncol(a),nc)
    asel <- a[rowi,coli,drop=FALSE]
    bsel <- b[rowi,coli,drop=FALSE]
    expect_equal_Matrix(asel,bsel,
                        paste0(
                            "subset by index of ",
                            desc))
    ## recursively test!
    test_Matrix_Subsetting(asel,bsel,
                        paste0(
                            "indexed subset of ",
                            desc))
    
    if(!is.null(rownames(a)))
        rowi <- sample(rownames(a),nr)
    if(!is.null(colnames(a)))
        coli <- sample(colnames(a),nc)
    asel <- a[rowi,coli,drop=FALSE]
    bsel <- b[rowi,coli,drop=FALSE]
    expect_equal_Matrix(asel,bsel,
                        paste0(
                            "subset by names of ",
                            desc))
    ## recursively test!
    test_Matrix_Subsetting(asel,bsel,
                        paste0(
                            "named subset of ",
                            desc))
}

##' @export
expect_equal_Matrix <- function(a,b,desc="",debug=TRUE){
    if(debug==TRUE){
        cat("\n-------------- ",desc,"\nR Matrix Object:\n")
        print(a)
        cat("\nFL Matrix Object:\n")
        print(b)
    }
    stripNames <- function(x) {
        if(is.null(x)) return(NULL)
        if(is.numeric(x) & all(x==as.numeric(names(x))))
            x <- NULL
        else 
            names(x) <- NULL
        if(is.list(x)) x <- llply(x,stripNames)
        ##if(is.null(unlist(x))) x <- NULL
        x
    }
    test_that(desc,{
        testthat::expect_equal(dimnames(a),stripNames(dimnames(b)))
        testthat::expect_equal(rownames(a),stripNames(rownames(b)))
        testthat::expect_equal(colnames(a),stripNames(colnames(b)))
        testthat::expect_equal(as.vector(a),as.vector(b))
    })
}

expect_equal_RVector_FLVector <- function(a){
    # browser()
    debugOld <- getOption("debugSQL")
    options(debugSQL=FALSE)
    b <- as.FLVector(a)
    options(debugSQL=debugOld)
    expect_equal_Vector(a,b,
                        "cast Vector equal")

    test_Vector_Subsetting(a,b,"as.FLVector")
    if(!is.null(names(a)))
    test_Vector_Subsetting(a,b,"as.FLVector",index=FALSE)
}

##' tests vector subsetting by names and by index recursively.
##' 
##' @param a 
##' @param b 
##' @param desc 
##' @author  Gregor Kappler <g.kappler@@gmx.net>
##' @export
test_Vector_Subsetting <- function(a,b, desc="",index=TRUE){
    if(length(a)<3) return()
    len <- length(a)-2
    if(index){
      leni <- sample(1:length(a),len)
      cat("index is ... ",leni,"\n")
      asel <- a[leni]
      bsel <- b[leni]
      expect_equal_Vector(asel,bsel,
                          paste0(
                              "subset by index of ",
                              desc))
      ## recursively test!
      test_Vector_Subsetting(asel,bsel,
                            paste0(
                              "indexed subset of ",
                              desc))
    }
    else{
      leni <- sample(names(a),len)
      cat("index is ... ",leni,"\n")
      asel <- a[leni]
      bsel <- b[leni]
      expect_equal_Vector(asel,bsel,
                          paste0(
                              "subset by names of ",
                              desc))
      ## recursively test!
      test_Vector_Subsetting(asel,bsel,
                          paste0(
                              "named subset of ",
                              desc))
    }
}

##' @export
expect_equal_Vector <- function(a,b,desc="",debug=TRUE){
    if(debug==TRUE){
        cat("\n-------------- ",desc,"\nR vector Object:\n")
        print(a)
        cat("\nFLVector Object:\n")
        print(b)
    }
    stripNames <- function(x) {
        if(is.null(x)) return(NULL)
        if(is.numeric(x) & all(x==as.numeric(names(x))))
            x <- NULL
        else 
            names(x) <- NULL
        if(is.list(x)) x <- llply(x,stripNames)
        ##if(is.null(unlist(x))) x <- NULL
        x
    }
    test_that(desc,{
        testthat::expect_equal(names(a),stripNames(names(b)))
        testthat::expect_equal(a,as.vector(b))
    })
}


