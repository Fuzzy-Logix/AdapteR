
    #benchmarking for solve.
    FL_benchmarking_generic(specs = list(list(n =5,isSquare =TRUE)),classes = c("FLMatrix"),operator = "solve")
    

    #benchmarking for ginv
    FL_benchmarking_generic(specs = list(list(n=5,isSquare = FALSE)),classes = c("FLMatrix"),operator = "ginv")
    

    #benchmarking for dim
    alply(expand.grid(list(n=c(5,50),m = c(6,60),la = c("FLMatrix","FLTable"))),function(des){
        browser()
        prepargs <- function(D){
            if(D %in% "FLTable")
                preparedArg <- list (rows = des$n,cols =des$m)
            else 
                preparedArg <- list (n = des$n,isSquare =TRUE)
            preparedArg   
        }
        specslist<-prepargs(des$la)
        FL_benchmarking_generic(specs = list(specslist),classes = c(paste0(des$la)),operator = "dim")
        })


    #benchmarking for cast functions
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE)),classes = c("FLVector"),operator = "as.FLVector")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "as.FLMatrix")
    

    #benchmarking for cholesky decomposition
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "chol")
    

    ##benchmarking for LU decomposition
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "lu")
    
    #benchmarking for length
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "length")
    FL_benchmarking_generic(specs = list(list(rows=5,cols =4)),classes = c("FLTable"),operator = "length")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE)),classes = c("FLVector"),operator = "length")
    

    #benchmarking for transpose.
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "tr")
    

    #benchmarking for diagnol
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "diag")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=FALSE)),classes = c("FLVector"),operator = "diag")
    

    #benchmarking for subtraction operator.
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","FLMatrix"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLMatrix","numeric"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLVector","numeric"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLVector","matrix"),operator = "-")

    

    ##benchmarking for division operator.
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=6,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix","FLMatrix"),operator = "%/%")


    #benchmarking for crossproduct.
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "%*%")
    

    #benchmarking for addition operator.
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "+")
    


    #benchmarking for / operator.
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "/")
   

    #benchmarking for multiplication operator.
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "*")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "*")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "*")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "*")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "*")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "*")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "*")
    


    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "%%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "%%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "%%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "%%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "%%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "%%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "%%")
    


    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "==")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "==")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "==")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "==")
    

    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=FALSE),list(n=9,isRowVec = FALSE)),classes = c("FLVector","FLVector"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=FALSE),list(n=9,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "-")
    

    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE)),classes = c("FLMatrix"),operator = "t")
    


    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "rowMeans")
    


    FL_benchmarking_generic(specs = list(list(n=8,isSquare=TRUE)),classes = c("FLMatrix"),operator = "rowSums")
    


    FL_benchmarking_generic(specs = list(list(n=8,isSquare=FALSE)),classes = c("FLMatrix"),operator = "colMeans")
   


    FL_benchmarking_generic(specs = list(list(n=8,isSquare=FALSE)),classes = c("FLMatrix"),operator = "colSums")
    


    FL_benchmarking_generic(specs = list(list(n=8,isRowVec=FALSE)),classes = c("FLVector"),operator = "[")
    

#Test for FL Table is showing some sql syntax error in FL wide to deep
#initFgeneric will generate same FL Table and I think correalation would be a constant

    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","FLMatrix"),operator = "cor")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "cor")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=FALSE),list(rows=5,cols= 5)),classes = c("FLVector","FLTable"),operator = "cor")
    FL_benchmarking_generic(specs = list(list(rows=5,cols=5),list(rows=5,cols= 5)),classes = c("FLTable","FLTable"),operator = "cor")
    FL_benchmarking_generic(specs = list(list(rows=5,cols=6),list(n=5,isSquare=TRUE)),classes = c("FLTable","FLMatrix"),operator = "cor")
    FL_benchmarking_generic(specs = list(list(n=6,isRowVec = TRUE),list(n=5,isRowVec=TRUE)),classes = c("FLVector","FLVector"),operator = "cor")

   


    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLMatrix","FLVector"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLMatrix","numeric"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","FLMatrix"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","numeric"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = FALSE)),classes = c("matrix","FLVector"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = FALSE)),classes = c("matrix","numeric"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("matrix","FLMatrix"),operator = "crossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("matrix","matrix"),operator = "crossprod")
    


    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLMatrix","FLVector"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLMatrix","numeric"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","FLMatrix"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","numeric"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = FALSE)),classes = c("matrix","FLVector"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = FALSE)),classes = c("matrix","numeric"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("matrix","FLMatrix"),operator = "tcrossprod")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("matrix","matrix"),operator = "tcrossprod")
    

#objects are not created.Sql problem.

    FL_benchmarking_generic(specs = list(list(n=6,isRowVec = FALSE,type ="character"),list(n=5,isRowVec=FALSE,type = "character")),classes = c("FLVector","FLVector"),operator = "hamming.distance")
    