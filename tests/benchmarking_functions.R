FL_benchmarking_generic(specs = list(list(n =5,isSquare =TRUE)),classes = c("FLMatrix"),operator = "solve")


    FL_benchmarking_generic(specs = list(list(n =5,isSquare =TRUE)),classes = c("FLMatrix"),operator = "solve")
    


    FL_benchmarking_generic(specs = list(list(n=5,isSquare = FALSE)),classes = c("FLMatrix"),operator = "ginv")
    


    FL_benchmarking_generic(specs = list(list(n=5,isSquare = TRUE)),classes = c("FLMatrix"),operator = "dim")
    FL_benchmarking_generic(specs = list(list(5,6)),classes = c("FLTable"),operator = "dim")
    
#as.FLMatrix is not defined for FLMatrix signature and as.FLVector needs vconnection for FLVector.
#This test case is not working

    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE)),classes = c("FLVector"),operator = "as.FLVector")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "as.FLMatrix")
    

#Problem in do.call for obj1 in FL_test_generic

    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "chol")
    


    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "lu")
    

#Error for FL Vector test case.

    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "length")
    FL_benchmarking_generic(specs = list(list(rows=5,cols =4)),classes = c("FLTable"),operator = "length")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE)),classes = c("FLVector"),operator = "length")
    


    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE)),classes = c("FLMatrix"),operator = "tr")
    

#Error for Fl Vector test case 

    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "diag")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=FALSE)),classes = c("FLVector"),operator = "diag")
    

#Error for FL vector cases
#May be need to check initF.FLVector

    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","FLMatrix"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLMatrix","numeric"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLVector","numeric"),operator = "-")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLVector","matrix"),operator = "-")

    

#Error in FL vector cases
#Need some modifications to make this generic test to work for more than two objects with a single operator

    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=6,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "%/%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix","FLMatrix"),operator = "%/%")



    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "%*%")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "%*%")
    


    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "+")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "+")
    



    FL_benchmarking_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "/")
    FL_benchmarking_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "/")
   


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
    