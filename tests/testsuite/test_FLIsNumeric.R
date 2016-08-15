library(testthat)
Renv <- new.env(parent = globalenv())

Renv$vecChar <- letters[3:1]
Renv$vecInt <- c("1","2")
Renv$vecFloat <- c("1","2.2")

FLenv <- as.FL(Renv)

test_that("Check correct result for FLIsNumeric ",{
        vars <- ls(envir=Renv)
        lapply(vars,function(x){
            FLexpect_equal(FLIsNumeric((get(x,envir=FLenv))),
                !is.na(as.numeric(get(x,envir=Renv))))
            })
       })


test_that("Check for FLIsNumeric function",{
         widetable  <- FLTable("tblAutoMpg","ObsID")
         flv <- widetable[1:6,"CarName"]
         resultflvector <- FLIsNumeric(flv)
       })
