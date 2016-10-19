############################################################
## First you pull all variable / data setup out of the example:
Renv <- new.env(parent = globalenv())
Renv$m <- Matrix(c(0,1,0,2),2,sparse=T)
Renv$m <- as(Renv$m,"dgCMatrix")
FLenv <- as.FL(Renv)
FLenv$T1 <- initF.FLTable(rows=5,cols=5)
Renv$T1 <- as.data.frame(FLenv$T1)

## Testing FLDims
test_that("check FLDims if all elements of a row are zero",
{
    eval_expect_equal({
        dimm <- dim(m)
        dimt <- dim(T1)
    },Renv,FLenv)
})
