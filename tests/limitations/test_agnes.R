library(testthat)
Renv <- new.env(parent = globalenv())

Renv$votesdat <- votes.repub
rownames(Renv$votesdat) <- 1:nrow(Renv$votesdat)
Renv$animalsdat <- animals
colnames(Renv$animalsdat)[colnames(Renv$animalsdat)=="end"] <- "endCol"
rownames(Renv$animalsdat) <- 1:nrow(Renv$animalsdat)
FLenv <- as.FL(Renv)

# This dataset fails 
test_that("agnes on votes.repub dataset ",{
eval_expect_equal({
  aa.ga <- agnes(votesdat)
  orderGa <- length(aa.ga$order)
  heightGa <- length(aa.ga$height)
  mergeGa <- dim(aa.ga$merge)
},Renv,FLenv,
noexpectation=c("aa.ga")
)
FLexpect_equal(dim(FLenv$aa.ga$diss),
              as.vector(c(nrow(FLenv$votesdat),
                nrow(FLenv$votesdat))))
})

# This dataset fails
test_that("agnes on animals dataset ",{
eval_expect_equal({
  aa.ga <- agnes(animalsdat)
  orderGa <- length(aa.ga$order)
  heightGa <- length(aa.ga$height)
  mergeGa <- dim(aa.ga$merge)
},Renv,FLenv,
noexpectation=c("aa.ga")
)
FLexpect_equal(dim(FLenv$aa.ga$diss),
              as.vector(c(nrow(FLenv$animalsdat),
                nrow(FLenv$animalsdat))))
})
