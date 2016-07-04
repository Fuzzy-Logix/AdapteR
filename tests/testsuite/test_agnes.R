library(testthat)
Renv <- new.env(parent = globalenv())

Renv$data <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                 matrix(rnorm(100, mean = 10, sd = 0.3), ncol = 2))

# Renv$votesdat <- votes.repub
# rownames(Renv$votesdat) <- 1:nrow(Renv$votesdat)
# Renv$animalsdat <- animals
# colnames(Renv$animalsdat)[colnames(Renv$animalsdat)=="end"] <- "endCol"
# rownames(Renv$animalsdat) <- 1:nrow(Renv$animalsdat)
FLenv <- as.FL(Renv)

test_that("agnes components dimensions for average method ",{
  eval_expect_equal({
  agn1 <- agnes(data)
  order <- length(agn1$order)
  height <- length(agn1$height)
  merge <- dim(agn1$merge)
  },Renv,FLenv,
  expectation=c("order","height","merge"),
  noexpectation=c("agn1")
  )
  FLexpect_equal(dim(FLenv$agn1$diss),
                c(nrow(FLenv$data),nrow(FLenv$data)))
})

test_that("agnes components dimensions for complete method ",{
  eval_expect_equal({
  agn2 <- agnes(data,method="complete")
  order2 <- length(agn2$order)
  height2 <- length(agn2$height)
  merge2 <- dim(agn2$merge)
  },Renv,FLenv,
  expectation=c("order2","height2","merge2"),
  noexpectation=c("agn2")
  )
  FLexpect_equal(dim(FLenv$agn2$diss),
                c(nrow(FLenv$data),nrow(FLenv$data)))
})

test_that("agnes components dimensions for single method ",{
  eval_expect_equal({
  agn3 <- agnes(data,method="single")
  order3 <- length(agn3$order)
  height3 <- length(agn3$height)
  merge3 <- dim(agn3$merge)
  },Renv,FLenv,
  expectation=c("order3","height3","merge3"),
  noexpectation=c("agn3")
  )
  FLexpect_equal(dim(FLenv$agn3$diss),
                c(nrow(FLenv$data),nrow(FLenv$data)))
})

## No centroid method in R
test_that("check working for centroid method ",{
  FLenv$agn4 <- agnes(FLenv$data,method="centroid")
  FLexpect_equal(length(FLenv$agn4$height),
                nrow(FLenv$data)-1)
  FLexpect_equal(length(FLenv$agn4$order),
                nrow(FLenv$data))
  FLexpect_equal(dim(FLenv$agn4$merge),
                c(nrow(FLenv$data)-1,2))
  FLexpect_equal(dim(FLenv$agn4$diss),
                c(nrow(FLenv$data),nrow(FLenv$data)))
})

# # This dataset fails from jdbc
# test_that("agnes on votes.repub dataset ",{
# eval_expect_equal({
#   aa.ga <- agnes(votesdat)
#   orderGa <- length(aa.ga$order)
#   heightGa <- length(aa.ga$height)
#   mergeGa <- dim(aa.ga$merge)
# },Renv,FLenv,
# noexpectation=c("aa.ga")
# )
# FLexpect_equal(dim(FLenv$aa.ga$diss),
#               as.vector(c(nrow(FLenv$votesdat),
#                 nrow(FLenv$votesdat))))
# })

# # This dataset fails
# test_that("agnes on animals dataset ",{
# eval_expect_equal({
#   aa.ga <- agnes(animalsdat)
#   orderGa <- length(aa.ga$order)
#   heightGa <- length(aa.ga$height)
#   mergeGa <- dim(aa.ga$merge)
# },Renv,FLenv,
# noexpectation=c("aa.ga")
# )
# FLexpect_equal(dim(FLenv$aa.ga$diss),
#               as.vector(c(nrow(FLenv$animalsdat),
#                 nrow(FLenv$animalsdat))))
# })
