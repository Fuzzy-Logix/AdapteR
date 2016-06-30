library(testthat)
Renv <- new.env(parent = globalenv())
Renv$data <- votes.repub
rownames(Renv$data) <- 1:nrow(Renv$data)
# Renv$animals <- animals
# colnames(Renv$animals)[colnames(Renv$animals)=="end"] <- "endCol"
# rownames(Renv$animals) <- 1:nrow(Renv$animals)
FLenv <- as.FL(Renv)

## For Loop gives error
#for(vmethod in c("average","complete"))

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

## This dataset fails
 # test_that("agnes gaverage",{
 #  eval_expect_equal({
 #    aa.ga <- agnes(animals, method = "complete")
 #    orderGa <- length(aa.ga$order)
 #    heightGa <- length(aa.ga$height)
 #    mergeGa <- dim(aa.ga$merge)
 #  },Renv,FLenv,
 # noexpectation=c("aa.ga")
 # )
 # FLexpect_equal(dim(FLenv$aa.ga$diss),
 #                as.vector(c(nrow(FLenv$animals),nrow(FLenv$animals))))
 # })
