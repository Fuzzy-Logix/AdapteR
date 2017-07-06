Renv <- new.env(parent = globalenv())

Renv$x <- rbind(matrix(rnorm(10, sd = 0.3), ncol = 2),
                   matrix(rnorm(10, mean = 10, sd = 0.3), ncol = 2))

n <- 10;
Renv$y <- matrix(rnorm(2,mean = 100, sd = 0.01), ncol = 2)
for(i in 2:n){
  Renv$y <- rbind(matrix(rnorm(2,mean = 100/i, sd = 0.01), ncol = 2), Renv$y)
}

FLenv <- as.FL(Renv)

## height cannot be compared since cluster package and db-lytix address
## height differently. Height gives distance between merging clusters
## at each stage. However, Adapter gives difference from reference.

test_that("agnes components merge order for average method ",{
  eval_expect_equal({
      agn1 <- agnes(y)
      o <- agn1$order
      h <- sort(agn1$height)
      m <- agn1$merge
  },Renv,FLenv,
  expectation=c("o","h","m"),
  noexpectation=c("agn1"),
  platforms = c("TD", "Hadoop")
  )
  FLexpect_equal(dim(FLenv$agn1$diss),
                c(nrow(FLenv$x),nrow(FLenv$x)))
})

##
test_that("agnes components dimensions for average method ",{
  eval_expect_equal({
      agn1 <- agnes(x)
      o <- length(agn1$order)
      h <- length(agn1$height)
      m <- dim(agn1$merge)
  },Renv,FLenv,
  expectation=c("o","h","m"),
  noexpectation=c("agn1")
  )
  FLexpect_equal(dim(FLenv$agn1$diss),
                c(nrow(FLenv$x),nrow(FLenv$x)))
})

##
test_that("agnes components dimensions for complete method ",{
  eval_expect_equal({
  agn2 <- agnes(x,method="complete")
  order2 <- length(agn2$order)
  height2 <- length(agn2$height)
  merge2 <- dim(agn2$merge)
  },Renv,FLenv,
  expectation=c("order2","height2","merge2"),
  noexpectation=c("agn2")
  )
  FLexpect_equal(dim(FLenv$agn2$diss),
                c(nrow(FLenv$x),nrow(FLenv$x)))
})

##
test_that("agnes components dimensions for single method ",{
  eval_expect_equal({
  agn3 <- agnes(x,method="single")
  order3 <- length(agn3$order)
  height3 <- length(agn3$height)
  merge3 <- dim(agn3$merge)
  },Renv,FLenv,
  expectation=c("order3","height3","merge3"),
  noexpectation=c("agn3")
  )
  FLexpect_equal(dim(FLenv$agn3$diss),
                c(nrow(FLenv$x),nrow(FLenv$x)))
})



## No centroid method in R
#test_that("check working for centroid method ",{
#  FLenv$agn4 <- agnes(FLenv$x,method="centroid")
#  FLexpect_equal(length(FLenv$agn4$height),
#                nrow(FLenv$x)-1)
#  FLexpect_equal(length(FLenv$agn4$order),
#                nrow(FLenv$x))
#  FLexpect_equal(dim(FLenv$agn4$merge),
#                c(nrow(FLenv$x)-1,2))
#  FLexpect_equal(dim(FLenv$agn4$diss),
#                c(nrow(FLenv$x),nrow(FLenv$x)))
#})
