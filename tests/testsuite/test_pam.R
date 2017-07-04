Renv <- new.env(parent = globalenv())
Renv$x <- rbind(cbind(rnorm(10,0,0.5), 
            rnorm(10,0,0.5)),
            cbind(rnorm(10,5,0.5), 
            rnorm(10,5,0.5)),
            cbind(rnorm(10,10,0.5), 
            rnorm(10,10,0.5)),
            cbind(rnorm(10,15,0.5), 
            rnorm(10,15,0.5)))

FLenv <- as.FL(Renv)


test_that("Kmedoids: dimensions of results match",{
  eval_expect_equal({
      pamx <- pam(x,4)
      medoids <- pamx$medoids
      id.med <- pamx$id.med
      clust <- pamx$clustering
      isolate <- pamx$isolation
      clusinfo <- pamx$clusinfo
  },Renv,FLenv,
  expectation=c("medoids","id.med",
                "clust","isolate","clusinfo"),
  noexpectation = c("pamx"),
  check.attributes=FALSE)
})


test_that("Kmedoids: dimensions of silinfo_width match",{
  eval_expect_equal({
      pamx <- pam(x,4)
      
      silinfo_width <- dim(pamx$silinfo$widths)
  },Renv,FLenv,
  expectation=c("medoids","id.med",
                "clust","isolate","clusinfo",
                "silinfo_width"),
  noexpectation = c("pamx"))
  FLexpect_equal(dim(FLenv$pamx$diss),
                 c(nrow(FLenv$x),nrow(FLenv$x)))
})




