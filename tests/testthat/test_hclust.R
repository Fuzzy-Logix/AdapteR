library(testthat)
Renv <- new.env(parent = globalenv())
data <- dist(USArrests) 
FLenv <- as.FL(Renv)


#asana ticket                            https://app.asana.com/0/143316600934101/149556758745186

test_that("hclust",{eval_expect_equal(
  hc <- hclust(data)
  merge <-  dim(hc$merge, method="ave")
  height <- length(hc$height)
  order <- length(hc$order)
  label <- length(hc$labels)
  )Renv,FLenv,
  noexpectations = c("hc")
})

#cutree,rbind functions not developed yet.
# test_that("hclust",{eval_expect_equal(
#   hc1 <- hclust(dist(USArrests)^2, "cen")
#   memb <- cutree(hc1, k = 10)
#   cent <- NULL
#   for(k in 1:10){
#     cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
#   }
#   hc2 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
#   merge1 <-  dim(hc2$merge, method="ave")
#   height1 <- length(hc2$height)
#   order1 <- length(hc2$order)
#   label1 <- length(hc2$labels)
#   )Renv,FLenv,
#   noexpectations = c("hc1","memb","hc2")
# })



