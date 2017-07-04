Renv <- new.env(parent = globalenv())
## a 2-dimensional example with distinct clusters
Renv$x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                 matrix(rnorm(100, mean = 10, sd = 0.3), ncol = 2))

FLenv <- as.FL(Renv)

## The following test case compares most of the objects returned by kmeans in R and FL environment
test_that("kmeans: Cluster, centers, withinss, totss, tot.withinss, betweenss, size",{
  eval_expect_equal({
    a <- kmeans(x, 2)
         
    temp2 <- cbind(1:100,a$cluster)
    colnames(temp2) <- c("pointno", "assignedcluster")
    temp2 <- as.data.frame(temp2)

    temp <- rowSums(apply(a$centers,2, function(x) x^2))
    temp <- cbind(temp, a$centers, a$withinss)
    temp <- temp[order(temp[,1]),]
    temp <- temp[,-1]
    temp <- cbind(as.numeric(rownames(temp)),1:nrow(a$centers), temp)
    colnames(temp) <- 1:ncol(temp)
    rownames(temp) <- NULL

    temp <- merge(temp2, temp, by.x = "assignedcluster" ,by.y = "1")

    temp <- temp[order(temp[,c("pointno")]),]
    temp <- temp[,-1]

    kmeans.totSS <- a$totss
    kmeans.tot.withinSS <- a$tot.withinss
    kmeans.betweenss <- a$betweenss
    kmeans.size <- a$size

  },Renv,FLenv,
  ##verbose=T,
  tolerance=1e-5,
  noexpectation=c("a","property","temp2"))
})



test_that("pam: kmedoids isolation, clusinfo, idmed, clustering, medoids",{
    eval_expect_equal({
        cl <- pam(x, 2)
        pam.isolation <- cl$isolation
        pam.clusinfo <- cl$clusinfo
        pam.idmed <- cl$id.med
        pam.clustering <- cl$clustering
        pam.medoids <- cl$medoids
    },Renv,FLenv,
    noexpectation=c("cl","property"),
    expectation=c("pam.isolation","pam.clusinfo","pam.idmed", "pam.clustering", "pam.medoids"),
    check.attributes=FALSE)
})

## Objective component may differ in R and FL as:
## DBLytix TotalCost is the average;R cost is absolute.
## The difference is not AdapteR specific as result is fetched
## from fzzlkmedoidstotalCost table.
## The main idea is the same,i.e to see improvement from build to swap.
## test_that("pam: objective",{
##     eval_expect_equal({
##         pam.objective <- cl$objective
##     },Renv,FLenv,
##     noexpectation=c("cl","property"))
## })

## In convergence component, NA returned in iterations
## as that info is not available in DB-Lytix.
test_that("fanny: Fuzzy kmeans results ",{
    eval_expect_equal({
        cl <- fanny(x, 2)
        fanny.k.crisp <- cl$k.crisp
        fanny.objective <- cl$objective
        fanny.coeff <- cl$coeff
        fanny.silinfo <- cl$silinfo
        fanny.convergence <- cl$convergence
        fanny.clustering <- cl$clusteri
    },Renv,FLenv,
    tolerance = 0.1,
    noexpectation=c("cl","property"))
})



## Height component differs.
## Height gives distance between merging clusters
## at each stage. Somehow AdapeR height is monotonously increasing.
test_that("agnes results",{
    eval_expect_equal({
    	cl <- agnes(x)
        agnes.height <- cl$height
    },Renv,FLenv,
    noexpectation=c("cl","property"))
})
