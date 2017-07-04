############################################################
## First you pull all variable / data setup out of the example:
Renv <- new.env(parent = globalenv())
## a 2-dimensional example
Renv$x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                 matrix(rnorm(100, mean = 10, sd = 0.3), ncol = 2))
## Renv$iris <- iris
## gk @phani:  please make rownames invisible as FLtable column, expose only through rownames()
## Renv$iris <- cbind(rownames=1:nrow(iris),iris)
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

    kmeans.totss <- a$totss
    kmeans.tot.withinss <- a$tot.withinss
    kmeans.betweenss <- a$betweenss
    kmeans.size <- a$size

  },Renv,FLenv,
  ##verbose=T,
  tolerance=1e-5,
  noexpectation=c("a", "property","temp2"),
  platforms("TD", "Hadoop"))
})


# test_that("Kmeans returns numerical correct results",{
#     eval_expect_equal({
#        cl <- kmeans(iris[,2:5], 2)
#         ## plot(x, col = cl$cluster)
#         ## points(cl$centers, col = 1:2, pch = 8, cex = 2)
#         mycluster <- cl$cluster
#         mycenters <- cl$centers
#     },Renv,FLenv,
#     expectation=c("mycluster","mycenters"),
#     noexpectation="cl")
# })


 test_that("fitted: on kmeans results returns objects correctly",{
     eval_expect_equal({
        cl <- kmeans(x, 2)
        fitted.xDim <- dim(fitted(cl));  ## head(fitted.x)
    },Renv,FLenv,
    noexpectation=c("cl","property"),
    platforms = c("TD", "Hadoop"))
 })


## Checks that fitted function works correctly (This test case can be clubbed into the first one for saving time)
 test_that("fitted: on kmeans results",{
     eval_expect_equal({
        cl <- kmeans(x, 2)
        fitted.x <- as.data.frame(fitted(cl))  ## head(fitted.x)
        resid.x <- as.data.frame(x - fitted(cl))

        rownames(fitted.x) <- NULL
        rownames(resid.x) <- NULL

        temp2 <- cbind(1:100,cl$cluster, fitted.x, resid.x)
        colnames(temp2) <- c("pointno","assignedcluster",3:ncol(temp2))
        temp2 <- as.data.frame(temp2)

        temp <- rowSums(apply(cl$centers,2, function(x) x^2))
        temp <- cbind(temp, cl$centers)
        temp <- temp[order(temp[,1]),]
        temp <- temp[,-1]
        temp <- cbind(as.numeric(rownames(temp)),1:nrow(cl$centers), temp)
        colnames(temp) <- 1:ncol(temp)
        rownames(temp) <- NULL

        temp <- merge(temp2, temp, by.x = "assignedcluster" ,by.y = "1")

        temp <- temp[order(temp[,c("pointno")]),]
        temp <- temp[,-1]

     },Renv,FLenv,
     tolerance=1e-5,
     noexpectation=c("cl","temp2", "fitted.x", "resid.x"),
     platforms = c("TD", "Hadoop"))
 })


if(!is.TDAster()){

test_that("kmeans: correct square sums",{
     ## sum of squares
    cl <- kmeans(FLenv$x, 2)
    fitted.x <- fitted(cl)  ## head(fitted.x)
    resid.x <- FLenv$x - fitted(cl)

    ss <- function(y) sum(scale(y, scale = FALSE)^2)

    expect_equal(cl$totss, ss(as.R(FLenv$x)), tolerance = 0.001)
    expect_equal(cl$tot.withinss, ss(as.R(resid.x)), tolerance = 0.001)
    expect_equal(cl$betweenss,    ss(as.R(fitted.x)), tolerance = 0.001)
    expect_equal(cl$betweenss, cl$totss - cl$tot.withinss, tolerance = 0.001)
    expect_equal(ss(as.R(FLenv$x)), ss(as.R(fitted.x)) + ss(as.R(resid.x)), tolerance = 0.001)
})
    
}
test_that("fitted: trivial one-cluster, (its W.SS == ss(x))",{
    eval_expect_equal({
         wiss <- kmeans(x,1)$withinss 
    },Renv,FLenv,
    tolerance = 0.001,
    platforms = c("TD", "Hadoop"))
})

# ## anany: please go from here:
# ## random starts do help here with too many clusters
# ## (and are often recommended anyway!):
# (cl <- kmeans(x, 5, nstart = 25))
# plot(x, col = cl$cluster)
# points(cl$centers, col = 1:5, pch = 8)


