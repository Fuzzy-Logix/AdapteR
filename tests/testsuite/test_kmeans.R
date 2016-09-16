## demo("connecting")

############################################################
## First you pull all variable / data setup out of the example:
Renv <- new.env(parent = globalenv())
## a 2-dimensional example
Renv$x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                 matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
Renv$iris <- iris

FLenv <- as.FL(Renv)
## gk @phani:  please make rownames invisible as FLtable column, expose only through rownames()
Renv$iris <- cbind(rownames=1:nrow(Renv$iris),Renv$iris)

test_that("kmeans: results on simulated 2-cluster data with clusters far apart",{
    eval_expect_equal({
        cl <- kmeans(x,2)
        cluster <- cl$cluster
        centers <- cl$centers
        totss <- cl$totss
        withinss <- cl$withinss
        tot.withinss <- cl$tot.withinss
        betweenss <- cl$betweenss
        size <- cl$size
    },Renv,FLenv,
    noexpectation=c("cl"),
    check.attributes=FALSE
  )
})

test_that("Kmeans returns objects correctly",{
    eval_expect_equal({
        (cl <- kmeans(iris[,2:5], 2))
        ## plot(x, col = cl$cluster)
        ## points(cl$centers, col = 1:2, pch = 8, cex = 2)
        clusterDim <- length(cl$cluster)
        centersDim <- dim(cl$centers)
    },Renv,FLenv,
    expectation=c("clusterDim","centersDim"),
    noexpectation="cl")
})


# test_that("Kmeans returns numerical correct results",{
#     eval_expect_equal({
#         (cl <- kmeans(iris[,2:5], 2))
#         ## plot(x, col = cl$cluster)
#         ## points(cl$centers, col = 1:2, pch = 8, cex = 2)
#         mycluster <- cl$cluster
#         mycenters <- cl$centers
#     },Renv,FLenv,
#     expectation=c("mycluster","mycenters"),
#     noexpectation="cl")
# })


# test_that("fitted: on kmeans results returns objects correctly",{
#     eval_expect_equal({
#         ## cluster centers "fitted" to each obs.:
#         fitted.xDim <- dim(fitted(cl));  ## head(fitted.x)
#     },Renv,FLenv)
# })

# test_that("fitted: on kmeans results",{
#     eval_expect_equal({
#         ## cluster centers "fitted" to each obs.:
#         fitted.x <- fitted(cl);  ## head(fitted.x)
#         resid.x <- x - fitted(cl)
#     },Renv,FLenv)
# })


# test_that("kmeans: correct square sums",{
#     ## sum of squares
#     ss <- function(x) sum(scale(x, scale = FALSE)^2)
    
#     ## Equalities : ----------------------------------
#     cbind(cl[c("betweenss", "tot.withinss", "totss")], # the same two columns
#           c(ss(fitted.x), ss(resid.x),    ss(x)))
#     expect_equal(cl$ totss,        ss(x))
#     expect_equal(cl$ tot.withinss, ss(resid.x))
#               ## these three are the same:
#     expect_equal(cl$ betweenss,    ss(fitted.x))
#     expect_equal(cl$ betweenss, cl$totss - cl$tot.withinss)
#               ## and hence also
#     expect_equal(ss(x), ss(fitted.x) + ss(resid.x))
# })
    
# test_that("fitted: trivial one-cluster, (its W.SS == ss(x))",{
#     eval_expect_equal({
#         wiss <- kmeans(x,1)$withinss 
#     },Renv,FLenv)
# })

# ## anany: please go from here:
# ## random starts do help here with too many clusters
# ## (and are often recommended anyway!):
# (cl <- kmeans(x, 5, nstart = 25))
# plot(x, col = cl$cluster)
# points(cl$centers, col = 1:5, pch = 8)


