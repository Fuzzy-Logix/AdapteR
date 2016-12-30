Renv <- new.env(parent = globalenv())
## a 2-dimensional example with distinct clusters
Renv$x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                 matrix(rnorm(100, mean = 10, sd = 0.3), ncol = 2))
options(debugSQL=F)

FLenv <- as.FL(Renv)

test_that("kmeans: square sums and cluster size",{
    eval_expect_equal({
        cl <- kmeans(x, 2)
        kmeans.totSS <- cl$totss
        kmeans.withinSS <- cl$withinss
        kmeans.tot.withinSS <- cl$tot.withinss
        kmeans.betweenss <- cl$betweenss
        kmeans.size <- cl$size
    },Renv,FLenv,
    ##verbose=T,
    tolerance=1e-5,
    noexpectation=c("cl","property"))
})

test_that("pam: kmedoids isolation, clusinfo, silinfo, idmed",{
    eval_expect_equal({
        cl <- pam(x, 2)
        pam.isolation <- cl$isolation
        pam.clusinfo <- cl$clusinfo
        pam.idmed <- cl$id.med
    },Renv,FLenv,
    noexpectation=c("cl","property"))
})

test_that("pam: kmedoids isolation, clusinfo, silinfo, idmed",{
    eval_expect_equal({
        cl <- pam(x, 2)
        pam.silinfo <- cl$silinfo
    },Renv,FLenv,
    tolerance=1e-3,
    noexpectation=c("cl","property"))
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
    },Renv,FLenv,
    noexpectation=c("cl","property"))
})

test_that("agnes results",{
    eval_expect_equal({
        cl <- agnes(x)
        agnes.order <- cl$order
        agnes.merge <- cl$merge
        agnes.ac <- cl$ac
    },Renv,FLenv,
    noexpectation=c("cl","property"))
})

## Height component differs.
## Height gives distance between merging clusters
## at each stage. Somehow AdapeR height is monotonously increasing.
test_that("agnes results",{
    eval_expect_equal({
        agnes.height <- cl$height
    },Renv,FLenv,
    noexpectation=c("cl","property"))
})
