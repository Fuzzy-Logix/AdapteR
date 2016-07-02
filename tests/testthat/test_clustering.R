Renv <- new.env(parent = globalenv())
## a 2-dimensional example with distinct clusters
Renv$x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                 matrix(rnorm(100, mean = 10, sd = 0.3), ncol = 2))
options(debugSQL=FALSE)

FLenv <- as.FL(Renv)

test_that("Kmeans compare R and FL Manually ",{
    eval_expect_equal({
        cl <- kmeans(x, 2)
        for(property in c("totss","withinss","tot.withinss","betweenss","size")){
          cat(property," : \n ")
          print(do.call("$",list(cl,property)))
        }
        cat(" \n \n ")
    },Renv,FLenv,
    noexpectation=c("cl","property"))
})

## Objective component may differ in R and FL as:
## DBLytix TotalCost is the average;R cost is absolute.
## The difference is not AdapteR specific as result is fetched
## from fzzlkmedoidstotalCost table.
## The main idea is the same,i.e to see improvement from build to swap.
test_that("kmedoids compare R and FL Manually ",{
    eval_expect_equal({
        cl <- pam(x, 2)
        for(property in c("objective","isolation","clusinfo","silinfo","id.med")){
          cat(property," : \n ")
          print(do.call("$",list(cl,property)))
        }
        cat(" \n \n ")
    },Renv,FLenv,
    noexpectation=c("cl","property"))
})

## In convergence component, NA returned in iterations
## as that info is not available in DB-Lytix.
test_that("FKMeans compare R and FL Manually ",{
    eval_expect_equal({
        cl <- fanny(x, 2)
        for(property in c("k.crisp","objective","coeff","silinfo","convergence")){
          cat(property," : \n ")
          print(do.call("$",list(cl,property)))
        }
        cat(" \n \n ")
    },Renv,FLenv,
    noexpectation=c("cl","property"))
})

## Height component differs.
## Height gives distance between merging clusters
## at each stage. Somehow AdapeR height is monotonously increasing.
test_that("Agnes compare R and FL Manually ",{
    eval_expect_equal({
        cl <- agnes(x)
        for(property in c("order","merge","ac","height")){
          cat(property," : \n ")
          print(do.call("$",list(cl,property)))
        }
        cat(" \n \n ")
    },Renv,FLenv,
    noexpectation=c("cl","property"))
})
