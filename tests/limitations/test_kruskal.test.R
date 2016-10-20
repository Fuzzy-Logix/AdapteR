Renv = new.env(parent = globalenv())
Renv$x <- c(2.9, 3.0, 2.5, 2.6, 3.2,3.8, 2.7, 4.0, 2.4,
            2.8, 3.4, 3.7, 2.2, 2.0)
Renv$g <- factor(rep(1:3, c(5, 4, 5)),
                labels = c("Normal subjects",
                            "Subjects with obstructive airway disease",
                            "Subjects with asbestosis"))
Renv$flv <- c(1:3,62:64)
FLenv <- as.FL(Renv)
Renv$data <- data.frame(Ozone=airquality$Ozone,
                        MonthCol=airquality$Month)
FLenv$data <- as.FLTable(Renv$data,
                        tableName="ARBaseTestTempTable",
                        drop=TRUE)



## different results
FLenv$fit <- kruskal.test(Ozone ~ MonthCol, data = FLenv$data)
Renv$fit <- stats::kruskal.test(Ozone ~ MonthCol, data = Renv$data)

test_that("kruskal Test on FLTable: R example: checking Result Equality without data.name:",{
    result = eval_expect_equal({
            # fit <- kruskal.test(Ozone ~ MonthCol, data = data)
            # fit$p.value <- NULL
            fit$data.name <- NULL
            class(fit) <- "list"
    },Renv,FLenv,
    expectation=c("fit"))
})

## R default function not called transparently
test_that("kruskal Test on FLTable using subset: R example: checking Result Equality with subset:",{
    result = eval_expect_equal({
            fit <- kruskal.test(Ozone ~ MonthCol, data = data,subset=flv)
            # fit$p.value <- NULL
            fit$data.name <- NULL
            class(fit) <- "list"
    },Renv,FLenv,
    expectation=c("fit"))
})