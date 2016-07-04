Renv <- new.env(parent = globalenv())

Renv$x <- c(1,1,3:1,1:4,3);
Renv$y <- c(9,9:1);
Renv$z <- c(2,1:9)
Renv$cy <- as.character(Renv$y)
Renv$x1 <- c(5:1, 6:8, 12:9)

FLenv <- as.FL(Renv)
test_that("order",{eval_expect_equal({
                       test1 <- order(x , y , z)
                       test2 <- order(x1)
                                        #test2 <- rbind(x, y, z)[, order(x, -xtfrm(cy), z)]     xtfrm and rbind(all in comments) not for FL
                       test3 <- order(x, cy, z, decreasing = c(FALSE, TRUE, FALSE))
                                        #method="radix")] not supported
                   },Renv,FLenv)
})








