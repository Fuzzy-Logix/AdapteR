# for 2 clusters

Renv <- new.env(parent = globalenv())
Renv$x <- rbind(matrix(rnorm(300, sd = .5), ncol = 2),
                matrix(rnorm(500, mean = 10, sd = .3), ncol = 2))
FLenv <- as.FL(Renv)
FLenv$obj <- fanny(FLenv$x, 2)
Renv$obj <- fanny(Renv$x, 2)

test_that("FLFKmeans compare R and FL",{
    eval_expect_equal({
       # cl <- fanny(x, 2)
# silinfo <- obj$silinfo(Problems with computing as cluster are
# numbered differently)
        objective <- obj$objective
        coeff <- obj$coeff
        membership <- obj$membership
        
        
    },Renv,FLenv,
    expectation = c("objective", "coeff", "membership"),
    noexpectation = c("obj", "x" ),
    tolerance = .01
    )
    
})

# silinfo comparison 

#          Robject
#           cluster neighbor sil_width|             cluster neighbor sil_width
#                                     |  
#  35        1        2 0.9535267     |   35     35      2         1     0.9535267
#  100       1        2 0.9532853     |   100   100      2         1     0.9532853
#  64        1        2 0.9532169     |   64     64      2         1     0.9532169   
#  146       1        2 0.9532139     |   146   146      2         1     0.9532139
#  89        1        2 0.9528938     |   89     89      2         1     0.9528938
#  92        1        2 0.9528883     |   92     92      2         1     0.9528883



# for iris dataset

FLenv$widetable  <- FLTable("iris", "rownames")
Renv$widetable <- as.R(FLenv$widetable)
FLenv$obj2 <- fanny(FLenv$widetable, 3)
Renv$obj2 <- fanny(Renv$widetable, 3)

test_that("FLFKmeans compare R and FL for iris dataset",{
    eval_expect_equal({
       # cl <- fanny(x, 2)
# silinfo <- obj$silinfo(Problems with computing as cluster are
# numbered differently)
        objective2 <- obj2$objective
        coeff2 <- obj2$coeff
        membership2 <- obj2$membership
        
        
    },Renv,FLenv,
    expectation = c("objective2", "coeff2", "membership2"),
    noexpectation = c("obj2", "widetable"),
    tolerance = .01
    )
    
})

# silinfo comparison for iris dataset(for 3 clusters)

#      ObsID ClusID Neighbour  siliwidth             ObsID ClusID Neighbour   siliwidth
#8         1        2         0.72565124        |    8       8      1         2  0.85279508
#1         1        2         0.67132991        |    1       1      1         2  0.85189869
#50        2        1        -0.05471623        |    50      1      2         0.85098665
#100       2        3         0.0417991650      |    100     100    2         3  0.63481551

