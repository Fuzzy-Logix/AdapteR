                                        # DB-Lytix Example.
Renv <- new.env(parent = globalenv())
Renv$tbl <- iris
Renv$tbl$Species <- as.numeric(Renv$tbl$Species)
FLenv <- as.FL(Renv)
#' 
#' fliris <- as.FL(rtbl)
#' flirispca <- prcomp(Species~., data = fliris)
Renv$mod <- princomp(Renv$tbl[,-1])
FLenv$mod <- prcomp(Species~.,FLenv$tbl)
eval_expect_equal({
    fit <- prcomp(data = tbl)
},Renv,FLenv,
noexpectation = "fit")

##FLexpect_equal(FLenv$mod$rotation, as.matrix(Renv$mod$loading[1:4,1:4]))

FLexpect_equal(FLenv$mod$sdev, as.numeric(Renv$mod$sdev),tolerance = .05)
