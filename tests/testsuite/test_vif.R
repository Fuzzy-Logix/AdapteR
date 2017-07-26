require(MASS)
require(clusterGeneration)

set.seed(2)
num.vars<-15
num.obs<-200
cov.mat<-genPositiveDefMat(num.vars,covMethod="unifcorrmat")$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
parms<-runif(num.vars,-10,10)
y<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)

Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
Renv$tbl<-data.frame(y,rand.vars)
colnames(Renv$tbl) <- tolower(colnames(Renv$tbl))

# FLenv <- as.FL(Renv)
FLenv$tbl <- as.FLTable(Renv$tbl,tableName = getOption("TestTempTableName"),temporary=FALSE,drop=TRUE)
## formula 
vform <- as.formula(paste0("y~ ",paste0(names(FLenv$tbl)[3:length(names(FLenv$tbl))], collapse = "+")))
#' VIF values for independent variable.
flmod <- vif(vform,data = FLenv$tbl)
flvif <- flmod$vif
rvif <- vif(lm(vform,data = Renv$tbl))
for(i in names(rvif)){
  FLexpect_equal(flvif[i],rvif[i],tolerance=0.0001)
}
# FLexpect_equal(flmod$vif,rvif)
# FLexpect_equal(flmod$vif[1:5],c(27.73,
#                                    36.89,
#                                    12.56,
#                                    50.73,
#                                    8.35),
#                tolerance = .01)


## for backward with threshold = 5
## Fails for Hadoop as there is no FLVIFBW function yet
## Fails for Aster as different result is returned
flmod <- vif(vform,data = FLenv$tbl, method = "bw", threshold  = 5)

if(is.TD()){
	FLexpect_equal(length(flmod$vif),11)
	FLexpect_equal(length(flmod$select),11)
	FLexpect_equal(flmod$select,c(1, 2,  3,  4,  5,  9, 10, 11, 13, 14, 15))
}
