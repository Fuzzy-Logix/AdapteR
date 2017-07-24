## DB-Lytix Example Test-Case.
FLenv <- new.env(parent = globalenv())
FLenv$tbl <- FLTable(table = "tblinfoval", obs_id_colname="BinID")
FLenv$mod <- woe.FLTable(event = "Events", nonevents = "NonEvents", data = FLenv$tbl,n = 5)

FLexpect_equal(FLenv$mod@results$otable$WOE[[1]],-1.7544, tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable$WOE[[2]],0.1961,tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable$WOE[[3]],2.9127,tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable$WOE[[4]],2.6155,tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable$WOE[[5]],2.7994,tolerance = .001)
FLexpect_equal(nrow(FLenv$mod@results$otable),5)

## External Example from the website below.
##http://ucanalytics.com/blogs/information-value-and-weight-of-evidencebanking-case/
FLenv <- new.env(parent = globalenv())
vdf <- data.frame(BinID = 1:4, Events =c(206,357,776,183) , NonEvents = c(4615,9909,32150,12605))
for (i in colnames(vdf)){    
    vdf[[i]] <- as.integer(vdf[[i]]) }

FLenv$tbl <- as.FL(vdf)


FLenv$mod <- woe(event = "Events", nonevents = "NonEvents", data = FLenv$tbl,n = 4)
FLenv$Infoval <- woe(event = "Events", nonevents = "NonEvents", data = FLenv$tbl,n = 4)



## for WOE.
FLexpect_equal(FLenv$mod@results$otable[,2][[1]],0.55303887, tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable[,2][[2]],0.33876692,tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable[,2][[3]],-0.06178536,tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable[,2][[4]],-0.57013283,tolerance = .001)
FLexpect_equal(nrow(FLenv$mod@results$otable),4)



## for InformationValue.
FLexpect_equal(FLenv$Infoval@results$otable[,2][[1]],0.55303887,tolerance = .001)
FLexpect_equal(FLenv$Infoval@results$otable[,2][[2]],0.33876692,tolerance = .001)
FLexpect_equal(FLenv$Infoval@results$otable[,2][[3]],-0.06178536,tolerance = .001)
FLexpect_equal(FLenv$Infoval@results$otable[,2][[4]],-0.57013283,tolerance = .001)
FLexpect_equal(nrow(FLenv$mod@results$otable),4)
