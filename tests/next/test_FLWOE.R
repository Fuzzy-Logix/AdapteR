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
