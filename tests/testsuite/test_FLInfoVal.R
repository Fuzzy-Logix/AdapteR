## DB-Lytix Example Test-Case.
FLenv <- new.env(parent = globalenv())
FLenv$tbl <- FLTable(table = getTestTableName("tblinfoval"), obs_id_colname="BinID")
FLenv$mod <- InfoVal(event = "Events", nonevents = "NonEvents", data = FLenv$tbl,n = 6)

FLexpect_equal(FLenv$mod@results$otable$infoval[[1]],2.9537, tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable$infoval[[2]],1.165, tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable$infoval[[3]],0.0067,tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable$infoval[[4]],1.1902,tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable$infoval[[5]],0.3891,tolerance = .001)
FLexpect_equal(FLenv$mod@results$otable$infoval[[6]],0.2029,tolerance = .001)
FLexpect_equal(nrow(FLenv$mod@results$otable),6)
