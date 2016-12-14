## benchmarking Connection(RODBCDBI, RJDBC)
##No. Rows: 50- 4000, ncol = 11


rodbi <- flConnect(odbcSource = "Gandalf", database = "FL_DEMO", platform = "TD",pkg = "dbi")

rjdb <- flConnect("jdbc:teradata://10.200.4.116",
                      "FL_DEMO",
                      "mbondre","fzzlpass",
                      c("F:/terajdbc4.jar",
                         "F:/tdgssconfig.jar"),
                  debug=FALSE)

rodbc <- flConnect(odbcSource = "Gandalf",database = "FL_DEMO",platform = "TD", pkg = "dbc")


Benv <- new.env(parent = globalenv())
Benv$nid <- 50

Benv$odb <- NULL
Benv$jdb <- NULL

vids <- seq(from=50, to=200, by=100)

sapply(vids, function(x){
    whrcond <- paste0("OBSID<",x)
    tym1 <- system.time(widetable  <- FLTable("tblAbaloneWide", "ObsID", whereconditions=whrcond, connection = rodbi))
    tym2 <-system.time(widetable  <- FLTable("tblAbaloneWide", "ObsID", whereconditions=whrcond, connection = rjdb))
    Benv$odb <- rbind(Benv$odb,data.frame(ObsID = x, Time = tym1[[3]]))
    Benv$jdb <- rbind(Benv$jdb, data.frame(ObsID = x, Time = tym2[[3]]))    
})






## as.FLMatrix time note:
