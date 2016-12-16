
table2<-FLTable(table= getTestTableName("tblAutoMpg"),
                obs_id_colname = "ObsID")

if(!is.Hadoop()){
    table3<-FLTable(table= getTestTableName("tblUSArrests"),
                    obs_id_colname = "OBSID",
                    var_id_colname = "VarID",
                    cell_val_colname ="Num_Val")
} else {
    table3<-FLTable(table= getTestTableName("tblUSArrests"),
                    obs_id_colname = "OBSID",
                    var_id_colname = "DimID",
                    cell_val_colname ="Value")
}

test_that("check if wide to deep works",{
    deep <- wideToDeep(object = table2,
                       ExcludeCols = "HorsePower,Weight",
                       ClassSpec = list(CarName = "Toyota",Cylinders = "4"),
                       WhereClause="",
                       OutObsIDCol="ObsID",
                       OutVarIDCol="",
                       OutValueCol="out_value")
    dbDrop(deep)
})

test_that("check if deep to wide works",{
    wide <- deepToWide(object = table3,
                 whereconditions="",
                 mapTable="",
                 mapName="",
                 Analysisid = ""
                 )
    head(wide)
    dbDrop(wide)
})
