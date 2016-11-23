
table2<-FLTable(table= "tblAutoMpg",
                obs_id_colname = "ObsID")

table3<-FLTable(table= "tblUSArrests",
                obs_id_colname = "OBSID",
                var_id_colname = "VarID",
                cell_val_colname ="Num_Val")
dropFLTestTable()

test_that("check if wide to deep works",{
    wideToDeep(object = table2,
               ExcludeCols = "HorsePower,Weight",
               ClassSpec = list(CarName = "Toyota",Cylinders = "4"),
               WhereClause="",
               OutDeepTable="ARBaseTestTempTable",
               OutObsIDCol="ObsID",
               OutVarIDCol="",
               OutValueCol="out_value")
})

dropFLTestTable()
test_that("check if deep to wide works",{
    deepToWide(object = table3,
                 whereconditions="",
                 mapTable="",
                 mapName="",
                 outWideTableName="ARBaseTestTempTable",
                 Analysisid = ""
                 )
})
