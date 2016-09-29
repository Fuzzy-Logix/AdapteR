table1<-initF.FLTable(rows = 100,col = 2)


table2<-FLTable(table= "tblAutoMpg",
                obs_id_colname = "ObsID")

table3<-FLTable(table= "tblUSArrests",
                obs_id_colname = "OBSID",
                var_id_colname = "VarID",
                cell_val_colname ="Num_Val")
dropFLTestTable()

test_that("check if wide to deep works",{
    wideToDeep(object = table2,
               excludeCols = "HorsePower,Weight",
               classSpec = list(CarName = "Toyota",Cylinders = "4"),
               whereconditions="",
               outDeepTableName="ARBaseTestTempTable",
               outObsIDCol="ObsID",
               outVarIDCol="",
               outValueCol="out_value")
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
