

wide_table<-FLTable(database = getOption("ResultDatabaseFL"),
                table= "tblAutoMpg",
                obs_id_colname = "ObsID")

deep_table<-FLTable(database = getOption("ResultDatabaseFL"),
                    table= "tblUSArrests",
                    obs_id_colname = "OBSID",
                    var_id_colname = "VarID",
                    cell_val_colname ="Num_Val")

test_that("check for FL Regr Data Prep",FLRegrDataPrep
                    (object = wide_table,
                    depCol="HorsePower",
                    outDeepTableName="tblAutoMpgdeep1",
                    outDeepTableDatabase="fuzzylogix",
                    outObsIDCol="ObsID",
                    outVarIDCol="",
                    outValueCol="",
                    catToDummy=1,
                    performNorm=1,
                    performVarReduc=1,
                    makeDataSparse=1,
                    minStdDev=3,
                    maxCorrel=0.6,
                    trainOrTest=0,
                    excludeCols="Weight",
                    classSpec=list(CarName = "Toyota",Cylinders = "4"),
                    whereconditions="",
                    inAnalysisID=""
                    )
          )

#Some stack usage was upto limit.
#Takes time to get executed.
test_that("check for FL Regr Data Prep",FLRegrDataPrep
                    (object = wide_table,
                    depCol="",
                    outDeepTableName="",
                    outDeepTableDatabase="",
                    outObsIDCol="ObsID",
                    outVarIDCol="",
                    outValueCol="",
                    catToDummy=0,
                    performNorm=0,
                    performVarReduc=1,
                    makeDataSparse=0,
                    minStdDev= -1,
                    maxCorrel= 1.1,
                    trainOrTest=1,
                    excludeCols="",
                    classSpec="",
                    whereconditions="",
                    inAnalysisID="NULL"
                    )
          )

#Takee time to get executed.
test_that("check for FL Regr Data Prep",FLRegrDataPrep
                    (object = deep_table,
                    depCol="",
                    outDeepTableName="",
                    outDeepTableDatabase="",
                    outObsIDCol="ObsID",
                    outVarIDCol="",
                    outValueCol="",
                    catToDummy=0,
                    performNorm=0,
                    performVarReduc=1,
                    makeDataSparse=0,
                    minStdDev= -1,
                    maxCorrel= 1.1,
                    trainOrTest=1,
                    excludeCols="",
                    classSpec="",
                    whereconditions="",
                    inAnalysisID=""
                    )
          )
