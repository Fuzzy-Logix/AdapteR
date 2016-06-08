table1<-initF.FLTable(rows = 100,col = 2)

table2<-FLTable(database = getOption("ResultDatabaseFL"),
                table= "tblAutoMpg",
                obs_id_colname = "ObsID")

table3<-FLTable(database = getOption("ResultDatabaseFL"),
               table= "tblUSArrests",
                 obs_id_colname = "OBSID",
                  var_id_colname = "VarID",
                  cell_val_colname ="Num_Val")



test_that("check for wide to deep",{sqlstr<-paste0("DROP TABLE ",getOption("ResultDatabaseFL"),".tblAutoMpgd;")
                                    sqlSendUpdate(getOption("connectionFL"),sqlstr)
                                    wideToDeep(object = table2,
                                              excludeCols = "HorsePower,Weight",
                                              classSpec = list(CarName = "Toyota",Cylinders = "4"),
                                              whereconditions="",
                                              outDeepTableName="tblAutoMpgd",
                                              outDeepTableDatabase="",
                                              outObsIDCol="ObsID",
                                              outVarIDCol="",
                                              outValueCol="out_value")
                                  }
 )

## added analysis id feature but saying that unable to find inherited method.
test_that("check for deep",deepToWide(object = table3,
                                      whereconditions="",
                                      mapTable="",
                                      mapName="",
                                      outWideTableDatabase="",
                                      outWideTableName="tblUSArrestswide1",
                                      Analysisid = ""
                                      ))
