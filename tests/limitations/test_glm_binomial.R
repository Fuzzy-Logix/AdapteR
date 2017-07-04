## MD Testing
test_that("glm: multi dataset",{
    flMDObjectDeep <- FLTableMD(table=getTestTableName("tblLogRegrMulti"),
                            group_id_colname="DATASETID",
                            obs_id_colname="OBSID",
                            var_id_colname="VARID",
                            cell_val_colname="NUM_VAL")

    fit <- lm(NULL,
                data = flMDObjectDeep)
    coeffList <- coef(lm)
    summaryList <- summary(lm)
    test_that("Check for dimensions of coefficients and summary for DeepTable ",{
        expect_equal(names(coeffList),
                     paste0("Model",flMDObjectDeep@Dimnames[[3]]))
        expect_equal(names(coeffList),
                     names(summaryList))
        vlenCoeffs <- colnames(flMDObjectDeep)[[1]]
        vlenCoeffs <- length(setdiff(vlenCoeffs[1]:vlenCoeffs[2],-1))
        lapply(coeffList,function(x){
            expect_equal(length(x),vlenCoeffs)
        })
    })
})

