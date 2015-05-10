test_that("validate", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(DATA_DIR)
    app$readConfig()
    app$validate()
})

test_that("run without runid", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    app <- LGApplication$new(DATA_DIR)
    app$readConfig()
    app$validate()
    app$run()
})


test_that("run", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(DATA_DIR)
    app$readConfig()
    app$run()
    expect_true(file.exists(file.path(DATA_DIR, 'out/files', 'exampleGraph.png')))
    expect_true(file.exists(file.path(DATA_DIR, 'out/files', 'exampleGraph.png.manifest')))
})

