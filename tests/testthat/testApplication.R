test_that("validate", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(file.path(DATA_DIR, '1'))
    app$readConfig()
    app$validate()    
    expect_equal(3, length(app$fileTags))
})

test_that("validate no tags", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(file.path(DATA_DIR, '2'))
    app$readConfig()
    app$validate()
    expect_equal(1, length(app$fileTags))
})

test_that("column conversions", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(file.path(DATA_DIR, '2'))
    app$readConfig()
    app$validate()
    app$run()
    expect_equal('test-column', app$scriptParameters$paramNested$columnName)
})


test_that("run without runid", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    app <- LGApplication$new(file.path(DATA_DIR, '1'))
    app$readConfig()
    app$validate()
    app$run()
})


test_that("run", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(file.path(DATA_DIR, '1'))
    app$readConfig()
    app$run()
    expect_true(file.exists(file.path(DATA_DIR, '1/out/files', 'exampleGraph.png')))
    expect_true(file.exists(file.path(DATA_DIR, '1/out/files', 'exampleGraph.png.manifest')))
})

