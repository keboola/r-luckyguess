test_that("validate", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(file.path(KBC_DATADIR, '1'))
    app$readConfig()
    app$validate()
    expect_equal("numeric", class(app$scriptParameters$paramNested$columnFoo))
    expect_equal(3, length(app$fileTags))
})

test_that("validate no tags", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(file.path(KBC_DATADIR, '2'))
    app$readConfig()
    app$validate()
    expect_equal(1, length(app$fileTags))
})

test_that("validate type cast", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(file.path(KBC_DATADIR, '2'))
    app$readConfig()
    app$run()
    expect_equal("numeric", class(app$scriptParameters$paramNested$columnFoo))
})

test_that("column conversions", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(file.path(KBC_DATADIR, '2'))
    app$readConfig()
    app$validate()
    app$run()
    expect_equal('test-column', app$scriptParameters$paramNested$columnName)
})


test_that("run without runid", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    app <- LGApplication$new(file.path(KBC_DATADIR, '1'))
    app$readConfig()
    app$validate()
    app$run()
})


test_that("run", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    Sys.setenv('KBC_RUNID' = KBC_RUNID)
    app <- LGApplication$new(file.path(KBC_DATADIR, '1'))
    app$readConfig()
    app$run()
    expect_true(file.exists(file.path(KBC_DATADIR, '1/out/files', 'exampleGraph.png')))
    expect_true(file.exists(file.path(KBC_DATADIR, '1/out/files', 'exampleGraph.png.manifest')))
})


test_that("file manifests", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    app <- LGApplication$new(file.path(KBC_DATADIR, '1'))
    app$readConfig()    
    app$startUp()

    dataFile = file.path(KBC_DATADIR, '1/out/files', 'fooBar.csv')
    manifestFile = file.path(KBC_DATADIR, '1/out/files', 'fooBar.csv.manifest')

    fileConn <- file(dataFile)
    writeLines("test", fileConn)
    app$saveFileName('fooBar.csv', c('baz', 'buzz'))
    app$saveFileName('BarFoo.csv', c('ping', 'pong'))
    app$db$saveDataFrame(app$fileNames, app$fileNamesTable, rowNumbers = FALSE, incremental = TRUE)
    app$saveFiles()

    expect_true(file.exists(manifestFile))
    expect_false(file.exists(file.path(KBC_DATADIR, '1/out/files', 'BarFoo.csv.manifest')))
    
    data <- readChar(manifestFile, file.info(manifestFile)$size)
    config <- jsonlite::fromJSON(data)
    
    expect_equal(config$is_public, FALSE)
    expect_equal(config$is_permanent, TRUE)
    expect_equal(config$notify, FALSE)
    # file tag from application, two file tags from config, two file tags from file
    expect_true('LuckyGuess' %in% config$tags)
    expect_true('TestFile' %in% config$tags)
    expect_true('RExecutorServiceTest' %in% config$tags)
    expect_true('baz' %in% config$tags)
    expect_true('buzz' %in% config$tags)
    file.remove(manifestFile)
    file.remove(dataFile)
})

test_that("save not files", {
    Sys.setenv('KBC_TOKEN' = KBC_TOKEN)
    app <- LGApplication$new(file.path(KBC_DATADIR, '1'))
    app$readConfig()    
    app$startUp()
    app$saveFiles()
})