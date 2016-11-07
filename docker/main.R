suppressMessages(library('keboola.r.luckyguess', quiet = TRUE))

# run it
app <- LGApplication$new()
ret <- app$readConfig()
ret <- app$run()