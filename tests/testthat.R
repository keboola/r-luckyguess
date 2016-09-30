library(testthat)

# default values
KBC_TOKEN <- 'yourToken'
KBC_RUNID <- '123'
KBC_DATADIR <- 'tests/data'

# override with config if any
if (file.exists("config.R")) {
    source("config.R")
}

# override with environment if any
if (nchar(Sys.getenv("KBC_TOKEN")) > 0) {
    KBC_TOKEN <- Sys.getenv("KBC_TOKEN")  
}
if (nchar(Sys.getenv("KBC_RUNID")) > 0) {
    KBC_TOKEN <- Sys.getenv("KBC_RUNID")  
}
if (nchar(Sys.getenv("KBC_DATADIR")) > 0) {
    KBC_DATADIR <- Sys.getenv("KBC_DATADIR")  
}

test_check("keboola.r.luckyguess")
