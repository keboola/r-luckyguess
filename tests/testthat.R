library(testthat)

# default values
KBC_TOKEN = 'yourToken'
KBC_RUNID = '123'
DATA_DIR = 'tests/data'

# override with config if any
if (file.exists("config.R")) {
    source("config.R")
}

# override with environment if any
if (nchar(Sys.getenv("KBC_TOKEN")) > 0) {
    apitoken <- Sys.getenv("KBC_TOKEN")  
}

test_check("keboola.r.luckyguess")
