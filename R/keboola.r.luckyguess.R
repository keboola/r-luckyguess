#' Application which serves as a backend for LuckyGuess component
#' @import methods
#' @import keboola.r.docker.application
#' @import keboola.redshift.r.client
#' @import keboola.provisioning.r.client
#' @export LGApplication
#' @exportClass LGApplication
LGApplication <- setRefClass(
    'LGApplication',
    contains = c("DockerApplication"),
    fields = list(
        # KBC token (read from environment)
        token = 'ANY',
        # KBC runId (read from environment)
        runId = 'ANY',
        # source table in database (provided in config)
        sourceTable = 'character',
        # script parameters (provided in config)
        scriptParameters = 'list',
        # script to execute (provided in config)
        scriptContent = 'character',
        # file tags (provided in config)
        fileTags = 'character',
        # storage for resulting key-values
        keyValue = 'data.frame',
        # storage for resulting files
        fileNames = 'data.frame',
        # storage for resulting tables
        tableNames = 'data.frame',
        # connection to redshift database (RedshiftDriver)
        db = 'RedshiftDriver',
        # name of table with key-value results
        keyValTable = 'character',
        # name of table with generated files
        fileNamesTable = 'character',
        # name of table with generated tables
        tableNamesTable = 'character',
        # internal list of warnings generated when checking for parameters
        checkWarnings = 'ANY',
        # internal list of errors generated when checking for parameters
        checkErrors = 'ANY',
        # working directory
        workingDir = 'character'
    ),
    methods = list(
        #' Constructor.
        #'
        #' @param Optional name of data directory, if not supplied then it
        #'  will be read from command line argument.
        #' @exportMethod
        initialize = function(args = NULL) {
            callSuper(args)
            keyValTable <<- 'r__key_val_results'
            fileNamesTable <<- 'r__file_names'
            tableNamesTable <<- 'r__table_names'
        },

        
        #' Save an arbitrary simple value.
        #'
        #' @param key String key name.
        #' @param value Arbitrary value.
        #' @param grouping Optional grouping of values if they are related
        #' @exportMethod
        saveValue = function(name, value, grouping = 0) {
           newRow <- data.frame(name, value, grouping, stringsAsFactors = FALSE)           
           keyValue <<- rbind(keyValue, newRow)
        },
        
        
        #' Save reference to a file.
        #'
        #' @param key String file name.
        #' @param value String physical file path.
        #' @exportMethod
        saveFileName = function(name, value) {
           newRow <- data.frame(name, value, stringsAsFactors = FALSE)
           fileNames <<- rbind(fileNames, newRow)
        },
        
        
        #' Save a dataframe to database using bulk inserts. The table will be created to accomodate to data frame columns.
        #'
        #' @param dataFrame A data frame, column names of data frame must correspond to column names of table
        #' @param tableName Name of the table.
        #' @param rowNumbers If true then the table will contain a column named 'row_num' with sequential row index
        #' @param incremental If true then the table will not be recreated, only data will be inserted
        #' @exportMethod
        saveDataFrame = function(dataFrame, tableName, rowNumbers = FALSE, incremental = FALSE, forcedColumnTypes) {
           if (missing(forcedColumnTypes)) {
               db$saveDataFrame(dataFrame, tableName, rowNumbers, incremental)
           } else {
               db$saveDataFrame(dataFrame, tableName, rowNumbers, incremental, forcedColumnTypes)        
           }
           newRow <- data.frame(name = tableName, stringsAsFactors = FALSE)
           tableNames <<- rbind(tableNames, newRow)
        },

        
        #' Recursive function to check a parameter provided in JSON data. Note that this function is called for
        #'  each parameter (name) separately and it tries to find that 'name' in the provided 'data'. It will
        #'  return the data possibly with coerced values
        #' 
        #' @param data JSON parsed data
        #' @param name Name of the parameter, either a string or a vector of characters if the parameter is nested.
        #' @param dataType String name of the R data type required
        #' @param fullName Arbitrary named used for global identification of the parameter.
        #' @return Partially modified data.
        checkParam = function(data, name, dataType, fullName) {
            checkWarnings <<- NA
            if (length(name) > 1) {
                # name is still nested, we need to go deeper   
                tmpData <- checkParam(data[[name[1]]], name[-1], dataType, fullName)
                data[[name[1]]] <- tmpData[["data"]]
                checkWarnings <<- tmpData[["warnings"]]
            } else if (length(name) == 1) {
                # this is just for reporting errors
                if (name == fullName) {
                    fullName <- 'root'
                }
                # name is present in data (i.e. parameter value is supplied)
                if (name[1] %in% names(data)) {
                    if (typeof(data[[name[1]]]) != dataType) {
                        # parametr value differs from the required one
                        if (dataType == "integer") {
                            # try to cast
                            tryCatch({
                                data[[name[1]]] <- as.integer(data[[name[1]]])
                            }, warning = function(w) {
                                checkWarnings <<- paste0("Parameter ", name, " in ", fullName, " has different datatype, got ", 
                                                    typeof(data[[name[1]]]), " expected ", dataType, ", and cannot be converted: ", w$message)
                            })
                        } else if (dataType == "double") {
                            # try to cast
                            tryCatch({
                                data[[name[1]]] <- as.double(data[[name[1]]])
                            }, warning = function(w) {
                                checkWarnings <<- paste0("Parameter ", name, " in ", fullName, " has different datatype, got ", 
                                                    typeof(data[[name[1]]]), " expected ", dataType, ", and cannot be converted: ", w$message)
                            })
                        } else if (dataType == "logical") {
                            # try to cast
                            tryCatch({
                                # convert 1,0 and TRUE,FALSE and "TRUE","FALSE"
                                data[[name[1]]] <- as.logical(data[[name[1]]])
                                if (is.na(data[[name[1]]])) {
                                    # convert "1","0"
                                    data[[name[1]]] <- as.logical(as.integer(data[[name[1]]]))
                                }
                            }, warning = function(w) {
                                checkWarnings <<- paste0("Parameter ", name, " in ", fullName, " has different datatype, got ", 
                                                    typeof(data[[name[1]]]), " expected ", dataType, ", and cannot be converted: ", w$message)
                            })
                        } else if (dataType == "lg_column") {
                            # 'lg_column' is our own special type used to lowercase a string
                            data[[name[1]]] <- tolower(data[[name[1]]])
                        } else {
                            checkWarnings <<- paste0("Parameter ", name, " in ", fullName, " has different datatype, got ", 
                                               typeof(data[[name[1]]]), " expected ", dataType, ".")
                        }
                    }
                } else {
                    checkWarnings <<- paste0("Parameter ", name, " is missing in ", fullName, ".")
                }
            }  
            list("data" = data, "warnings" = checkWarnings)
        },
        

        #' Silence all but error output from a command.
        #' 
        #' Note: this function does nothing if the debugMode variable is set to TRUE.
        #' @return Command return value.
        silence = function(command) {
            if (!debugMode) {
                msg.trap <- capture.output(suppressPackageStartupMessages(suppressMessages(suppressWarnings(ret <- command))))
            } else {
                ret <- command
            }
            ret
        },
        
        
        #' Install and load all required libraries.
        #' 
        #' @param character vector of package names to install
        installModulePackages = function(packages = c()) {
            if (!is.null(packages) && (length(packages) > 0)) {
                # repository <- "http://cran.us.r-project.org"
                # use the czech mirror to increase speed slightly
                repository <- "http://mirrors.nic.cz/R/"
                # get only packages not yet installed
                packagesToInstall <- packages[which(!(packages %in% rownames(installed.packages())))]
                if (length(packagesToInstall) > 0) {
                    silence(
                        install.packages(
                            pkgs = packagesToInstall, 
                            lib = workingDir, 
                            repos = repository, 
                            quiet = TRUE, 
                            verbose = FALSE, 
                            dependencies = c("Depends", "Imports", "LinkingTo"), 
                            INSTALL_opts = c("--no-html")
                        )
                    )
                }
                # load all packages
                lapply(packages, function (package) {
                    silence(library(package, character.only = TRUE, quietly = TRUE))
                })
            }
        },
        
        
        #' Save uploaded files (move them to out directory and create manifests)
        saveFiles = function() {
            logDebug("Saving files")
            files <- db$select(paste0("SELECT name, value FROM \"", db$schema, "\".\"r__file_names\";"))
            for (i in 1:nrow(files)) {
                logDebug(paste0("I'm creating manifest for file: ", files[i,]$value))
                    
                destination <- file.path(dataDir, 'out/files', files[i,]$value)
                # intentionally use copy, because file.rename may use hardlinks which are not supported across partititons (in docker)
                file.copy(
                    from = file.path(workingDir, files[i,]$value),
                    to = destination
                )
                writeFileManifest(
                    fileName = destination, fileTags = fileTags, isPublic = FALSE, isPermanent = TRUE, notify = FALSE
                )
            }
        },

        
        #' Validate application configuration
        #' @exportMethod
        validate = function() {
            # validate environment
            token <<- getEnv('KBC_TOKEN')
            runId <<- getEnv('KBC_RUNID')
            if (empty(token)) {
                stop("KBC token must be set in environment variable KBC_TOKEN.")
            }
            if (empty(runId)) {
                runId <<- ""
            }
            
            # validate parameters
            sourceTable <<- configData$parameters$sourceTable
            if (empty(sourceTable)) {
                stop("Source table must be provided in configuration.")
            }
            scriptParameters <<- configData$parameters$scriptParameters
            scr <- configData$parameters$script
            if (length(scr) > 1)  {
                scriptContent <<- paste(scr, collapse = "\n")
            } else {
                scriptContent <<- scr
            }
            if (!empty(configData$parameters$fileTags)) {
                fileTags <<- c('LuckyGuess', configData$parameters$fileTags);
            } else {
                fileTags <<- c('LuckyGuess')
            }
            
            # debug print parameters
            logDebug(paste0("Using data directory: ", dataDir))
            logDebug(paste0("Token set to: ", token))
            logDebug(paste0("RunId set to: ", runId))
            logDebug(paste0("Source table set to: ", sourceTable))
            logDebug(paste0("Script set to: ", scriptContent))
            logDebug("Script parameters set to: ")
            logDebug(scriptParameters)
            
            # tohle cely bude taky package a main.R bude jen v dockerFilu udelany
        },
        
        #' Main application entry point
        #' @exportMethod
        startUp = function() {
            logInfo("Starting")
            logDebug("Validating parameters")
            validate()
            
            # initialize working directory
            workingDir <<- tempdir()
            if (file.exists(workingDir)) {
                unlink(workingDir, recursive = TRUE)
            }
            dir.create(workingDir, recursive = TRUE)
            # make working dir also library dir so that parallel runs do not clash with each other
            .libPaths(c(.libPaths(), workingDir)) 
            logDebug(paste0("Created working directory: ", workingDir))
            
            # get database credentials and connect to database
            client <- ProvisioningClient$new('redshift', token, runId)
            credentials <- client$getCredentials('transformations')$credentials 
            db <<- RedshiftDriver$new()
            db$connect(
                credentials$host, 
                credentials$db, 
                credentials$user, 
                credentials$password, 
                credentials$schema
            )
            logDebug(paste0("Connected to database schema ", credentials$schema))

            # prepare database structure
            if (db$tableExists(keyValTable)) {
                db$update(paste("DROP TABLE ", keyValTable, ";", sep = ""))
            }
            if (db$tableExists(fileNamesTable)) {
                db$update(paste("DROP TABLE ", fileNamesTable, ";", sep = ""))
            }
            if (db$tableExists(tableNamesTable)) {
                db$update(paste("DROP TABLE ", tableNamesTable, ";", sep = ""))
            }            
            db$update(paste0("CREATE TABLE ", keyValTable, " (name VARCHAR(200), value VARCHAR(200), grouping VARCHAR(200), PRIMARY KEY (name));"))
            db$update(paste0("CREATE TABLE ", fileNamesTable, " (name VARCHAR(200), value VARCHAR(200), id INTEGER, PRIMARY KEY (name));"))
            db$update(paste0("CREATE TABLE ", tableNamesTable, " (name VARCHAR(200));"))        
        },
        
        #' Main application entry point
        #' @exportMethod
        run = function() {
            startUp()
            # prepare the module
            logInfo("Preparing module")
            scriptFile = file.path(workingDir, 'script.R')
            write(file = scriptFile, x = scriptContent)
            wrapTryCatch({
                # load the module
                source(scriptFile)
                module = Module$new()
                requiredParams <- module$parameters()
                
                logDebug("Module packages")
                logDebug(requiredParams$packages)
                # install module packages        
                installModulePackages(requiredParams$packages)
                requiredParams[['packages']] <- NULL
                
                # verify parameters required by the module
                checkErrors <<- vector()
                paramsJ <- scriptParameters
                for (key in names(requiredParams)) {
                    name <- unlist(strsplit(key, ".", fixed = TRUE))
                    tryCatch({
                        tmpData <- checkParam(paramsJ, name, requiredParams[[key]], key)
                        paramsJ <- tmpData[["data"]]
                        checkErrors <<- c(checkErrors, tmpData[["warnings"]])
                    }, error = function(e) {
                        checkErrors <<- c(checkErrors, e$message)
                    })
                }
                checkErrors <<- checkErrors[!is.na(checkErrors)]
                if (length(checkErrors) > 0) {
                    stop(paste0("Some required parameters for module ", moduleToExecute, " were not provided: ", paste(errors, collapse = " ")))
                }
                
                logInfo("Running module")
                module$run(.self)
            })
            logInfo("Saving data")
            db$saveDataFrame(keyValue, keyValTable, rowNumbers = FALSE, incremental = TRUE)
            db$saveDataFrame(fileNames, fileNamesTable, rowNumbers = FALSE, incremental = TRUE)
            db$saveDataFrame(tableNames, tableNamesTable, rowNumber = FALSE, incremental = TRUE)
            saveFiles()
            logInfo("LGR Finished")
            TRUE
        }
    )
)
