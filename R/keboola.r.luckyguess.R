#' Application which serves as a backend for LuckyGuess component
#' @import methods keboola.r.docker.application keboola.backend.r.client keboola.provisioning.r.client
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
        # Type of workspace backend (redshift-workspace or snowflake, provided in config)
        backendType = 'character',
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
        # connection to keboola database (BackendDriver)
        db = 'BackendDriver',
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
        initialize = function(args = NULL) {
            "Constructor.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{fileName} Optional name of data directory, if not supplied then it
            will be read from command line argument.}
            }}"
            callSuper(args)
            keyValTable <<- 'r__key_val_results'
            fileNamesTable <<- 'r__file_names'
            tableNamesTable <<- 'r__table_names'
        },

        saveValue = function(name, value, grouping = 0) {
            "Save an arbitrary simple value.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{key} String key name.}
            \\item{\\code{value} Arbitrary value.}
            \\item{\\code{grouping} Optional grouping of values if they are related.}
            }}
            \\subsection{Return Value}{TRUE}"
           newRow <- data.frame(name, value, grouping, stringsAsFactors = FALSE)           
           keyValue <<- rbind(keyValue, newRow)
           TRUE
        },
        
        saveFileName = function(name, tags) {
            "Save reference to a file.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{name} String physical file path.}
            \\item{\\code{tags} Character vector of file tags.}
            }}
            \\subsection{Return Value}{TRUE}"
           newRow <- data.frame(name, tags, stringsAsFactors = FALSE)
           fileNames <<- rbind(fileNames, newRow)
           TRUE
        },
        
        saveDataFrame = function(dataFrame, tableName, rowNumbers = FALSE, incremental = FALSE, forcedColumnTypes) {
            "Save a dataframe to database using bulk inserts.
            The table will be created to accomodate to data frame columns.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{dataFrame} A data frame, column names of data frame 
            must correspond to column names of table.}
            \\item{\\code{tableName} Name of the table.}
            \\item{\\code{rowNumbers} If true then the table will contain a column 
            named \\code{row_num} with sequential row index.}
            \\item{\\code{incremental} If true then the table will not be recreated, 
            only data will be inserted.}
            }}
            \\subsection{Return Value}{TRUE}"
            if (missing(forcedColumnTypes)) {
                db$saveDataFrame(dataFrame, tableName, rowNumbers, incremental)
            } else {
                db$saveDataFrame(dataFrame, tableName, rowNumbers, incremental, forcedColumnTypes)
            }
            newRow <- data.frame(name = tableName, stringsAsFactors = FALSE)
            tableNames <<- rbind(tableNames, newRow)
            TRUE
        },

        checkParam = function(data, name, dataType, fullName) {
            "Recursive function to check a parameter provided in JSON data. 
            Note that this function is called for each parameter (name) separately and it
            tries to find that \\code{name} in the provided \\code{data}. It will
            return the data possibly with coerced values.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{data} JSON parsed data.}
            \\item{\\code{name} Name of the parameter, either a string or a vector 
            of characters if the parameter is nested.}
            \\item{\\code{dataType} String name of the R data type required.}
            \\item{\\code{fullName} Arbitrary named used for global identification of the parameter.}
            }}
            \\subsection{Return Value}{List with partially modified data.}"            
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
                        } else if (dataType == "numeric") {
                            # try to cast
                            tryCatch({
                                print(paste0("castin ", name[[1]], " to numeric"))
                                data[[name[1]]] <- as.numeric(data[[name[1]]])
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
        
        silence = function(command) {
            "Silence all but error output from a command.
            Note: this function does nothing if the debugMode variable is set to TRUE.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{command} Arbitrary command.}
            }}
            \\subsection{Return Value}{ Command return value.}"   
            #if (!debugMode) {
                msg.trap <- capture.output(suppressPackageStartupMessages(suppressMessages(suppressWarnings(ret <- command))))
            #} else {
            #    ret <- command
            #}
            ret
        },
        
        installModulePackages = function(packages = c()) {
            "Install and load all required libraries.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{packages} Vector of package names.}
            }}
            \\subsection{Return Value}{TRUE}"
            con <- textConnection("installMessages", open = "w", local = TRUE)
            sink(con, type = c("output", "message"))                
            if (!is.null(packages) && (length(packages) > 0)) {
                repository <- "http://cran.us.r-project.org"
                #repository <- "http://mirrors.nic.cz/R/"
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
            sink(NULL, type = c("output", "message"))
            logDebug(installMessages)
            TRUE
        },
        
        saveFiles = function() {
            "Save uploaded files (move them to out directory and create manifests)
            \\subsection{Return Value}{TRUE}"
            logDebug("Saving files")
            files <- db$select(paste0("SELECT \"name\", \"tags\" FROM \"", fileNamesTable, "\";"))
            fileList <- split(files, files$name)
            if (length(fileList) > 0) {
                for (i in 1:length(fileList)) {
                    tags <- fileList[[i]][, 'tags']
                    tags <- c(fileTags, tags)
                    # name is same for each row, we pick the first one
                    fileName <- fileList[[i]][1, 'name']
                    logDebug(paste0("I'm creating manifest for file: ", fileName))
                            
                    destination <- file.path(dataDir, 'out/files', fileName)
                    # intentionally use copy, because file.rename may use hardlinks which are not supported across partititons (in docker)
                    file.copy(
                        from = file.path(workingDir, fileName),
                        to = destination
                    )
                    if (file.exists(destination)) {
                        writeFileManifest(
                            fileName = destination, fileTags = tags, isPublic = FALSE, isPermanent = TRUE, notify = FALSE
                        )
                    } else {
                        logError(paste0("Failed to write file ", fileName, ", manifest not created."));
                    }
                }
            } # else no files
            TRUE
        },

        validate = function() {
            "Save uploaded files (move them to out directory and create manifests)
            \\subsection{Return Value}{TRUE}"
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
            validBackends <- c("redshift-workspace", "snowflake")
            backendType <<- configData$parameters$backendType
            if (empty(backendType) || !(backendType %in% validBackends)) {
                stop(paste("Unsupported backendType", backendType, "Must be one of", validBackends))
            }
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
            # It is important to add the LuckyGuess tag to all output files, because 
            # it is used later in searching for the right file uploads.
            if (!empty(configData$parameters$fileTags)) {
                fileTags <<- c('LuckyGuess', configData$parameters$fileTags);
            } else {
                fileTags <<- c('LuckyGuess')
            }
            
            # debug print parameters
            logDebug(paste0("Using data directory: ", dataDir))
            #logDebug(paste0("Token set to: ", token))
            logDebug(paste0("RunId set to: ", runId))
            logDebug(paste0("Source table set to: ", sourceTable))
            #logDebug(paste0("Script set to: ", scriptContent))
            logDebug("Script parameters set to: ")
            logDebug(scriptParameters)
            
            TRUE
        },

        startUp = function() {
            "Start up the Application, initialize working directory and database tables.
            \\subsection{Return Value}{TRUE}"
            logInfo("Starting")
            logDebug("Validating parameters")
            validate()
            
            # initialize working directory
            workingDir <<- tempdir()
            if (file.exists(workingDir)) {
                unlink(workingDir, recursive = TRUE)
            }
            dir.create(workingDir, recursive = TRUE)
            logDebug(paste0("Created working directory: ", workingDir))
            logDebug(paste("Connecting to backend: " , .self$backendType))
            # get database credentials and connect to database
            client <- ProvisioningClient$new(.self$backendType, token, runId)
            credentials <- client$getCredentials('luckyguess')$credentials 
            logDebug("GOT CREDENTIALS:")
            print(credentials)
            db <<- BackendDriver$new()
            db$connect(
                credentials$hostname, 
                credentials$db, 
                credentials$user, 
                credentials$password, 
                credentials$schema,
                backendType = .self$backendType
            )
            
            logDebug(paste0("Connected to", .self$backendType, "database schema ", credentials$schema))

            # prepare database structure
            if (db$tableExists(keyValTable)) {
                db$update(paste("DROP TABLE \"", keyValTable, "\";", sep = ""))
            }
            if (db$tableExists(fileNamesTable)) {
                db$update(paste("DROP TABLE \"", fileNamesTable, "\";", sep = ""))
            }
            if (db$tableExists(tableNamesTable)) {
                db$update(paste("DROP TABLE \"", tableNamesTable, "\";", sep = ""))
            }            
            db$update(paste0("CREATE TABLE \"", keyValTable, "\" (\"name\" VARCHAR(200), \"value\" VARCHAR(200), \"grouping\" VARCHAR(200), PRIMARY KEY (\"name\"));"))
            db$update(paste0("CREATE TABLE \"", fileNamesTable, "\" (\"name\" VARCHAR(200), \"tags\" VARCHAR(200));"))
            db$update(paste0("CREATE TABLE \"", tableNamesTable, "\" (\"name\" VARCHAR(200));"))
        },
        
        run = function() {
            "Main application entry point.
            \\subsection{Return Value}{TRUE}"
            startUp()
            # prepare the module
            logInfo("Preparing module")
            scriptFile = file.path(workingDir, 'script.R')
            write(file = scriptFile, x = scriptContent)
            wrapTryCatch({
                # load the module
                source(scriptFile)
                module = LGModule$new()
                module$setDebugMode(debugMode)
                requiredParams <- module$getParameters()
                
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
                scriptParameters <<- paramsJ
                checkErrors <<- checkErrors[!is.na(checkErrors)]
                if (length(checkErrors) > 0) {
                    stop(paste0("Some required parameters for module were not provided: ", paste(checkErrors, collapse = " ")))
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
        },

        getConvertedDataType = function(type, mode) {
            "Convert LG type definition to an R data type.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{type} LG data type string (\\code{integer}, \\code{datetime}, etc.).}
            \\item{\\code{mode} LG variable mode (\\code{continuous}, \\code{discrete}).}
            }}
            \\subsection{Return Value}{String with R data type name}"            
            if (is.null(type) || is.na(type) || (length(type) == 0)) {
                ret <- 'character'
            } else if (type == "integer" || type == "float") {
                ret <- "numeric"
            } else if (type == "date") {
                ret <- "date"
            } else if (type == "datetime") {
                ret <- "posix"
            } else if (type == "string") {
                if (mode == "continuous") {
                    ret <- "character"
                } else {
                    ret <- "factor"
                }
            } else {
                ret <- "factor"
            }
            return(ret)
        },

        getCleanData = function(types, cleanData) {
            "Apply column types detected by LG to a data frame.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{types} Data frame with contents of table with LG datatypes 
            (this table is usually named \\code{VAI__1} in SAPI).}
            \\item{\\code{cleanData} A data frame with actual data, its columns are
            expected to be listed as rows in the types table.}
            }}
            \\subsection{Return Value}{data.frame supplied in cleanData parameter with
            applied data types.}"            
            # remove columns run_id and _timestamp which are internal only
            cleanData <- cleanData[,!names(cleanData) %in% c("run_id", "_timestamp")]
            out <- lapply(
                1:length(cleanData),
                FUN = function(i) {
                    varName <- colnames(cleanData)[i]
                    varType <- types[which(types$var_name == varName),]
                    # there may be still multiple definitions if a job was executed repeatedly, so pick the first one
                    type <- .self$getConvertedDataType(varType[1, "data_type"], varType[1, "mode"])
                    FUN1 <- switch(
                        type,
                        "posix" = as.POSIXlt,
                        "date" = as.Date,
                        "character" = as.character,
                        "numeric" = as.numeric,
                        "factor" = as.factor
                    )
                    if (type == "date" || type == "posix") {
                        cleanData[which(cleanData[,i] == ""), i] <- NA
                    }
                    FUN1(cleanData[,i])
                }
            )
            names(out) <- colnames(cleanData)
            return(as.data.frame(out))
        }
    )
)
