# install really required packages
install.packages(c('git2r', 'jsonlite', 'devtools', 'rJava', 'RJDBC', 'dplyr', 'testthat'), repos = 'http://cran.us.r-project.org', dependencies = c("Depends", "Imports", "LinkingTo"), INSTALL_opts = c("--no-html"))

# install some commonly used packages
install.packages(c('arules', 'corrgram', 'data.table', 'gbm', 'ggplot2', 'leaps', 'plyr', 'lubridate', 'reshape2', 'rpart', 'rpart.plot'), repos = 'http://cran.us.r-project.org', dependencies = c("Depends", "Imports", "LinkingTo"), INSTALL_opts = c("--no-html"))

library('devtools')

# install the R application
install_github('keboola/redshift-r-client', ref = "1.0.10")
install_github('snowflakedb/dplyr-snowflakedb', ref="v0.2.0")
install_github('keboola/provisioning-r-client', ref = "1.0.3")
install_github('keboola/backend-r-client', ref = "0.0.5")
install_github('keboola/r-application', ref = "1.0.2")
install_github('keboola/r-docker-application', ref = "1.0.2")

# install commonly used packages from github
install_github('keboola/AnomalyDetection', ref = "master")
