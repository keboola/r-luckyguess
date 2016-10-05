# install really required packages
install.packages(c('git2r', 'jsonlite', 'devtools', 'rJava', 'RJDBC', 'dplyr'), repos = 'http://cran.us.r-project.org', dependencies = c("Depends", "Imports", "LinkingTo"), INSTALL_opts = c("--no-html"))

# install some commonly used packages
install.packages(c('corrgram', 'data.table', 'gbm', 'ggplot2', 'leaps', 'plyr', 'lubridate', 'arules', 'reshape2'), repos = 'http://cran.us.r-project.org', dependencies = c("Depends", "Imports", "LinkingTo"), INSTALL_opts = c("--no-html"))

library('devtools')

# install the R application
install_github('keboola/redshift-r-client', ref = "1.0.10")
install_github('snowflakedb/dplyr-snowflakedb', ref="v0.2.0")
install_github('keboola/provisioning-r-client', ref = "1.0.1")
install_github('keboola/backend-r-client', ref = "master")
install_github('keboola/r-application', ref = "1.0.1")
install_github('keboola/r-docker-application', ref = "1.0.0")

# install commonly used packages from github
install_github('keboola/AnomalyDetection', ref = "master")
