FROM quay.io/keboola/docker-base-r:3.2.5-b

WORKDIR /home

# Initialize the LGR runner
COPY . /home/

# Install some commonly used R packages and the R application
RUN Rscript /home/docker/init.R

# Install the r-luckyguess package which is in the local directory
RUN R CMD build .
RUN R CMD INSTALL keboola.r.luckyguess_*	  

# Run the application
ENTRYPOINT Rscript /home/docker/main.R /data/
