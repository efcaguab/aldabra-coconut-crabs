# Includes R 3.5.0, src build tools, rstudio, tidyverse & devtools and tex and publishing-related pacakages. R 3.5.0 was released on April 23
FROM rocker/tidyverse:3.5.2
# System dependencies for rgdal and data base files
RUN apt-get update && \
  apt-get install -y --no-install-recommends libgdal-dev libproj-dev mdbtools
# Needed R packages
RUN R -e "install.packages(c('foreach', 'grImport', 'smatr', 'ggfortify', 'oce', 'Hmisc', 'rgdal', 'geosphere', 'unmarked', 'inline', 'checkmate', 'cowplot', 'drake'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
