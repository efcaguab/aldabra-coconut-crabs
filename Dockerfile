# Includes R 3.5.0, src build tools, rstudio, tidyverse & devtools and tex and publishing-related pacakages. R 3.5.0 was released on April 23
FROM rocker/tidyverse:3.5.2
RUN R -e "install.packages('drake', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('cowplot', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('checkmate', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('inline', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('unmarked', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('geosphere', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
# System dependencies for rgdal
RUN apt-get -y install libgdal-dev libproj-dev
RUN R -e "install.packages('rgdal', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
# To open data base files
RUN apt-get -y install mdbtools
RUN R -e "install.packages('Hmisc', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('oce', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('smatr', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('ggfortify', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('grImport', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
RUN R -e "install.packages('foreach', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"