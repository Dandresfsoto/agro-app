FROM rocker/shiny:4.0.5

# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    openjdk-11-jdk \
    libmysqlclient-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true
COPY Rprofile.site /etc/R
RUN install2.r --error --skipinstalled \
    shiny \
    shinydashboard \
    shinydashboardPlus \
    shinyWidgets \
    shinycssloaders \
    shinyjs \
    shinyBS \
    fresh \
    DT \
    tidyverse \
    dplyr \
    magrittr \
    stringr \
    ggplot2 \
    plotly \
    readxl \
    reshape2 \
    leaflet \
    rlang \
    RColorBrewer \
    data.table \
    FactoMineR \
    factoextra \
    xlsx \
    DBI \
    RMySQL \
    rJava \
    glue

COPY ./app/* /srv/shiny-server/

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]