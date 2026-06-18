FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('plumber','quantmod','tidyverse','jsonlite','quadprog'), repos='https://cloud.r-project.org')"

COPY api_portafolio.R /app/api_portafolio.R

WORKDIR /app

EXPOSE 8000

CMD ["R", "-e", "pr <- plumber::plumb('api_portafolio.R'); pr$run(host='0.0.0.0', port=8000)"]
