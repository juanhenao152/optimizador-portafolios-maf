FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('plumber', repos='https://cloud.r-project.org', dependencies=TRUE)"
RUN R -e "install.packages('quantmod', repos='https://cloud.r-project.org', dependencies=TRUE)"
RUN R -e "install.packages('tidyverse', repos='https://cloud.r-project.org', dependencies=TRUE)"
RUN R -e "install.packages('jsonlite', repos='https://cloud.r-project.org', dependencies=TRUE)"
RUN R -e "install.packages('quadprog', repos='https://cloud.r-project.org', dependencies=TRUE)"

RUN R -e "if (!require('plumber')) stop('plumber NO instalado')"

COPY api_portafolio.R /app/api_portafolio.R

WORKDIR /app

EXPOSE 8000

CMD ["R", "-e", "pr <- plumber::plumb('api_portafolio.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 8000)))"]
