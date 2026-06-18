FROM rstudio/plumber:latest

RUN R -e "install.packages(c('quantmod','tidyverse','jsonlite','quadprog'), repos='https://cloud.r-project.org')"

COPY api_portafolio.R /app/api_portafolio.R

WORKDIR /app

ENTRYPOINT ["R", "-e"]
CMD ["pr <- plumber::plumb('api_portafolio.R'); pr$run(host='0.0.0.0', port=8000)"]
