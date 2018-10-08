FROM rocker/verse:latest

RUN R -e "install.packages('binb', repos = 'https://cloud.r-project.org/')"
