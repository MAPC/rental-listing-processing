FROM r-base

RUN set -ex; \
    \
    echo "deb http://archive.ubuntu.com/ubuntu xenial universe" >> /etc/apt/sources.list \
    ; \
    apt-get update \
    && apt-get install -y --allow-unauthenticated \
      libgdal1-dev \
      libproj-dev \
    ; \
    R -e "install.packages('plyr', repos='https://cloud.r-project.org/')" \
    && R -e "install.packages('ggplot2', repos='https://cloud.r-project.org/')" \
    && R -e "install.packages('stringr', repos='https://cloud.r-project.org/')" \
    && R -e "install.packages('stringdist', repos='https://cloud.r-project.org/')" \
    && R -e "install.packages('ngram', repos='https://cloud.r-project.org/')" \
    && R -e "install.packages('sp', repos='https://cloud.r-project.org/')" \
    && R -e "install.packages('rgdal', repos='https://cloud.r-project.org/')" \
    && R -e "install.packages('raster', repos='https://cloud.r-project.org/')"

WORKDIR /usr/src/app
