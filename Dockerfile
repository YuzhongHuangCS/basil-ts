FROM rocker/r-ver
LABEL maintainer="Andreas Beger <adbeger@gmail.com>"

WORKDIR  /basil-ts
COPY requirements.txt /basil-ts

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    python3-setuptools python3-pip \
  && pip3 install -r requirements.txt 
  
# install R stuff in separate layer because it takes forever to install
RUN install2.r forecast lubridate jsonlite stringr

COPY ./ /basil-ts

EXPOSE 5000

CMD ["python3", "app.py"]