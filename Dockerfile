#docker pull rocker/r-ver
#https://hub.docker.com/r/rocker/r-ver/~/dockerfile/

FROM rocker/r-ver
LABEL maintainer="Andreas Beger <adbeger@gmail.com>"

RUN mkdir /home/basil-ts
COPY requirements.txt /home/basil-ts

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    python3-setuptools python3-pip \
  && pip3 install -r /home/basil-ts/requirements.txt \
  && install2.r forecast lubridate jsonlite stringr

COPY ./ /home/basil-ts
