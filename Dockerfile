FROM ubuntu:rolling

RUN apt-get update
RUN apt-get install -y --no-install-recommends \
    scons \
    pkg-config \
    libxml2-dev \
    libgsl-dev \
    libpng-dev \
    bison \
    flex \
    gcc \
    libtiff-dev \
    libgeotiff-dev \
    libhdf5-dev \
    libglib2.0-dev \
    libgdal-dev \
    libshp-dev

RUN mkdir /mapready-source
ADD . /mapready-source
WORKDIR /mapready-source

RUN scons install --prefix=/mapready-build --release_build --no_gui
