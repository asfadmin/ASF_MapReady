# Dockerfile to build MapReady against Ubuntu 18.04
#
# 1. Install docker
# 2. Build container
#   docker build -t mapready:18.04 -f Dockerfile .
# 3. Copy the MapReady bundle in the container to your local directory
#   docker cp $(docker run -d --rm mapready:18.04 sleep 5):/mapready-u18.zip .
#

FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y --no-install-recommends \
    scons \
    pkg-config \
    gcc \
    g++ \
    bison \
    flex \
    libcunit1-dev \
    libexif-dev \
    libfftw3-dev \
    libgdal-dev \
    libgeotiff-dev \
    libglade2-dev \
    libglib2.0-dev \
    libgsl-dev \
    libgtk2.0-dev \
    libjpeg-dev \
    libpng-dev \
    libproj-dev \
    libshp-dev \
    libtiff5-dev \
    libxml2-dev \
    zip

# Additional changes to what is currently documented ...
# Inspired from https://github.com/BVLC/caffe/issues/4333#issuecomment-228874430
RUN ln -s /usr/lib/x86_64-linux-gnu/libhdf5_serial.so.100 /usr/lib/x86_64-linux-gnu/libhdf5.so
RUN ln -s /usr/lib/x86_64-linux-gnu/libhdf5_serial_hl.so /usr/lib/x86_64-linux-gnu/libhdf5_hl.so

RUN cp /usr/lib/x86_64-linux-gnu/gtk-2.0/include/gdkconfig.h /usr/include/gdkconfig.h

RUN mkdir /mapready-source
COPY . /mapready-source
WORKDIR /mapready-source

RUN scons install --prefix=/mapready-build --release_build --no_gui
RUN zip -r /mapready-u18.zip /mapready-build
