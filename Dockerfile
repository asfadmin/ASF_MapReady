FROM ubuntu:18.04

WORKDIR /tmp/build

RUN apt update && \
    apt install -y \
    build-essential \
    wget

# install proj 4.7.0 from source
ENV PROJ_SHA256=fc5440002a496532bfaf423c28bdfaf9e26cc96c84ccefcdefde911efbd98986 \
    PROJ_VERSION=4.7.0 \
    PROJ_DIR=/usr/bin/proj

RUN wget --quiet https://launchpad.net/ubuntu/+archive/primary/+sourcefiles/proj/${PROJ_VERSION}-2ubuntu1/proj_${PROJ_VERSION}.orig.tar.gz && \
    echo "${PROJ_SHA256} proj_${PROJ_VERSION}.orig.tar.gz" | sha256sum -c - && \
    tar -xf "proj_${PROJ_VERSION}.orig.tar.gz"
    
RUN cd proj-${PROJ_VERSION} && \
    ./configure && \
    make && \
    make install  && \
    ln -s /usr/lib/x86_64-linux-gnu/libproj.so /usr/lib/libproj.so.0

# install additional MapReady dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    scons \
    pkg-config \
    libxml2-dev \
    libgsl-dev \
    libpng-dev \
    bison \
    flex \
    gcc \
    g++ \
    libtiff-dev \
    libproj-dev  \
    libgeotiff-dev \
    libhdf5-dev \
    libglib2.0-dev \
    libgdal-dev \
    libshp-dev \
    shapelib \
    libcunit1-dev \
    fftw-dev

# symlink expected libhdf5_serial and libhdf5_serial_hl library names
RUN ln -s /usr/lib/x86_64-linux-gnu/libhdf5_serial.so.100.0.1 /usr/lib/x86_64-linux-gnu/libhdf5.so && \
    ln -s /usr/lib/x86_64-linux-gnu/libhdf5_serial_hl.so.100.0.0 /usr/lib/x86_64-linux-gnu/libhdf5_hl.so

# install MapReady
ADD . /tmp/build/MAPREADY
WORKDIR /tmp/build/MAPREADY
RUN scons install --prefix=/mapready-build --release_build --no_gui
ENV PATH=/mapready-build/bin:$PATH
