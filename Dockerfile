FROM ubuntu:18.04

SHELL ["/bin/bash","-c"]

RUN apt update \
    && apt install -y wget gnupg make g++ bc m4 xsltproc

RUN wget https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
    && apt-key add GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
    && rm GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
    && echo "deb https://apt.repos.intel.com/oneapi all main" | tee /etc/apt/sources.list.d/oneAPI.list \
    && apt update \
    && apt install -y intel-oneapi-compiler-fortran intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic \
    && rm -rf /var/lib/apt/lists/* 

RUN echo 'source /opt/intel/oneapi/setvars.sh intel64' | tee ~/.bashrc \
    && source ~/.bashrc

ENV CC=icc CXX=icpc CFLAGS="-O3 -xHost -ip -no-prec-div -static-intel -no-multibyte-chars" CXXFLAGS="-O3 -xHost -ip -no-prec-div -static-intel -no-multibyte-chars" F77=ifort FC=ifort F90=ifort FFLAGS="-O3 -xHost -ip -no-prec-div -static-intel" CPP="icc -E" CXXCPP="icpc -E"

COPY ./netcdf /netcdf/

RUN cd /netcdf/ \
    && ./configure --disable-netcdf-4 --disable-dap \
    && make check \
    && make install \
    && ldconfig

COPY ./netcdf-cxx /netcdf-cxx/

RUN cd /netcdf-cxx/ \
    && ./configure \
    && make check \
    && make install \
    && ldconfig

COPY ./netcdf-fortran /netcdf-fortran/

RUN cd /netcdf-fortran/ \
    && ./configure \
    && make check \
    && make install \
    && ldconfig

COPY ./genie /Plasim_genie/genie/

WORKDIR /Plasim_genie/genie/genie-main/