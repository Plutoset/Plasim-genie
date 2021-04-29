FROM continuumio/miniconda3

RUN conda install -c conda-forge pyproj jupyter requests geopandas

RUN pip install netCDF4