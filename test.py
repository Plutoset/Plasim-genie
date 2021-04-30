import pandas as pd
import numpy as np
import netCDF4 as nc
from datetime import datetime, timedelta
from cftime import num2date, date2num
import time

def opendata(file):
    count = 0
    record=[]
    nowRec=[]
    date=[]
    with open(file,"r") as f:
        while True:
            count += 1
            # Get next line from file
            line = f.readline()
            # if line is empty
            # end of file is reached
            if not line:
                break
            # if count == 39:
            #     break
            if count % 38 == 1:
                date.append([int(i) for i in line.split()])
            else:
                nowRec.append([(int(i)/100) for i in line.split()])
            if count % 38 == 0:
                record.append(nowRec)
                nowRec=[]
    record = np.array(record)
    date = np.array(date)
    return record,date

def write_to_nc(data,file_name_path):

    nlon = data.shape[2]
    nlat = data.shape[1]
    ntime = data.shape[0]
    n_month = 12
    nyr = ntime // n_month + 1

    da=nc.Dataset(file_name_path,'w')
    
    longitude = da.createDimension('longitude',nlon)  #创建坐标点
    lon = da.createVariable("lon",'f',("longitude"))  #添加coordinates  'f'为数据类型，不可或缺
    lon.long_name = 'longitude'
    lon.units = "degrees east"
    lon.axis = "X"
    lon.coordinate_defines = "center"
    lonS = np.linspace(0,360,nlon)
    lon[:]=lonS     #填充数据
    da.variables['lon']=lon

    latitude = da.createDimension('latitude',nlat)  #创建坐标点
    lat = da.createVariable("lat",'f',("latitude"))  #添加coordinates  'f'为数据类型，不可或缺
    
    lat.long_name = 'latitude'
    lat.units = "degrees north"
    lat.axis = "Y"
    lat.coordinate_defines = "center"
    lat.coordinates
    latS = np.linspace(90,-90,nlat)
    lat[:]=latS
         #填充数据

    Time = da.createDimension("times", None)
    times = da.createVariable("times","f8",("times",))
    times.units = "hours since 1850-01-01 00:00:00.0"
    times.long_name = 'time'
    times.calendar = "standard"
    dates=[]
    for yr in range(1,nyr+1):
        for m in range(1,n_month+1):
            dates.append(datetime(yr+1849,m,1))
    dates
    times[:] = date2num(dates,units=times.units,calendar=times.calendar)
    da.createVariable('Time','f',("times"))
    da.variables['Time'][:]=times[:]

    hadslp2 = da.createVariable('hadslp2','f8',('times','latitude','longitude')) #创建变量，shape=(time,lat,lon) 'f'为数据类型，不可或缺
    hadslp2.units = "hPa"
    hadslp2[:,:,:]=data[:,:,:]       #填充数据

    da.description = "The Met Office Hadley Centre's mean sea level pressure"
    da.history = "Created " + time.ctime(time.time())
    da.source = "netCDF4 python module tutorial"
    da.close()

file = 'hadslp2r.asc'
record,date=opendata(file)
write_to_nc(record,"123.nc")