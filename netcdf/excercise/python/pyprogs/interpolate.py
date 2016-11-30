#!/usr/bin/env python

import os
import sys
import pyproj
import numpy as np
import signal
from netCDF4 import Dataset , num2date

def handler(signum,stack):
    print('Closing file...')
    ofile.close( )
    ifile.close( )
    sys.exit(0)

signal.signal(signal.SIGTERM,handler)
signal.signal(signal.SIGINT,handler)

stationdir = '.' #'/home/netapp-clima/shared/OBS/GCOSGHCN'
griddir = '.'
stationfile = os.path.join(stationdir,'JC2010_stations_swap.nc') #'GCOSGHCN_prcp.nc'
gridfile = os.path.join(griddir,'JC2010_stations_gridded5km.nc') #'GCOSGHCN_prcp_gridded.nc'

geod = pyproj.Geod(ellps='WGS84')

ifile = Dataset(stationfile,'r')
time = ifile.variables['time'][:]
mlat = ifile.variables['latitude'][:]
mlon = ifile.variables['longitude'][:]
mhgt = ifile.variables['hgt'][:]
mcode = ifile.variables['code'][:]
inpcp = ifile.variables['schnee'] #prcp
#inqly = ifile.variables['quality_flag']

nstation = len(mlat)
tottime = len(time)

ii = np.array(mlat,dtype='i4')
jj = np.array(mlat,dtype='i4')
w = np.array(mlat)

deltalat = 0.05 #0.25
deltalon = 0.05 #0.25
hdla = deltalat/2.0
hdlo = deltalon/2.0
minlat = 45.0 #-90
maxlat =  50.0 #90
minlon = 5.0 #-180
maxlon =  20.0 #180
# minlat =  15.0
# maxlat =  50.0
# minlon = -140.0
# maxlon =  -40.0

spanlat = maxlat - minlat
spanlon = maxlon - minlon

glat = np.linspace(minlat+hdla, maxlat-hdla, int(spanlat/deltalat))
glon = np.linspace(minlon+hdlo, maxlon-hdlo, int(spanlon/deltalon))

for i in range(0,nstation):
    ii[i] = int((mlon[i] - minlon) / deltalon)
    jj[i] = int((mlat[i] - minlat) / deltalat)
    if (ii[i] < 0 or ii[i] > len(glon)-1 or
        jj[i] < 0 or jj[i] > len(glat)-1):
        ii[i] = -1
        jj[i] = -1
        w[i] = 1000.0
    else:
        angle1,angle2,w[i] = geod.inv(glon[ii[i]], glat[jj[i]],
                                      mlat[i], mlon[i])

if any(w) < 0.1:
    print('Division by zero!')
    sys.exit(-1)

w = 1.0/(w/100000.0)

nlat = len(glat)
nlon = len(glon)
gdata = np.empty((nlat,nlon))
cdata = np.empty((nlat,nlon),dtype='i4')
icdata = np.empty((nlat,nlon),dtype='i4')
filler = np.frompyfunc(lambda x: list( ), 1, 1)
idata = np.empty((nlat,nlon),dtype=np.object)
filler(idata, idata)

if os.path.isfile(gridfile) and os.access(gridfile, os.R_OK):
    ofile = Dataset(gridfile,'r+')
    nctim = ofile.variables['time']
    ncval = ofile.variables['schnee'] #prcp
    ncsta = ofile.variables['numstat']
    nctot = ofile.variables['global_station_number']
    ptim = ofile.variables['processed']
    tstart = ptim[:]
    filet = len(nctim)
else:
    ofile = Dataset(gridfile,'w',format='NETCDF4_CLASSIC')
    ofile.Conventions = 'CF-1.6'
    ofile.title = 'Gridded data set of snow depth on Alps'
    ofile.institution = 'Earth System Physics, International Centre for Theoretical Physics, Trieste, Italy'
    ofile.source = 'Austria stations data from Herbert Formayer <Herbert.Formayer@zamg.ac.at>'
    timn = ofile.createDimension('time')
    latn = ofile.createDimension('latitude',nlat)
    lonn = ofile.createDimension('longitude',nlon)
    nclat = ofile.createVariable('latitude','f8',('latitude'))
    nclon = ofile.createVariable('longitude','f8',('longitude'))
    nctim = ofile.createVariable('time','f8',('time'))
    ptim = ofile.createVariable('processed','i4')
    nctot = ofile.createVariable('global_station_number','i4',('time'))
    ncval = ofile.createVariable('schnee','f4',('time','latitude','longitude'), #prcp
               fill_value=-9999.0,zlib=True, complevel=9)
    ncsta = ofile.createVariable('numstat','i4',('time','latitude','longitude'),
               fill_value=0.0,zlib=True, complevel=9)
    nctim.units = 'days since 1850-01-01 00:00:00'
    nctim.standard_name = 'time'
    nclat.units = 'degrees_north'
    nclat.standard_name = 'latitude'
    nclat.long_name = 'Latitude'
    nclon.units = 'degrees_east'
    nclon.standard_name = 'longitude'
    nclon.long_name = 'Longitude'
    ncval.long_name = 'Snow Depth'
    ncval.standard_name = 'surface_snow_amount'
    ncval.units = 'kg m-2'
    ncsta.long_name = 'Station number per grid point'
    ncsta.units = '#'
    nctot.long_name = 'Total number of station for this timestep'
    nctot.units = '#'
    nclat[:] = glat
    nclon[:] = glon
    ptim[:] = 0
    tstart = 0
    filet = 0

#tstart = 47116+23740
#tstop = 56614+23740
tstop = tottime
tunits = 'days since 1850-01-01 00:00:00'

cdata.fill(0)
for s in range(0,nstation):
    si = ii[s]
    sj = jj[s]
    if si < 0 or sj < 0:
        continue
    cdata[sj,si] = cdata[sj,si] + 1
    idata[sj,si].append(s)

for t in range(tstart,tstop):
    atime = time[t]
    print('Gridding time '+
            str(num2date(time[t],'days since 1850-01-01 00:00:00'))+' '+
            repr(float(t+1)/float(tstop)*100.0)+' %')
    gdata.fill(-9999.0)
    icdata.fill(0)
    pcpstat = inpcp[t,:]
    #qflag = inqly[t,:]
    valid_data = np.argwhere(pcpstat >= 0.0)
    for j in range(0,nlat):
        for i in range(0,nlon):
            if cdata[j,i] > 0:
                z = idata[j,i]
                x = np.in1d(valid_data,z)
                x = valid_data[x]
                if len(x) > 0:
                    if x.size == 1:
                        gdata[j,i] = pcpstat[x]
                        icdata[j,i] = 1
                    else:
                        ww = np.sum(w[x])
                        gdata[j,i] = np.sum(pcpstat[x]*w[x])/ww
                        icdata[j,i] = len(x)
    nctim[filet] = atime
    nctot[filet] = np.sum(icdata)
    ncval[filet,:,:] = gdata
    ncsta[filet,:,:] = icdata
    ofile.sync( )
    filet = filet + 1
    ptim[:] = t

ifile.close( )
ofile.close( )

# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
