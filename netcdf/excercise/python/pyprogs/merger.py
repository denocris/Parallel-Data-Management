#!/usr/bin/env python

import os
import glob
import numpy as np
from netCDF4 import Dataset

convdir = '.'
mergedir = '.'

nclist = sorted(glob.glob(os.path.join(convdir,'*_schnee_*.nc')))
totfile = len(nclist)
fname = os.path.join(mergedir,'JC2010_stations.nc')

create_file = True

istat = 0
time = np.empty(0)
mlat = np.empty((totfile))
mlon = np.empty((totfile))
mhgt = np.empty((totfile))
mcod = np.empty((totfile,56),dtype='c')

for ncfile in nclist:
    print('Now working on '+ncfile+' '+
            repr(float(istat+1)/float(totfile)*100.0)+"%")
    ifile = Dataset(ncfile,'r')
    atime = (ifile.variables['time'][:]).astype('i8')
    mlat[istat] = ifile.variables['latitude'][:]
    mlon[istat] = ifile.variables['longitude'][:]
    mhgt[istat] = ifile.variables['hgt'][:]
    mcod[istat,:] = ifile.variables['code'][:]
    time = np.unique(np.append(time,atime))
    ifile.close( )
    istat = istat + 1

tottime = len(time)
print('Got a total global of '+repr(tottime)+' timesteps')

if create_file:
    mfile = Dataset(fname,'w', format='NETCDF4_CLASSIC')
    statn = mfile.createDimension('station',totfile)
    ndata = mfile.createDimension('time',tottime)
    ndata = mfile.createDimension('codelen',56)
    nclat = mfile.createVariable('latitude','f8',('station',))
    nclon = mfile.createVariable('longitude','f8',('station',))
    nchgt = mfile.createVariable('hgt','f8',('station',))
    nccod = mfile.createVariable('code','c',('station','codelen'))
    nctim = mfile.createVariable('time','f8',('time',))
    ncval = mfile.createVariable('schnee','f4',('station','time'),
               fill_value=9999.0)
    nctim.units = 'days since 1850-01-01 00:00:00'
    nctim.standard_name = 'time'
    nclat.units = 'degrees_north'
    nclat.standard_name = 'latitude'
    nclat.long_name = 'Latitude'
    nclon.units = 'degrees_east'
    nclon.standard_name = 'longitude'
    nclon.long_name = 'Longitude'
    nchgt.units = 'm'
    nchgt.statndard_name = 'height'
    nchgt.long_name = 'Height'
    ncval.long_name = 'Snow depth'
    ncval.standard_name = 'surface_snow_amount'
    ncval.units = 'mm'
    nctim[:] = time.astype('f8')
    nclat[:] = mlat
    nclon[:] = mlon
    nchgt[:] = mhgt
    nccod[:] = mcod
    mfile.sync( )

print('Processing data.')

mval = np.empty((tottime))

istat = 0
for ncfile in nclist:
    print('Now working on '+ncfile+' '+
            repr(float(istat+1)/float(totfile)*100.0)+"%")
    mval.fill(9999.0)
    ifile = Dataset(ncfile,'r')
    xtime = ifile.variables['time'][:]
    atime = xtime.astype('i8')
    xval = ifile.variables['schnee'][:]
    ii = np.where(np.in1d(time,atime))
    mval[ii] = xval
    ifile.close( )
    if create_file:
        ncval[istat,:] = mval
    mfile.sync( )
    istat = istat + 1

if create_file:
    mfile.close( )

# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
