# Author: Katelyn Watson
# Date: April 15, 2016

# Description: Find the indices for a grid cell containing a specified
# latitude and longitude coordinate pair

# Load required libraries
library(ncdf4)
library(fields)

# Specify coordinate pair
lat <- 42.3190
lon <- -114.7010

sprintf("Point coordinates:")
sprintf("Latitude: %f", lat)
sprintf("Longitude: %f", lon)

# Specify file URL for gridded data
file_url <- "http://data.boisestate.edu/opendap/WRF/WRF_30_year/surface/wy_2006/wrf_out/d01/wrfout_d01_2005-10-01_00_00_00_sfc.nc"

# Open file and retrieve latitude and longitude information
nc <- nc_open(file_url)
lat_nc <- ncvar_get(nc,"XLAT")  # 3D matrix (X,Y,Time) - constant over time so we can ignore that dimension
lon_nc <- ncvar_get(nc,"XLONG") # 3D matrix (X,Y,Time) - constant over time so we can ignore that dimension

# Reshape coordinate matricies to vector
lat1d <- matrix(lat_nc[,,1],1)
lon1d <- matrix(lon_nc[,,1],1)

# Create coordinate matrix
coords <- t(rbind(lon1d,lat1d))

# Replicate specified coordinate pair to match dims of coordinate matrix
loc2d <- t(matrix(c(lon,lat),2,dim(coords)[1]))

# Compute great circle distance for each set of pairs and find min distance
gc_dist = rdist.earth.vec(loc2d,coords)
i <- which.min(gc_dist)

# Convert from vector index to matrix indices
ind <- arrayInd(i,dim(lat_nc[,,1]))

sprintf("Index 1: %i", ind[,1])
sprintf("Index 2: %i", ind[,2])

sprintf("Grid cell coordinates:")
sprintf("Latitude: %f", lat_nc[ind[,1],ind[,2],1])
sprintf("Longitude: %f", lon_nc[ind[,1],ind[,2],1])

# Download and plot time series of temperature data for full time series in the file
t2 <- ncvar_get(nc,"T2",start=c(ind[,1],ind[,2],1),count=c(1,1,-1))
plot(t2)
