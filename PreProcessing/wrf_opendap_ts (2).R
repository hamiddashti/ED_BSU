wrf_opendap_ts <- function(cal_year, cal_month, cal_day, dom_num, var_name, i_index, j_index) {

# *************************************************************************************************
#
# FILE:     wrf_opendap_ts.R
# AUTHOR:   Matt Masarik      (MM)
# VERSION:  0    05/27/2016    MM   Initial version. Script.
#           1    05/31/2016    MM   Base function implementation  
#           1.1  06/20/2016    MM   Take input i_index, j_index  
#           1.2  11/10/2016    MM   Full grid (vs. a point) download option [i = -1, j = -1]
#
# USAGE:    var_ts <- wrf_opendap_ts(cal_year,cal_month,cal_day,dom_num,var_name,i_index,j_index)
#           
#               where,    
#                         cal_year  = <yyyy>      calender year.  WRF 30-year contains [1986-2015]
#                         cal_month = <mm>        calendar year.  [01-12]
#                         cal_day   = <dd>        calendar day.   [01-31]
#                         dom_num   = <n>         domain number.  n = [1,2].  1: d01, 2: d02.
#                         var_name  = <'string'>  WRF variable name. ex., 'T2' = 2M Temperature
#                         i_index   = <ii..i>     ii..i = WRF grid i index (longitude direction)
#                         j_index   = <jj..j>     jj..j = WRF grid j index (latitude direction)  
#               and,  
#                         var_ts    = <1-D>       1xN length object containing a time series 
#                                                 of variable <var_name>. d01: N=8, d02: N=24.
#                                     
#           
# PURPOSE:  Implements a function to extract a variable time series (ts) at a lat/lon point, from
#           a WRF output file via OPeNDAP.  User must input the lat/lon location in terms of WRF 
#           grid indices. The output type, and simulation type are specified in the parameter 
#           definition section below, (1) User Input Parameters.  The extraction occurs in two 
#           steps: (3.1) Contruct OPeNDAP URL, (3.2) OPeNDAP access via R/netCDF package, ncdf4
#           -- Use ncdf4 and the URL to access the file remotely, then download the subsetted
#           variable time series.  WRF grid indices:  i_index <=> longitude, 
#                                                     j_index <=> latitude.
#
# OUTLINE:  (1) User Input Parameters
#           (2) Set Up Environment
#           (3) Main
#               (3.1) Construct OPeNDAP URL to file
#               (3.2) Remote access of file
#
# EXAMPLE URL:  
#                   http://data.boisestate.edu:80/opendap/WRF/WRF_30_year    # base URL
#                   /    surface/wy_2006/wrf_out/d02                         # sub-directory
#                   /    wrfout_d02_2005-10-01_00_00_00_sfc.nc               # file name
#                   ?    T2[0:1:23][40][60]                                  # spatial subsetting
#  
# *************************************************************************************************





# -------------------------------------------------------------------------------------------------
# (1) User Input Parameters
# -------------------------------------------------------------------------------------------------
output_type    <- "out"                 #  "out"      = wrfout                        [default]
                                        #  "xtrm"     = wrfxtrm 
sim_type       <- "surface"             #  "surface"  = surface subset                [default]
                                        #  "full_sim" = full land/atmosphere simulation


# -------------------------------------------------------------------------------------------------
# (2) Set Up Environment
# -------------------------------------------------------------------------------------------------
##install.packages('ncdf4')
library(ncdf4)                       # Package for netCDF4

##install.packages('pracma')
library(pracma)                      # Package 'Practical Numerical Math Functions'. Contains the
                                     # ability to compare two strings


# -------------------------------------------------------------------------------------------------
# (3) Main
# -------------------------------------------------------------------------------------------------

# (3.1) Construct OPeNDAP URL
# ===========================
# online location of data file on OPeNDAP server, the WRF 30 year simulation is default
base_30_year_url <- "http://data.boisestate.edu:80/opendap/WRF/WRF_30_year"


# extension (.nc, sfc.nc).  For simulation type, 
#     full_sim:  extension = ".nc"
#     surface:   extension = "sfc.nc"
#       
if (strcmp(sim_type,"surface")) {                      # uses strcmp from package 'pracma'
  ext <- "sfc.nc"
} else {
  ext <- ".nc"
}

# Dates:  contruct time string (hh:mm:ss), and date string (yyyy-mm-dd). Use cal_year and 
# cal_month to determine the water year.  Also need to make sure that both cal_month and
# cal_day are padded with a 0 if they are less than 10...add the zero and then make a string.
time_string <- "00_00_00"                                             # alway at 00Z, GMT, UTC

# calendar month 0-pad
cal_month <- as.integer(cal_month)               # convert to integer in case passed as string
if (cal_month < 10) { cal_month <- paste("0",cal_month,sep = "") }

# calendar day 0-pad
cal_day <- as.integer(cal_day)                   # convert to integer in case passed as string
if (cal_day < 10) { cal_day <- paste("0",cal_day,sep = "") }

# now, construct date string
date_string <- paste(cal_year, cal_month, cal_day, sep = "-")
if (cal_month >= 10) {
  wy <- as.integer(cal_year) + 1
} else {
  wy <- cal_year
}

# water year (wy_yyyy)
water_year <- paste("wy", wy, sep = "_")


# output type (out,xtrm)
#    out:   standard WRF output files [default]
#           output_type_file = wrfout, 
#           output_type_dir  = wrf_out
#    xtrm:  wrfxtrm, surface diagnostic auxilary output files
#           output_type_file = wrfxtrm, 
#           output_type_dir  = wrf_xtrm
#
output_type_dir  <- paste("wrf", output_type, sep = "_")        # Associated directory  ex, wrf_out
output_type_file <- paste("wrf", output_type, sep = "")         # Associated file       ex. wrfout


# domain number (1,2)
#    1:  d01, outer domain, horizontal / temporal resolution = 3 km / 1 hr  (updated timesteps: 3hr to 1hr)
#    2:  d02, inner domain, horizontal / temporal resolution = 1 km / 1 hr  [default]
#
dom <- paste("d0", dom_num, sep = "")


# construct directory string
dir_url <- paste(sim_type, water_year, output_type_dir, dom, sep = "/")


# contruct file name
file_name <- paste(output_type_file, dom, date_string, time_string, ext, sep = "_")


# combine base url and the directory and file strings to construct full URL
data_url <- paste(base_30_year_url, dir_url, file_name, sep = "/")


# (3.2) OPeNDAP access and subsetting
# ===================================
# Open file and retrieve latitude and longitude information
cat("\n\n")
print("OPeNDAP data parameters: ")
print("-----------------------")
print(paste("Year:    ", cal_year,  sep = ""))                                 
print(paste("Month:   ", cal_month, sep = ""))                                
print(paste("Day:     ", cal_day,   sep = ""))                                  
print(paste("Domain:  ", dom_num,   sep = ""))                                  
print(paste("Field:   ", var_name,  sep = ""))                                 
print(paste("i index: ", i_index,   sep = ""))                                  
print(paste("j index: ", j_index,   sep = ""))                                  
print("Accessing file...")
nc <- nc_open(data_url)


# number of time steps (nt) have been updated to be the same for both domains, 24 (1hr timesteps).
nt <- 24

# start indices (X,Y,T)
start_indices <- c(i_index, j_index, 1)

# count indices (X,Y,T)
count_indices <- c(1, 1, nt)


# make the call
if (i_index == -1 & j_index == -1) {                       # download type: grid
  print("Downloading grid time series...")
  var_ts <- ncvar_get(nc, var_name)
  
} else if (i_index != -1 & j_index != -1) {                 # download type: point                   
  print("Downloading point time series...")  
  var_ts <- ncvar_get(nc, var_name, start = start_indices, count = count_indices)

} else {                                                     # download type: error
  error_string <- paste0("Invalid i,j indicies: i = ",i_index,", j = ",j_index,".")
  stop(error_string)
}


# close netCDF file
print("Data retrieved.  Closing file...")
nc_close(nc)
print("File closed.")

# if point, display
if (i_index != -1 & j_index != -1) {
  print("Displaying time series...")
  print(var_ts)            # display
  Sys.sleep(1)             # sleep for 1 second so display can be seen briefly
}

# return the time series
return(var_ts)
}

