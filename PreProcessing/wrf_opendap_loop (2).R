wrf_opendap_loop <- function(yr1, mn1, dy1, yr2, mn2, dy2, dom_num, var_name, i_index, j_index) {

# *************************************************************************************************
#
# FILE:     wrf_opendap_loop.R
# AUTHOR:   Matt Masarik      (MM)
# VERSION:  0    05/31/2016    MM   Initial version
#           1    06/01/2016    MM   First version
#           2    06/17/2016    MM   Script -> function. Output: list of matrices -> matrix
#           2.1  06/20/2016    MM   Take input i_index, j_index  
#           2.2  11/11/2016    MM   Implemented full grid download option [i = -1, j = -1]
#
# USAGE:    ts_array <- wrf_opendap_loop(yr1,mn1,dy1,yr2,mn2,dy2,dom_num,var_name,i_index,j_index)
#
#               where,    yr1      = start year     <yyyy>
#                         mn1      = start month    <mm>
#                         dy1      = start day      <dd>  
#                         yr2      = finish year    <yyyy>  
#                         mn2      = finish month   <mm>
#                         dy2      = finish day     <dd>
#                         dom_num  = domain number  <n>  (n:1,2) [default = 2,   1 km resolution]
#                         var_name = variable name  <'string'>   [default = "T2", 2M Temperature]
#                         i_index  = <ii..i>     ii..i = WRF grid i index (longitude direction)
#                         j_index  = <jj..j>     jj..j = WRF grid j index (latitude direction)  
#
#               Output access syntax,
#                                      point:  ts_array[time_step_per_day, day]
#                                      grid:   ts_array[i_index, j_index, time_step_per_day, day]
#           
# PURPOSE:  Loop over OPeNDAP time series (ts) extraction function, wrf_opendap_ts(...).  Works on
#           a point for (i_index > 0, j_index > 0), or the full grid (i_index = -1, j_index = -1).
#
#
# OUTLINE:  (1) Parameters
#           (2) Set Up Environment
#           (3) Function Definitions
#               (3.1) days_in_period
#           (4) Main
#               (4.1) Get time dimensions for output matrix 
#               (4.2) Initialize output variables
#               (4.2) Initialize loop variables
#               (4.3) Loop over days in period
#  
# NOTES:    [lon=339, lat=289]  
#
# *************************************************************************************************

# -------------------------------------------------------------------------------------------------
# (1) Parameters
# -------------------------------------------------------------------------------------------------
i_grid_pts <- 339          # number of i wrf grid points (longitude)
j_grid_pts <- 289          # number of j wrf grid points (latitude)
  
  

# -------------------------------------------------------------------------------------------------
# (2) Set Up Environment
# -------------------------------------------------------------------------------------------------
source('wrf_opendap_ts.R')

##install.packages('pracma')       # Package 'Practical Numerical Math Functions'.
library(pracma)                    # Contains the ability to compare two strings




# -------------------------------------------------------------------------------------------------
# (3) Function definitions
# -------------------------------------------------------------------------------------------------

# (3.1) days_in_period(...) - calculates the number of days in specified time period
# ==================================================================================
days_in_period <- function(start_yr, start_mn, start_dy, finish_yr, finish_mn, finish_dy) {

  # 0-pad months and days, convert to string (so it keeps it's zero)
  #   start month
  start_mn  <- as.integer(start_mn)
  if (start_mn  < 10) { start_mn  <- paste("0", start_mn,  sep = "") }
  
  #   start day
  start_dy  <- as.integer(start_dy)
  if (start_dy  < 10) { start_dy  <- paste("0", start_dy,  sep = "") }
  
  # finish month
  finish_mn <- as.integer(finish_mn)
  if (finish_mn < 10) { finish_mn <- paste("0", finish_mn, sep = "") }
  
  # finish day
  finish_dy <- as.integer(finish_dy)
  if (finish_dy < 10) { finish_dy <- paste("0", finish_dy, sep = "") }
  
  # create start/finish date strings (yy-mm-dd)
  start_date_string  <- paste(start_yr, start_mn, start_dy, sep = "-")
  finish_date_string <- paste(finish_yr, finish_mn, finish_dy, sep = "-")
  
  # get start/finish datetime objects
  start_date  <- strptime(start_date_string,  format = "%Y-%m-%d")
  finish_date <- strptime(finish_date_string, format = "%Y-%m-%d")
  
  # get the number of days in the period from start_date to finish_date
  num_days_in_period <- difftime(finish_date, start_date, units = "days")

  # return number of days in period (the "+ 1" accounts for 1 day offset)
  return(as.integer(num_days_in_period) + 1)
}


# -------------------------------------------------------------------------------------------------
# (4) Main
# -------------------------------------------------------------------------------------------------

# (4.1) Get time dimensions for output matrix
# ===========================================
# output matrix dimensions:
#     rows = number of time steps in a day
#     cols = number of days

# number of time steps: get number of time steps (update: d01 & d02 now have same temporal resolution)
#   d01: 24 time steps (1 hour increments)
#   d02: 24 time steps (1 hour increments)
num_time_steps <- 24

# number of days: get the number of days in the period from input dates
num_days <- days_in_period(yr1, mn1, dy1, yr2, mn2, dy2)



# (4.2) Initialize output arrays
# ==============================
# get dimensions based on download type:  point (i > 0, j > 0),  grid (i = -1, j = -1)
if (i_index == -1 & j_index == -1) {                      # grid
  array_dims <- c(i_grid_pts, j_grid_pts, num_time_steps, num_days)
  
} else if (i_index != -1 & j_index != -1) {               # point
  array_dims <- c(num_time_steps, num_days)
  
} else {                                                  # error
  error_string <- paste0("Invalid i,j indicies: i = ",i_index,", j = ",j_index,".")
  stop(error_string)
}

# initialize output array to 0's,
#    point:  ts_array[rows = time_step_per_day, cols = day]
#    grid:   ts_array[i_index, j_index, time_step_per_day, day]
ts_array <- array(0, array_dims)




# (4.3) Initialize loop variables
# ===============================
# month 0-pad
mn1 <- as.integer(mn1)
if (mn1  < 10) { mn1  <- paste("0", mn1,  sep = "") }

# day 0-pad
dy1 <- as.integer(dy1)
if (dy1  < 10) { dy1  <- paste("0", dy1,  sep = "") }

# initialize loop date variables
curr_year  <- yr1
curr_month <- mn1
curr_day   <- dy1
date_str   <- paste(curr_year, curr_month, curr_day, sep = "-")  # yyyy-mm-dd
date_fmt   <- "%Y-%m-%d"                                         # use same format in date calls



# (4.4) Loop over days in period
# ==============================
for (dy in 1:num_days) {
  
  # display the current date to screen  
  cat("\n\n")
  print(paste("Current date: ", date_str,  sep = "")) 
  
  # the calls.. download time series:  grid or point
  if (i_index == -1 & j_index == -1) {
    # download grid time series, put 3-D array at 'dy' index of 4-D array 
    ts_array[,,,dy] <- wrf_opendap_ts(curr_year,curr_month,curr_day,dom_num,var_name,i_index,j_index)
    
  } else if (i_index != -1 & j_index != -1) {
    # download point time series, put into matrix as a column
    ts_array[,dy] <- wrf_opendap_ts(curr_year,curr_month,curr_day,dom_num,var_name,i_index,j_index)
  
  } else {
    error_string <- paste0("Invalid i,j indicies: i = ",i_index,", j = ",j_index,".")
    stop(error_string)
  }
  
  
  
  # get next days date values for year, month, day
  date_str   <- format(as.Date(date_str, format = date_fmt) + 1, format = date_fmt)
  curr_year  <- substr(date_str,1,4)        # year
  curr_month <- substr(date_str,6,7)        # month
  curr_day   <- substr(date_str,9,10)       # day
}

# return the matrix, ts_array[hours,days]
return(ts_array)
}

