My_EOS <- function(df){
  
  
  ##################################################################################
  # This script calculates tge phenology based on the IEEE paper. 
  # J. Chen et al., "A Simple Method for Detecting Phenological Change From Time Series of Vegetation Index," in IEEE Transactions on Geoscience and Remote Sensing, vol. 54, no. 6, pp. 3436-3449, June 2016.
  # doi: 10.1109/TGRS.2016.2518167
  
  # Inputs:   
  # df --> which is NA filled smoothed MODIS data (e.g. 8 days gpp) 
  # the df containd a column called dates with "2017-05-17" standard format and a column of values
  # Note this script is desigend for 8 days (Resampling part)
  
  # Outputs: 
  # tr --> the SOS vector for a time series
  #################################################################################
  
  library(signal)
  library(zoo)
  library(prospectr)
  library(data.table)
  library(pspline)
  
  t1 <- substring(df$dates[1],1,4)   # Year 1
  t2 <- substring(df$dates[dim(df)[1]],1,4) # Year 2
  n <- as.numeric(t2)-as.numeric(t1)+1  # number of years
  y_seq <- seq(t1,t2)
  
  list <- matrix(nrow = 366,ncol = n) # Put each year in a column of this matrix
          for (dfyear in 1:n){
            # ------------------------------------------------------------------------------
            # First disaggregate 8 days data to daily for each year based on spline function
            # Note: we DON'T extrapolate! just interpolation between first and last observation/simulation
            # Thus, naturally we get NA's due to no observation (e.g. cloud cover etc)
            
            start <- paste0(y_seq[dfyear],"-01-01")                       # first day of the year
            end <- paste0(y_seq[dfyear],"-12-31")                         # laste day of the year
            tmp <- subset(df, dates>= start & dates < end)           
            d1 <- as.numeric(strftime(tmp$dates, format = "%j"))     # Get the day of the year for each observation
            daily_date <- seq(as.Date(tmp$dates[1]), as.Date(tmp$dates[dim(tmp)[1]]), "days") # daily dates between first and last observation
            d2 <- as.numeric(strftime(daily_date, format = "%j")) # DOY for the observation range
            f <- splinefun(d1,tmp$GPP,method = "fmm")    # Fit the spline function
            daily_gpp <- f(d2)
            list[d2[1]:d2[length(d2)],dfyear] <- daily_gpp  # We get NA values in days out of the range of interpolation. 
            # -----------------------------------------------------------------------------
            
          }
  
  doy <- seq(1:365)
  ref.curve <- rowMeans(list)[2:366]           # this is the reference curve as highlited in the IEEE paper
                                               # Note that we introduce 1 day uncertaintiy by removeing the first day
  ref <- as.data.frame(cbind(doy,ref.curve))
  colnames(ref) <- c("doy","curve")
  max.gpp <- which(ref$curve == max(ref$curve,na.rm = TRUE))  # day of maximum greeness
  
  # Segment used for SOS retrieval: the first day with no NA value to the max greeness
  # We also drop the first date (make ref year 365 days) However this introduce +- one day uncertainity to the SOS estimation
  I <- which(!is.na(ref$curve))
  endday <-as.numeric(I[length(I)])   # last day with no NA in the reference curve
  ref_segment <- ref$curve[max.gpp:endday]                             
  day.ref <- ref$doy[max.gpp:endday]                             
  ref_segment_df <- as.data.frame(cbind(day.ref,ref_segment))
  colnames(ref_segment_df) <- c("doy","curve")
  
  #  --------------------------- Find the EOS for the ref segment ------------------
  # The EOS is just the maximum of the first derivative of the segment (based on spline )
  # To compare derivitive results use "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/predict.smooth.spline.html" 
  
  derv1 <- predict(sm.spline(ref_segment_df$doy,ref_segment_df$curve),ref_segment_df$doy,1)
  
  I2 <- which(derv1>=0)[1]    # This is the DOY when sagebrush start to flower again; change in the sign of derivative
  
  derv2 <- predict(sm.spline(ref_segment_df$doy[I2:length(derv1)],ref_segment_df$curve[I2:length(derv1)]),
                   ref_segment_df$doy[I2:length(derv1)],1)
  #lines(ref_segment_df$doy[I2:length(derv1)],derv2)
  
  I3 <- which(derv2==min(derv2))
  
  #tr1 <- ref_segment_df$doy[which(derv1==min(derv1))]
  #tr2 <- ref_segment_df$doy[I2+I3-1]   # The second inflation point
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! tr1 adnEstimated from Flux tower data !!!!!!!
  tr1<-157 
  tr2 <- 243
  #plot(ref_segment_df)
  #lines(ref_segment_df$doy,derv1*50)
  
  
  # ------------------ Calculate the cross-correlation coefficients ---------------
  
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # just to test the code! Shifting a test curve artificially 
  #test <- data.table::shift(list[,1], n=-25,fill = 0)
  #test.curve <- test[endday:max.gpp]
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  tp.doy1 <- rep(NA, n)
  tp.date1 <- rep(NA, n)
  
  
  for (eachyear in 1:n){
    test.curve <- list[max.gpp:endday,eachyear] 
    good.days <-ref_segment_df$doy[!is.na(test.curve)]   # remove NAs from the test curve if any
    good.test <- test.curve[!is.na(test.curve)]
    good.ref <- ref_segment_df$curve[!is.na(test.curve)]
    #plot(good.days,good.test,col="red",ty="l")           #Some plotting to make sure if its ok
    #lines(good.days,good.ref,col="blue")
    
    
    R <- rep(NA, 61)  # shift 30 days to the right and 30 days to the left + no shift = 61
    
    # ------------------ Calculate R with NO shift to the left or right ----------------------
    
    overlap_days_origin <- length(good.ref) 
    Di <- rep(NA,overlap_days_origin)
    
    for (j in 1:overlap_days_origin) {
      ti=good.days[j]
      
      if (ti==tr1) {
        Di[j] <- 1
        next
      } else {
        Di[j] <- 1/((ti-tr1)^2)
        
      }
    }
    wi <- Di/sum(Di)  
    #plot(good.days,wi*5)
    #lines(good.days,good.ref)
    
    yt.w <- mean(wi * good.test)
    yr.w <- mean(wi * good.ref)
    
    numerator <- sum(wi*(good.test - yt.w)*(good.ref - yr.w))
    denominator <- sqrt(sum(wi*((good.test-yt.w)^2)) * sum(wi*((good.ref-yr.w)^2)))
    R[(31)] <- numerator/denominator
    
    # ----------------- Shift to the left ------------------------------------------
    
    for (i in 1:30){     # Shift to the left
    
      test.shift <- data.table::shift(good.test, n=-i)
      new.test <- test.shift[which(!is.na(test.shift))]
      newday <- seq((good.days[1]), good.days[length(good.days)-i])
      new.ref <- good.ref[1:(length(good.ref)-i)]
      #plot(newday,new.ref)
      #lines(newday,new.test)
      
      overlap_days <- length(good.ref)-i 
      Di <- rep(NA,overlap_days)
      
      for (j in 1:overlap_days) {
        ti=newday[j]
        
        if (ti==tr1) {
          Di[j] <- 1
          next
        } else {
          Di[j] <- 1/((ti-tr1)^2)
          
        }
      }
      wi <- Di/sum(Di)  
      #lines(newday,wi*5)
      
      yt.w <- mean(wi * new.test)
      yr.w <- mean(wi * new.ref)
      
      numerator <- sum(wi*(new.test - yt.w)*(new.ref - yr.w))
      denominator <- sqrt(sum(wi*((new.test-yt.w)^2)) * sum(wi*((new.ref-yr.w)^2)))
      R[(31-i)] <- numerator/denominator
    }
    
    #plot(R)
    #which(R==max(R,na.rm = TRUE))
    
    #----------------------- Shift to the right -----------------------------------
    
    for (i in 1:30){
      
      
      test.shift <- data.table::shift(good.test, n=i)
      new.test <- test.shift[which(!is.na(test.shift))]
      newday <- seq((good.days[1]+i), good.days[length(good.days)])
      new.ref <- good.ref[(i+1):length(good.ref)]
      #plot(newday,new.test)
      #lines(newday,new.ref)
      
      
      overlap_days <- length(good.ref)-i 
      Di <- rep(NA,overlap_days)
      
      for (j in 1:overlap_days) {
        ti=newday[j]
        
        if (ti==tr1) {
          Di[j] <- 1
          next
        } else {
          Di[j] <- 1/((ti-tr1)^2)
          
        }
      }
      wi <- Di/sum(Di)  
      #lines(newday,wi*5)
      
      yt.w <- mean(wi * new.test)
      yr.w <- mean(wi * new.ref)
      
      numerator <- sum(wi*(new.test - yt.w)*(new.ref - yr.w))
      denominator <- sqrt(sum(wi*((new.test-yt.w)^2)) * sum(wi*((new.ref-yr.w)^2)))
      R[31+i] <- numerator/denominator
    }
    m.position <- seq(-30,30)  # Match positions for 30 days
    #plot(m.position,R)
    MaxR <- which(R==max(R,na.rm = TRUE))
    mp <- m.position[MaxR]
  tp.doy1[eachyear] <- tr1-mp
  tp.date1[eachyear] <- as.Date((tp.doy1[eachyear]-1), origin = paste0(y_seq[eachyear],"-01-01","%d%b%Y")) # 
  }
  
  
  ## -------- using tr2 ----------- 
  tp.doy2 <- rep(NA, n)
  tp.date2 <- rep(NA, n)
  
  
  for (eachyear in 1:n){
    test.curve <- list[max.gpp:endday,eachyear] 
    good.days <-ref_segment_df$doy[!is.na(test.curve)]   # remove NAs from the test curve if any
    good.test <- test.curve[!is.na(test.curve)]
    good.ref <- ref_segment_df$curve[!is.na(test.curve)]
    #plot(good.days,good.test,col="red",ty="l")           #Some plotting to make sure if its ok
    #lines(good.days,good.ref,col="blue")
    
    
    R <- rep(NA, 61)  # shift 30 days to the right and 30 days to the left + no shift = 61
    
    # ------------------ Calculate R with NO shift to the left or right ----------------------
    
    overlap_days_origin <- length(good.ref) 
    Di <- rep(NA,overlap_days_origin)
    
    for (j in 1:overlap_days_origin) {
      ti=good.days[j]
      
      if (ti==tr2) {
        Di[j] <- 1
        next
      } else {
        Di[j] <- 1/((ti-tr2)^2)
        
      }
    }
    wi <- Di/sum(Di)  
    #plot(good.days,wi*5)
    #lines(good.days,good.ref)
    
    yt.w <- mean(wi * good.test)
    yr.w <- mean(wi * good.ref)
    
    numerator <- sum(wi*(good.test - yt.w)*(good.ref - yr.w))
    denominator <- sqrt(sum(wi*((good.test-yt.w)^2)) * sum(wi*((good.ref-yr.w)^2)))
    R[(31)] <- numerator/denominator
    
    # ----------------- Shift to the left ------------------------------------------
    
    for (i in 1:30){     # Shift to the left
      
      test.shift <- data.table::shift(good.test, n=-i)
      new.test <- test.shift[which(!is.na(test.shift))]
      newday <- seq((good.days[1]), good.days[length(good.days)-i])
      new.ref <- good.ref[1:(length(good.ref)-i)]
      #plot(newday,new.ref)
      #lines(newday,new.test)
      
      overlap_days <- length(good.ref)-i 
      Di <- rep(NA,overlap_days)
      
      for (j in 1:overlap_days) {
        ti=newday[j]
        
        if (ti==tr2) {
          Di[j] <- 1
          next
        } else {
          Di[j] <- 1/((ti-tr2)^2)
          
        }
      }
      wi <- Di/sum(Di)  
      #lines(newday,wi*5)
      
      yt.w <- mean(wi * new.test)
      yr.w <- mean(wi * new.ref)
      
      numerator <- sum(wi*(new.test - yt.w)*(new.ref - yr.w))
      denominator <- sqrt(sum(wi*((new.test-yt.w)^2)) * sum(wi*((new.ref-yr.w)^2)))
      R[(31-i)] <- numerator/denominator
    }
    
    #plot(R)
    #which(R==max(R,na.rm = TRUE))
    
    #----------------------- Shift to the right -----------------------------------
    
    for (i in 1:30){
      
      
      test.shift <- data.table::shift(good.test, n=i)
      new.test <- test.shift[which(!is.na(test.shift))]
      newday <- seq((good.days[1]+i), good.days[length(good.days)])
      new.ref <- good.ref[(i+1):length(good.ref)]
      #plot(newday,new.test)
      #lines(newday,new.ref)
      
      
      overlap_days <- length(good.ref)-i 
      Di <- rep(NA,overlap_days)
      
      for (j in 1:overlap_days) {
        ti=newday[j]
        
        if (ti==tr2) {
          Di[j] <- 1
          next
        } else {
          Di[j] <- 1/((ti-tr2)^2)
          
        }
      }
      wi <- Di/sum(Di)  
      #lines(newday,wi*5)
      
      yt.w <- mean(wi * new.test)
      yr.w <- mean(wi * new.ref)
      
      numerator <- sum(wi*(new.test - yt.w)*(new.ref - yr.w))
      denominator <- sqrt(sum(wi*((new.test-yt.w)^2)) * sum(wi*((new.ref-yr.w)^2)))
      R[31+i] <- numerator/denominator
    }
    m.position <- seq(-30,30)  # Match positions for 30 days
    #plot(m.position,R)
    MaxR <- which(R==max(R,na.rm = TRUE))
    mp <- m.position[MaxR]
    tp.doy2[eachyear] <- tr2-mp
    tp.date2[eachyear] <- as.Date((tp.doy2[eachyear]-1), origin = paste0(y_seq[eachyear],"-01-01","%d%b%Y")) # 
  }
  
  tp.date1 <- as.Date(tp.date1)
  tp.date2 <- as.Date(tp.date2)
  
  EOS <- data.frame(tp.doy1,tp.date1,tr1,tp.doy2,tp.date2,tr2)
  
}
################## End of function #########################


