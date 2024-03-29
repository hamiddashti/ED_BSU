#==========================================================================================#
#==========================================================================================#
#      This function appends the patch level either at the beginning or the end of each    #
# cohort.                                                                                  #
#------------------------------------------------------------------------------------------#
append.patch <<- function(ipa,ipaco,xpa,xco,left=TRUE){
   if (length(ipaco) == 0){
      xpaco = xpa
   }else{
      xpa            = mapply(FUN=list,xpa,SIMPLIFY=TRUE)
      names(xpa)     = ipa
      xcofull        = replicate(n=length(ipa),list(vector(length=0)))
      names(xcofull) = ipa
      xco            = split(x=xco,f=ipaco)
      idx            = match(names(xco),names(xcofull))
      xcofull[idx]   = xco
      if (left){
         xpaco = unlist(mapply(FUN=c,xpa,xcofull,SIMPLIFY=FALSE))
      }else{
         xpaco = unlist(mapply(FUN=c,xcofull,xpa,SIMPLIFY=FALSE))
      }#end if
   }#end if
   return(xpaco)
}#end append.patch
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      This function finds the total absorbed by each layer.  This assumes that                              #
#------------------------------------------------------------------------------------------#
fill.unresolved <<- function(ipaco,x,use){
   unlist( mapply( FUN      = function(x,use){
                                 #----- Use variable from . -------------------------------#
                                 xout = x
                                 no   = which(! use)
                                 yes  = which(  use)
                                 if (any(use) && any(! use)){
                                    prev.yes = mapply( FUN       = function(no,yes){
                                                                      max(0,yes[yes<no])
                                                                   }#end function
                                                     , no        = no
                                                     , MoreArgs = list(yes = yes)
                                                     )#end mapply
                                    xout[no] = ifelse(prev.yes > 0, x[prev.yes], 0)
                                 }else if ( all (! use)){
                                    xout = rep(0,times=length(x))
                                 }#end if
                                 return(xout)
                                 #---------------------------------------------------------#
                              }#end function
                 , x        = split(x=x   ,f=ipaco)
                 , use      = split(x=use ,f=ipaco)
                 , SIMPLIFY = FALSE
                 )#end mapply
         )#end unlist
   #---------------------------------------------------------------------------------------#
}#end fill.unresolved
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      This function finds the total absorbed by each layer.  This assumes that the data   #
# have been previously filled!                                                             #
#------------------------------------------------------------------------------------------#
layer.absorption <<- function(ipaco,down,up){
   #----- Apply the internal function to each patch. --------------------------------------#
   unlist( mapply( FUN      = function(down,up){
                                 #----- Find the absorption for 1 layer. ------------------#
                                 n   = length(down)
                                 ans = c(0,down[-n]-down[-1]+up[-1]-up[-n])
                                 return(ans)
                                 #---------------------------------------------------------#
                              }#end function
                 , down     = split(x=down,f=ipaco)
                 , up       = split(x=up  ,f=ipaco)
                 , SIMPLIFY = FALSE
                 )#end mapply
         )#end unlist
   #---------------------------------------------------------------------------------------#
}#end layer.absorption
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      This function picks the last value.  If scale.1st = TRUE, it returns the value      #
# relative to the first level.                                                             #
#------------------------------------------------------------------------------------------#
rel.last <<- function(x){
   if (length(x) == 0){
      ans = NA
   }else if (x[1] %==% 0){
      ans = NA
   }else{
      ans = x[length(x)] /x[1]
   }#end if
   return(ans)
}#end function rel.last
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      This function picks the last value.  If scale.1st = TRUE, it returns the value      #
# relative to the first level.                                                             #
#------------------------------------------------------------------------------------------#
scal.first <<- function(x,y=NULL){
   if (length(x) == 0){
      ans = x
   }else if(length(y) == 0){
      ans = x/x[1]
   }else{
      ans = x/y[1]
   }#end if
   return(ans)
}#end function scal.last
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      This function picks the last value.  If scale.1st = TRUE, it returns the value      #
# relative to the lowest level.                                                            #
#------------------------------------------------------------------------------------------#
scal.last <<- function(x,y=NULL){
   if (length(x) == 0){
      ans = x
   }else if(length(y) == 0){
      ans = x/x[length(x)]
   }else{
      ans = x/y[length(y)]
   }#end if
   return(ans)
}#end function scal.last
#==========================================================================================#
#==========================================================================================#
