rm(list = ls())
out <- vector(mode="character", length=365)

for (i in 1:365){
  
out[i] <- paste0("l1", " ", "(o",i,")1:11")  
  
  
}

x <- t(out)
