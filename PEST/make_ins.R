# script to create the instruction file

a=as.character(1096)
for (i in 1:1096){
  
  a[i] = paste("(o",i,")","1:11",sep="")
  
}
write.table(a,"ls_ins.txt",row.names = F, quote = FALSE,col.names = F)
