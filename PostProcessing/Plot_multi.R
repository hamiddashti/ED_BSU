setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/output_phen0/')
df0<-ed_out(y1=2014,m1=01,d1=01,y2=2019,m2=12,d2=29,pfx = "hhh-D")
save(df0,file='df0.rda')

setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/output_phen-1/')
df1<-ed_out(y1=2014,m1=01,d1=01,y2=2019,m2=12,d2=29,pfx = "hhh-D")
save(df1,file='df1.rda')

setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/output_phen2/')
df2<-ed_out(y1=2014,m1=01,d1=01,y2=2019,m2=12,d2=29,pfx = "hhh-D")
save(df2,file='df2.rda')

setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/output_phen3/')
df3<-ed_out(y1=2014,m1=01,d1=01,y2=2019,m2=12,d2=29,pfx = "hhh-D")
save(df3,file='df3.rda')
setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/output_phen-0-xml/')
df4_xml<-ed_out(y1=2014,m1=01,d1=01,y2=2019,m2=12,d2=29,pfx = "hhh-D")
save(df4_xml,file='df4.rda')


t0 = df0$lai_co
t1 = df1$lai_co
t4 = df4_xml$lai_co
plot (tmp,t0,ty='l')
lines(tmp,t4,col='red')

t2 = df2$lai_co
t3 = df3$lai_co
t4 = df4_xml$agb_co

tmp=1:length(t1)


lines(tmp,t2,col='green')
lines(tmp,t3,col='blue')
