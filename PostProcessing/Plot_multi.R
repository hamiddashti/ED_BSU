

setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/xml_test/xml_test4/')
df5<-ed_out(y1=2010,m1=01,d1=01,y2=2014,m2=12,d2=29,pfx = "hhh-D")
save(df5,file='df5.rda')

setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/holinv6/')
df6<-ed_out(y1=2010,m1=01,d1=01,y2=2014,m2=12,d2=30,pfx = "hhh-D")
save(df6,file='df6.rda')

setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/holinv7/')
df7<-ed_out(y1=2010,m1=01,d1=01,y2=2014,m2=12,d2=30,pfx = "hhh-D")
save(df7,file='df7.rda')

setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/holinv8/')
df8<-ed_out(y1=2010,m1=01,d1=01,y2=2014,m2=12,d2=30,pfx = "hhh-D")
save(df8,file='df8.rda')

setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/holinv9/')
df9<-ed_out(y1=2010,m1=01,d1=01,y2=2014,m2=12,d2=30,pfx = "hhh-D")
save(df9,file='df9.rda')

setwd('/home/hamiddashti/bcal/Data02/bcal/Personal/hamid/ed/test/holinv10/')
df10<-ed_out(y1=2010,m1=01,d1=01,y2=2014,m2=12,d2=30,pfx = "hhh-D")
save(df10,file='df10.rda')

t5 = df5$dmean_gpp_co
tmp=1:length(t5)
t7 = df7$lai_co
t8 = df8$lai_co
t9 = df9$lai_co
t10 = df10$lai_co

plot (tmp,t5,ty='l')
lines(tmp,t7,col='red')
lines(tmp,t8,col='green')
lines(tmp,t9,col='brown')
lines(tmp,t10,col='blue')


t2 = df2$lai_co
t3 = df3$lai_co
t4 = df4_xml$agb_co

tmp=1:length(t1)


lines(tmp,t2,col='green')
lines(tmp,t3,col='blue')
