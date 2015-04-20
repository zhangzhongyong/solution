
source('barClass.R')
stg<-c(predict1,predict2,predict3,predict4,predict5,predict6)
n=100
m_select=4
m=6
temp<-simulatingBar(stg=stg,m_select=m_select,maxism_week=100)
