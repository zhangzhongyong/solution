
library(xlsx)
require(reshape2)
library(ggplot2)
library(xlsx)

##dec 为偶数长度向量
##m_select 为偶数个参数选择
fftExpand<-function(dec,m_select){
  d<-fft(dec)
  m=length(dec)
  M = floor((m+1)/2)
  
  a0=d[1]/m
  an = 2*Re(d[c(2:M)])/m
  a6=d[(M+1)]/m
  bn=-2*Im(d[c(2:M)])/m
  
  n=c(1:length(an))
  temp1<-c(rep(1,m_select/2-1),rep(0,(length(an)-m_select/2+1)))
  temp2<-ifelse(m_select==m,1,0)
  fReturn<-function(x){
    y <-a0 + sum(temp1*an*cos(2*pi*n*x/(m)))+ sum(temp1*bn*sin(2*pi*n*x/(m)))+
    a6*cos(temp2*2*pi*M*x/(m))
    return(Re(y))
  }
  x<-c(0:(m-1))
  y<-sapply(x,fReturn)
  return(y)
}

complexDist<-function(v1,v2){
  return(mean(abs((v1-v2))^2))  
}













