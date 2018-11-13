###1820-1869年太阳黑子年度数据时序图###
setwd("C:/Users/dell/Desktop/时间序列r实现")
install.packages("TSA")
library(TSA)
sunspot=read.table("附录1.1.csv",header = T,sep = ",")
sunspot.ts<-ts(sunspot,start=1820)
ts.plot(sunspot.ts[,2],xlab="year",ylab="sunspot",type="o")
Box.test(sunspot[,2],lag=6,type="Box")
Box.test(sunspot[,2],lag=12,type="Box")
Box.test(sunspot[,2],lag=6,type="Ljung")
Box.test(sunspot[,2],lag=12,type="Ljung")
acf(sunspot[,2])
acf(x,lag.max=6,type="correlation",plot=F)

###中国纱年产量时序图###
sha=read.table("附录1.2.csv",header = T,sep = ",")
sha.ts<-ts(sha,start=1964)
ts.plot(sha.ts[,2],xlab="year",ylab="sha",type="o")
#纯随机检验函数
box_test=function(x,n){
  Q_sta=NULL
  LB_sta=NULL
  Q_pv=NULL
  LB_pv=NULL
  for(i in 1:n){
    test1=Box.test(x,lag=i,type="Box")
    test2=Box.test(x,lag=i,type="Ljung")
    Q_sta[i]=test1$statistic
    Q_pv[i]=test1$p.value
    LB_sta[i]=test2$statistic
    LB_pv[i]=test2$p.value
   }
   result=data.frame(Q_sta,Q_pv,LB_sta,LB_pv)
   return(result)
}
t1=box_test(sha[,2],12)
acf(sha.ts[,2])

###奶牛产奶量###
milk=scan("附录1.3.txt")
milk.ts=ts(milk,start=c(1962,1),frequency=12)
ts.plot(milk.ts,xlab="time",ylab="milk")
t2=box_test(milk,12)
acf(milk)

###白噪声####
noise=rnorm(1000,mean=0,sd=1)
noise.ts=ts(noise)
ts.plot(noise.ts,xlab="time",ylab="noise")
t3=box_test(noise.ts,12)
acf(noise)




