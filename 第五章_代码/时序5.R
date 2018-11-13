setwd("C:/Users/dell/Desktop/时间序列r实现")
install.packages("TSA")
library(TSA)

#车辆数据（消除长期趋势）
x=read.table("例5.2.txt",header=TRUE)
car=x[,2]
ts.plot(car,type="o")
y=diff(car)
plot(y,type="o")
y_1=diff(car,differences=2)
plot(y_1,type="o")

#产奶数据（消除季节波动）
milk=scan("例5.3.txt")
plot(milk,type="o")
milk_1=diff(milk)
plot(milk_1,type="o")
milk_2=diff(diff(milk,12))
plot(milk_2,type="o")

#上证指数处理
library("fGarch")
library("tseries")
library("zoo")
library("forecast")
data=read.table("book3.csv",header=F, sep=",")
st1=data[,1]
x=ts(st1,frequency = 52)
par(mfrow=c(3,2))
plot(x)
plot(diff(x))
acf(x)
acf(diff(x))
pacf(x)
pacf(diff(x))

x.fit=arima(x,order = c(1,1,1))
AIC(x.fit)#AIC
AIC(x.fit,k = log(length(x)))#SBC

x.fit2=arima(x,order=c(2,0,0))
AIC(x.fit2)


x.fit3=auto.arima(x)
AIC(x.fit3)

x.fit4=auto.arima(diff(x))
AIC(x.fit4)


r.fit=garch(x.fit$residuals,order=c(0,1))
summary(r.fit)
AIC(r.fit)

#提取波动率
vola=as.matrix(r.fit$fitted.values)
v1=as.vector(vola[,1])
par(mfrow=c(1,1))
plot(v1,type="l",main="volatility of SZ index")
