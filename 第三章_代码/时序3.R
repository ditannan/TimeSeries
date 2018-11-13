setwd("C:/Users/dell/Desktop/时间序列r实现")
#-------------模拟----------
# R code for AR(1) process
library(fBasics)
library(stats)
library(tseries)
# simulate a ma process with order 2
y=arima.sim(n = 5000, list(ma = c(0.5,0.2)),sd = 1) 
##模拟一个系数为0.5，0.2，残差项标准差为1的时间序列

fit <- arima(y, order = c(0,0,2),include.mean=F)
ypred=predict(fit, n.ahead = 6)


# simulate a ar process with order 2
y=arima.sim(n = 5000, list(ar = c(0.5,0.2)),sd = 1)
fit <- arima(y, order = c(2,0,0),include.mean=F)
ypred=predict(fit, n.ahead = 6)

# simulate a arma(2,2)
y=arima.sim(n = 5000, list(ar = c(0.5,0.2),ma=c(0.2,0.3)),sd = 1)
fit <- arima(y, order = c(2,0,2),include.mean=F)
ypred=predict(fit, n.ahead = 6)



##比较AR和MA时间序列的ACF和PACF
sim.ar=arima.sim(n=1000,list(ar=c(0.4,0.4)),sd=1)
sim.ma=arima.sim(n=1000,list(ma=c(0.4,0.4)),sd=1)
par(mfrow=c(2,2))
acf(sim.ar,main="ACF of AR(2)")
acf(sim.ma,main="ACF of MA(2)")
pacf(sim.ar,main="PACF of AR(2)")
pacf(sim.ma,main="PACF of MA(2)")

#parameter estimation arma(p,q)
sim.arma=arima.sim(n=1000,list(ar=c(0.4,0.4),ma=c(0.4,0.4)),sd=1);
par(mfrow=c(2,1))
acf(sim.arma,main="ACF of ARMA(2,2)")
pacf(sim.arma,main="PACF of ARMA(2,2)")
fit <- arima(sim.arma, order = c(2,0,2),include.mean=F)
tsdiag(fit) # It will generally plot the residuals, 
# the autocorrelation function of the residuals,
# and the p-values of a Portmanteau test 
Box.test(fit$residuals)

#-------------数据----------
x=read.csv("ex3_13.csv",header=FALSE)
xx=x[,1]
par(mfrow=c(1,1))
plot(xx,type="o")
install.packages("TSA")
library(TSA)
#ma model
t_ma=box_test(xx,12)
acf(xx) 
mafit=arima(xx,order=c(0,0,2))
mafit
acf(residuals(mafit))###或者用acf(mafit$residuals)两条命令的结果相同
Box.test(residuals(mafit),lag=24,type="Ljung")
xx.pred=predict(mafit, n.ahead = 6) ###预测###
plot(xx,type="l",xlim=c(0,80),ylim=c(0,80))
lines(xx.pred$pred,col="red")
lines(xx.pred$pred+2*xx.pred$se,col="red",lty=3)
##lty 控制连线的线型,可以是整数（1: 实线，2: 虚线，3: 点线，4: 点虚
##线，5: 长虚线，6: 双虚线）
lines(xx.pred$pred-2*xx.pred$se,col="red",lty=3)


#ar model
pacf(xx)  ###可以判定此序列为AR(1)
m1=ar(xx,method="mle") ##利用AIC准则找AR的阶
m1$order ###判定可建立AR(2)
arfit=arima(xx,order=c(1,0,0)) ##估计AR(1)参数
arfit ##在R中，intercept代表序列的均值，常数项可以利用(1+0.4191)*51.2658来求得
sqrt(arfit$sigma2) ##计算残差序列的标准差
par(mfrow=c(2,1))  #图的排版
acf(residuals(arfit), type = "partial")###或者 pacf(residuals(arfit))
Box.test(residuals(arfit),lag=6,type="Ljung") ###残差想为随机的，故而AR(1)可以被采用
arfit1=arima(xx,order=c(2,0,0)) ##估计AR(2)参数
arfit1  
Box.test(residuals(arfit1),lag=6,type="Ljung") ##AR(1)与AR(2)都可以，不同的标准得到不同的结果


mafit.ml=arima(xx,order=c(0,0,2),method="ML")  
mafit.ml
mafit.css=arima(xx,order=c(0,0,2),method="CSS");
mafit.css
mafit.cssml=arima(xx,order=c(0,0,2),method="CSS-ML");   #拟合方法：似然函数，最小二乘
mafit.cssml

###如果模拟出来的模型某一个系数不显著###
###假设模拟了一个AR（3）的模型，其中滞后二期的系数不显著，则可以利用
###m=arima(xx,order=c(3,0,0),fixed=c(NA,0,NA,NA))的函数将滞后二期的系数设为0，而后重新进行估计
#疏系数模型



