setwd("C:/Users/dell/Desktop/时间序列r实现")
consume=scan("例4.1.txt")
ts.plot(consume,type="o") ##显示线性递增，考虑使用线性模型
t=1:39
model=lm(consume~t) ##lm用于拟合线性模型
  ##或者使用 model1=lm(consume~time(consume))
summary(model) 
abline(model,col="red") ##添加线，但是只能添加直线

###画图是可以考虑创建画图窗口
win.graph(width=4.875, height=2.5,pointsize=8)
plot(consume,type='o')
abline(model) 

index=scan("例4.2.txt")
ts.plot(index)  
##用二次模拟
t=1:130
t_2=t^2
model_1=lm(index~t+t_2)
lines(fitted(model_1),col="blue")  ## ##lines用于在图形上添加线
                                   ##fitted()用于提取模型的拟合值 
summary(model_1)  ##t不显著
##
model=lm(index~t_2)
lines(fitted(model),col="red")
summary(model)
##用三次模拟
t_3=t^3
model_2=lm(index~t+t_2+t_3)
lines(fitted(model_2),col="orange")  ##拟合结果较二次的好
summary(model_2)  ##模型及三个参数均显著


x=read.table("例4.6.txt",header=TRUE)
tem=c(x[,1],x[,2],x[,3],x[,4],x[,5],x[,6])
plot(tem,type="o")
tem.season=c() ##计算各期的季节指数
for(i in 1:12){  
  sum_i=0
  for(j in 1:6){
    sum_i=sum_i+tem[i+12*(j-1)]
  }
  tem.season[i]=(sum_i/6)/mean(tem)
}
tem.season
plot(tem.season,type="o")
tem.ts=ts(tem,frequency = 12)
plot(decompose(tem.ts))


x=read.table("例4.7.txt",header=TRUE)
sale=c(x[,1],x[,2],x[,3],x[,4],x[,5],x[,6],x[,7],x[,8])
plot(sale,type="o")
sale.season=c() ##计算各期的季节指数
for(i in 1:12){  
  sum_i=0
  for(j in 1:8){
    sum_i=sum_i+sale[i+12*(j-1)]
  }
  sale.season[i]=(sum_i/8)/mean(sale)
}
sale.season

###消除季节性因素的影响
z=rep(sale.season,8)
sale.nonseason=sale/z
ts.plot(sale.nonseason,type="o")
t=1:96
sale.lm=lm(sale.nonseason~t)
lines(fitted(sale.lm),col="red")
summary(sale.lm)  ##参数及模型均显著
##预测##
sale.predict=c()
for(i in 97:108){
  sale.predict[i]=1015.5222+20.9318*i #利用模拟的线性模型计算接下来12个月的数据
}
sale.pre.season=sale.predict*sale.season  ##带有季节性的预测值
ts.plot(sale,xlim=c(0,110),ylim=c(0,5000),type="o")   ##xlim,ylim为设定x轴和y轴的范围
lines(sale.pre.season,col="red")
abline(v=97,col="blue") ##添加直线 abline（h=,v=）h为水平线的y值；v为垂直线的x值

#decompose函数
sale.ts=ts(sale,frequency = 12)
plot(decompose(sale.ts))



