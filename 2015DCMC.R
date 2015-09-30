#***数据集处理以及第一题***#

data<-read.table("data.txt",sep="|",header=T)
data.od <- read.csv("~/Desktop/data.od.csv")
data.vt <- read.csv("~/Desktop/data.vt.csv")

data.vt$Date<-as.Date(data.vt$Date,"%m/%d/%Y")
data$Call_date<-as.Date(data$Call_date,"%Y-%m-%d")

summary(lm(data.vt$Call.Volume~data.vt$Handling_time))
#两变量之间有线性关系

plot(data.vt$Date,data.vt$Call.Volume)
#有周期

data01<-subset(data,Call_date >= "1999-01-01" & Call_date <= "1999-01-31")
data03<-subset(data,Call_date >= "1999-03-01" & Call_date <= "1999-03-31")
data0101<-subset(data01,data01$Call_date =="1999-01-01")
#有多少行数据就代表有多少Call Volume

handlt<-aggregate(cbind(data01$IVR_time,data01$queue_time,data01$servc_time),by=list(data01$Call_date),FUN=sum)
colnames(handlt)<-c("Date","IVR_time","queue_time","servc_time")


Vol=c();dat=c();Ivr=c();Que=c();Ser=c()
dat= as.Date(10592:10925,origin="1970-01-01")
dat<-as.data.frame.Date(dat)

for (i in 10592:10925){
  Vol[i-10591]=nrow(subset(data,data$Call_date==i))
  Ivr[i-10591]=sum(subset(data,data$Call_date==i)[,9])
  Que[i-10591]=sum(subset(data,data$Call_date==i)[,12])
  Ser[i-10591]=sum(subset(data,data$Call_date==i)[,16])}

callvol<-cbind(dat,Vol)     
call.viqs<-cbind(callvol,Ivr,Que,Ser)
#计算每月每日的流量估数（有略微误差）

callvol[,3]<-weekdays(callvol$dat)

callvol[1:31,4]<-c("Jan")
callvol[32:59,4]<-c("Feb")
callvol[60:90,4]<-c("Mar")
callvol[91:120,4]<-c("Apr")
callvol[121:151,4]<-c("May")
callvol[152:181,4]<-c("Jun")
callvol[182:212,4]<-c("Jul")
callvol[213:243,4]<-c("Aug")
callvol[244:273,4]<-c("Sep")
callvol[274:304,4]<-c("Oct")
callvol[305:334,4]<-c("Nov")

#按照月份查看数据是否有组别差异，无月份差异

names(callvol)<-c("date","vol","day","month")
par(mfrow = c(3,3))
plot(callvol$date[callvol$day=="Sunday"],callvol$vol[callvol$day=="Sunday"])
plot(callvol$date[callvol$day=="Saturday"],callvol$vol[callvol$day=="Saturday"])
plot(callvol$date[callvol$day=="Friday"],callvol$vol[callvol$day=="Friday"])
plot(callvol$date[callvol$day=="Thursday"],callvol$vol[callvol$day=="Thursday"])
plot(callvol$date[callvol$day=="Wednesday"],callvol$vol[callvol$day=="Wednesday"])
plot(callvol$date[callvol$day=="Tuesday"],callvol$vol[callvol$day=="Tuesday"])
plot(callvol$date[callvol$day=="Monday"],callvol$vol[callvol$day=="Monday"])

summary(aov(callvol$vol~callvol$day))
# 数据集有因为星期而产生的差异

t.test(callvol$vol[callvol$day=="Wednesday"],callvol$vol[callvol$day=="Thursday"])
t.test(callvol$vol[callvol$day=="Tuesday"],callvol$vol[callvol$day=="Thursday"])
t.test(callvol$vol[callvol$day=="Monday"],callvol$vol[callvol$day=="Thursday"])
# Wed-Thur: p=.696; Tue-Thur: p=.599; Mon-Thur: p=.896; Wed-Mon: p=.628; Wed-Tue: p=.379; Mon-Tue: p=.718

par(mfrow = c(4,3))
plot(call.viqs$dat[1:31],call.viqs$Vol[1:31])
plot(call.viqs$dat[32:59],call.viqs$Vol[32:59])
plot(call.viqs$dat[60:90],call.viqs$Vol[60:90])
plot(call.viqs$dat[91:120],call.viqs$Vol[91:120])
plot(call.viqs$dat[121:151],call.viqs$Vol[121:151])
plot(call.viqs$dat[152:181],call.viqs$Vol[152:181])
plot(call.viqs$dat[182:212],call.viqs$Vol[182:212])
plot(call.viqs$dat[213:243],call.viqs$Vol[213:243])
plot(call.viqs$dat[244:273],call.viqs$Vol[244:273])
plot(call.viqs$dat[274:304],call.viqs$Vol[274:304])
plot(call.viqs$dat[305:334],call.viqs$Vol[305:334])


#ARIMA模型
library(astsa)
library(forecast)
acf(diff(callvol$vol,differences=1))
pacf(diff(callvol$vol,differences=1))
callvol.fit <- arima(callvol$vol,order=c(7,1,0), seasonal=list(order=c(1,1,0), period=7))
pre.vol<-predict(callvol.fit,n.ahead=31)
plot(c(callvol$vol,pre.vol$pre[1:31]))
#将一星期的所有数据整合一起效果不好，需要按星期几分组数据完成

acf(diff(callvol$vol[callvol$day=="Sunday"],differences = 1),lag.max = 50)
pacf(diff(callvol$vol[callvol$day=="Sunday"],differences = 1),lag.max = 50)
# 利用acf得出p=1,pacf得出q=2
vol.fit.sun<-arima(callvol$vol[callvol$day=="Sunday"],order=c(1,1,2),seasonal=list(order=c(1,1,0), period=4))
pre.sun<-predict(vol.fit.sun,n.ahead=10)
plot(c(callvol$vol[callvol$day=="Sunday"],pre.sun$pre[1:10]),ylim=c(0,3000))
AIC(vol.fit.sun)
tsdiag(vol.fit.sun)
# tsdiag 检验需要所有p值大于蓝线，越大越好
##思路一：使用sarima，按照各种检验拟合出最佳模型
##去除缺失值，period=7，周日-周六大致有一个周期
##分星期几的组：周末一组、周二到周四一组、周五周六一组（暂时不执行）
##思路二：使用auto.arima和forecast，直接出结果看是否拟合（貌似不行）

#*最后结果：
acf(diff(callvol$vol[13:334],differences = 1),lag.max = 50)
pacf(diff(callvol$vol[13:334],differences = 1),lag.max = 50)

vol.fit<-arima(callvol$vol[13:334],order=c(7,1,2),seasonal=list(order=c(1,1,0), period=7))
pre.vol<-predict(vol.fit,n.ahead=30)
plot(c(callvol$vol[13:334],pre.vol$pre[1:30]),ylim=c(0,3000))
AIC(vol.fit)
tsdiag(vol.fit)

plot.forecast(forecast.Arima(vol.fit,h=31,level=c(99.5)))

forecast.vol
[1] 1671.4812 1565.5289  354.0341  262.5564 1524.0218 1643.0427 1717.9512 1695.6343 1502.0976
[10]  404.0928  251.2089 1534.8112 1540.5394 1721.1586 1677.6365 1529.5671  379.2249  259.5627
[19] 1524.7232 1581.8893 1715.7564 1689.9180 1518.8177  392.7621  255.9548 1533.0565 1567.5044
[28] 1721.8454 1685.1480 1525.5246  388.0946


#***第二题***#

library(chron)
data0101$IVR_entry<-chron(times=data0101$IVR_entry)

for (a in 10592:10925){
  data.sub<-subset(data,data$Call_date==a)
  one.day<-chron(times=data.sub$IVR_entry)
  for (b in 1:length(one.day))
    for (c in 1:48) {
      if (daytime[c,1] <= one.day[b] & daytime[c+1,1] > one.day[b] & is.na(daytime[c,1] <= one.day[b] & daytime[c+1,1] > one.day[b]) == FALSE)
        daytime[c,a-10590] <- daytime[c,a-10590] + 1
    }
  daytime[48,a-10590]<-length(one.day)-sum(daytime[,a-10590])
}
#把1-11月每天各时段的流量按照一定格式处理得到相应的表格

names(daytime)[2:335]<-as.character(chron(as.numeric(names(daytime)[2:335])))
daytime.weekday<-names(daytime)[2:335]

for (i in 14:335){
  daytime.ts.all<-c(daytime.ts.all,daytime[,i])
}
#构建从01/13/99-11/31/99的全天全序列

plot(daytime.ts.all,type="l")

#无法进行全部周期的预测（数据量太大以及周期太长），因此将每个星期每个时段分开预测
#如010199-113099的所有Friday的000000时段预测011299-311299的所有Friday的0000000时段预测（效果不好）

daytime.wd<-daytime
daytime.wd[49,2:335]<-weekdays(10592:10925)

daytime.fri<-daytime.wd[1:48,daytime.wd[49,2:335]=="Fri"]
time000000.fri<-as.numeric(daytime.fri[1,2:49])
#暂时无用，数据集星期几时间段的处理

#以所有天数的某一时间段来进行总体估计，周期为7
time000000.fit<-arima(as.numeric(daytime.wd[1,14:335]),order=c(7,1,1),seasonal=list(order=c(1,1,0), period=7))
plot.forecast(forecast.Arima(time000000.fit,h=31))
forecast.time000000<-(forecast.Arima(time000000.fit,h=31))
forecast.time000000$mean[1:31]
#如取334天的000000时段进行周期为7的估计

forecast.time<-matrix(nrow = 48,ncol = 31)
forecast.time[1:48,1:31]<-0
for (i in 1:48){
  time.fit<-arima(as.numeric(daytime.wd[i,14:335]),order=c(7,1,1),seasonal=list(order=c(1,1,0), period=7))
  forecast.time.fit<-(forecast.Arima(time.fit,h=31))
  forecast.time[i,1:31]<-forecast.time.fit$mean[1:31]}

AIC(time.fit)
tsdiag(time.fit)

forecast.time.round<-round(forecast.time)

for (i in 1:48){
  for (j in 1:31){
    if (forecast.time.round[i,j]<0){
      forecast.time.round[i,j]=0}}}

#把点估计小于0的设定成0
forecast.time.round
forecast.time.round[20,1:31]
#12月1日-12月31日09:30:00的每天流量
[1] 87 62 28  0 87 74 80 85 66 34  0 90 67 78 87 65 32  0 89 69 79 86 65 33  0 90 69 79 87
[30] 66 33

forecast.time.round[1:48,15]
#12月15日00:00:00-23:30:00的每天流量
[1]  2  2  0  2  0  0  1  0  1  0  0  2  3  3 18 17 29 39 47 87 72 79 74 68 68 69 60 65 61
[30] 71 73 73 47 54 49 36 41 39 29 36 16 28 38 33 30 33 30 20


#***第三题***#
data.agent<-subset(data,as.character(data$outcome)=="AGENT  ")

ser.time=c()
for (i in 10592:10925){
  ser.time[i-10591]=sum(subset(data.agent,data.agent$Call_date==i)[,16])}
handlt<-cbind(dat,ser.time)

acf(diff(handlt$ser.time,differences = 1),lag.max = 50)
pacf(diff(handlt$ser.time,differences = 1),lag.max = 50)

ht.fit<-arima(handlt$ser.time,order=c(7,1,2),seasonal=list(order=c(1,1,0), period=7))
pre.ht<-predict(ht.fit,n.ahead=31)
forecast.ht.fit<-(forecast.Arima(ht.fit,h=31))
plot.forecast(forecast.ht.fit)
AIC(ht.fit)
tsdiag(ht.fit)
