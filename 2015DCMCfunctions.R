one.day<-chron(times=data0103$IVR_entry)
daytime[,3:100]<-0

for (a in 10701:10925){
  data.sub<-subset(data,data$Call_date==a)
  one.day<-chron(times=data.sub$IVR_entry)
for (b in 1:length(one.day))
  for (c in 1:48) {
   if (daytime[c,1] <= one.day[b] & daytime[c+1,1] > one.day[b] & is.na(daytime[c,1] <= one.day[b] & daytime[c+1,1] > one.day[b]) == FALSE)
      daytime[c,a-10590] <- daytime[c,a-10590] + 1
}
daytime[48,a-10590]<-length(one.day)-sum(daytime[,a-10590])
}

for i in (2:335)
  names(daytime)[i]<-dat[i-1,]

for (i in seq(50,170,by=7)){
  plot(daytime[,i],ylab=names(daytime)[i])
}

for (i in 14:335){
  daytime.ts.all<-c(daytime.ts.all,daytime[,i])
}

forecast.time<-matrix(nrow = 48,ncol = 31)
forecast.time[1:48,1:31]<-0
for (i in 1:48){
time.fit<-arima(as.numeric(daytime.wd[i,14:335]),order=c(7,1,1),seasonal=list(order=c(1,1,0), period=7))
#plot.forecast(forecast.Arima(time000000.fit,h=31))
forecast.time.fit<-(forecast.Arima(time.fit,h=31))
forecast.time[i,1:31]<-forecast.time.fit$mean[1:31]}

forecast.time.round<-round(forecast.time)

for (i in 1:48){
  for (j in 1:31){
    if (forecast.time.round[i,j]<0){
    forecast.time.round[i,j]=0}}}
  

ser.time=c()
for (i in 10592:10925){
  ser.time[i-10591]=sum(subset(data.agent,data.agent$Call_date==i)[,16])}