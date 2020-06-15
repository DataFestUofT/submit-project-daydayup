xx<-read.csv("/Users/doublehappy/Desktop/比赛/nihao.csv")
library(forecast)
aa<-xx$X.1
df2 <- data.frame(numbers = aa)
df2$numbers <- as.numeric(gsub(",","",df2$numbers))
df2=df2[,1]
rev(df2)
data2<-ts(rev(df2),start=2015, frequency=12)
mtrx2<-matrix(rev(df2),nrow=12,ncol=6)
colnames(mtrx2)<-seq(2015,2020)
rownames(mtrx2)<-month.abb
c_2020<-c(mtrx2[61:66])
c_2020<-c(c_2020,NA,NA,NA,NA,NA,NA)
mtrx2<-mtrx2[,-5]
mtrx2[,"2020"] <- c_2020
autoplot(ts(mtrx2),ylab="Average Concurrent Viewers", xlab="Month")

###
yy<-read.csv("/Users/doublehappy/Desktop/比赛/nihao2.csv")
a<-yy$X.1
df <- data.frame(numbers = a)
df$numbers <- as.numeric(gsub(",","",df$numbers))
df=df[,1]
rev(df)
data<-ts(rev(df),start=2015, frequency=12)

fit <- auto.arima(data, seasonal=TRUE, approximation = FALSE, stepwise = FALSE,ic="aic")
fit
fc3<-forecast(fit, h=4,level=.99)
plot(fc3)
fc3$upper < rev(df2)[63:66]

#rate of change
(mean(rev(df2)[63:66]) -  mean(rev(df2)[51:62]))/ mean(rev(df2)[51:62])
(mean(rev(df2)[63:66]) -  mean(rev(df2)[57:62]))/ mean(rev(df2)[57:62])
(mean(rev(df2)[63:66]) -  mean(rev(df2)[60:62]))/ mean(rev(df2)[60:62])
