weather<-read.csv("C:\\Users\\stat\\Downloads\\weather.csv")
train<-read.csv("C:\\Users\\stat\\Downloads\\train.csv")
weather.DU<-read.csv("C:\\Users\\stat\\Downloads\\weatherDU.csv")
weather.FG<-read.csv("C:\\Users\\stat\\Downloads\\weatherFG.csv")
weather.GR<-read.csv("C:\\Users\\stat\\Downloads\\weatherGR.csv")
weather.SN<-read.csv("C:\\Users\\stat\\Downloads\\weatherSN.csv")
weather.SQ<-read.csv("C:\\Users\\stat\\Downloads\\weatherSQ.csv")
weather.TS<-read.csv("C:\\Users\\stat\\Downloads\\weatherTS.csv")
weather.01<-read.csv("C:\\Users\\stat\\Downloads\\weather01.csv")
weather.ext<-read.csv("C:\\Users\\stat\\Downloads\\weatherext.csv")

weather.DU$date<-weather$date
weather.FG$date<-weather$date
weather.GR$date<-weather$date
weather.SN$date<-weather$date
weather.SQ$date<-weather$date
weather.TS$date<-weather$date
weather.01$date<-weather$date
weather.ext$date<-weather$date
store_nbr<-c(1:45)
station_nbr<-c(1,14,7,9,12,14,6,4,17,12,10,11,6,16,13,2,20,17,15,7,3,10,17,9,13,17,10,7,3,19,17,13,3,17,5,18,13,14,8,13,12,14,11,12,16)
##將所有資料合併
weather$codesumDU<-weather.DU$codesum
weather$codesumFG<-weather.FG$codesum
weather$codesumGR<-weather.GR$codesum
weather$codesumSN<-weather.SN$codesum
weather$codesumSQ<-weather.SQ$codesum
weather$codesumTS<-weather.TS$codesum
weather$codesum01<-weather.01$codesum
weather$codesumext<-weather.ext$codesum
##將所有資料合併
ptm <- proc.time()
xx<-subset(weather,station_nbr=="1")
yy<-subset(subset(train,item_nbr=="1"),store_nbr=="1")
w.s.store<-merge(xx,yy)
sale<-subset(train,store_nbr=="1")
len<-length(sale[,1])/111
zz<-rep(1:len,each=111)
sale$b<-zz
sale1<-c(rep(NA,len))
for(j in 1:len){
  sale1[j]<-sum(subset(sale,b==j)[,"units"])
}
w.s.store$units<-sale1
test13<-w.s.store
for(i in 2:45){
  z<-station_nbr[i]
  xx<-subset(weather,station_nbr==z)
  yy<-subset(subset(train,item_nbr=="1"),store_nbr==i)
  w.s.store<-merge(xx,yy)
  sale<-subset(train,store_nbr==i)
  len<-length(sale[,1])/111
  zz<-rep(1:len,each=111)
  sale$b<-zz
  sale1<-c(rep(NA,len))
  for(j in 1:len){
    sale1[j]<-sum(subset(sale,b==j)[,"units"])
  }
  w.s.store$units<-sale1
  new<-w.s.store
  test13<-rbind(test13,new)
}
proc.time() - ptm
##儲存結果
test12<-test13









library(psych)
library(nortest)
test13 <-test13[which(test13$units!="5613"),] 
test13 <-test13[which(test13$units!="3405"),] 


##########普通天氣 boxplot
test131 <- test13[which(test13$codesum01=="1"),]
test130 <- test13[which(test13$codesum01=="0"),]
summary(test131$units)
summary(test130$units)
par(mfrow=c(1,2))
test131points<-sample(test131$units,200)
test130points<-sample(test130$units,200)
boxplot(test131$units,ylim=c(0,800),xlab="weather=yes")
stripchart(test131points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(test130$units,ylim=c(0,800),xlab="weather=no")
stripchart(test130points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)



##########極端天氣 boxplot
testext1<-test13[which(test13$codesumext=="1"),]
testext0<-test13[which(test13$codesumext=="0"),]
summary(testext1$units)
summary(testext0$units)
par(mfrow=c(1,2))
testext1points<-sample(testext1$units,200)
testext0points<-sample(testext0$units,200)
boxplot(testext1$units,ylim=c(0,800),xlab="extreme weather=yes")
stripchart(testext1points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testext0$units,ylim=c(0,800),xlab="extreme weather=no")
stripchart(testext0points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)

########各極端天氣 boxplot
testDU1<-test13[which(test13$codesumDU=="1"),]
testDU0<-test13[which(test13$codesumDU=="0"),]
testFG1<-test13[which(test13$codesumFG=="1"),]
testFG0<-test13[which(test13$codesumFG=="0"),]
testGR1<-test13[which(test13$codesumGR=="1"),]
testGR0<-test13[which(test13$codesumGR=="0"),]
testSN1<-test13[which(test13$codesumSN=="1"),]
testSN0<-test13[which(test13$codesumSN=="0"),]
testSQ1<-test13[which(test13$codesumSQ=="1"),]
testSQ0<-test13[which(test13$codesumSQ=="0"),]
testTS1<-test13[which(test13$codesumTS=="1"),]
testTS0<-test13[which(test13$codesumTS=="0"),]

summary(testDU1$units)
describe(testDU1$units)
summary(testDU0$units)
describe(testDU0$units)
summary(testFG1$units)
describe(testFG1$units)
summary(testFG0$units)
describe(testFG0$units)
summary(testGR1$units)
describe(testGR1$units)
summary(testGR0$units)
describe(testGR0$units)
summary(testSN1$units)
describe(testSN1$units)
summary(testSN0$units)
describe(testSN0$units)
summary(testSQ1$units)
describe(testSQ1$units)
summary(testSQ0$units)
describe(testSQ0$units)
summary(testTS1$units)
describe(testTS1$units)
summary(testTS0$units)
describe(testTS0$units)

par(mfrow=c(3,4))

testDU1points<-sample(testDU1$units,64)
testDU0points<-sample(testDU0$units,200)
testFG1points<-sample(testFG1$units,200)
testFG0points<-sample(testFG0$units,200)
testGR1points<-sample(testGR1$units,3)
testGR0points<-sample(testGR0$units,200)
testSN1points<-sample(testSN1$units,200)
testSN0points<-sample(testSN0$units,200)
testSQ1points<-sample(testSQ1$units,26)
testSQ0points<-sample(testSQ0$units,200)
testTS1points<-sample(testTS1$units,200)
testTS0points<-sample(testTS0$units,200)

boxplot(testDU1$units,horiz = TRUE,ylim=c(0,800),xlab="DU=yes")
stripchart(testDU1points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testDU0$units,ylim=c(0,800),xlab="DU=no")
stripchart(testDU0points,vertical = TRUE, method = "jitter", pch =18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testFG1$units,ylim=c(0,800),xlab="FG=yes")
stripchart(testFG1points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testFG0$units,ylim=c(0,800),xlab="FG=no")
stripchart(testFG0points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testGR1$units,ylim=c(0,800),xlab="GR=yes")
stripchart(testGR1points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testGR0$units,ylim=c(0,800),xlab="GR=no")
stripchart(testGR0points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testSN1$units,ylim=c(0,800),xlab="SN=yes")
stripchart(testSN1points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testSN0$units,ylim=c(0,800),xlab="SN=no")
stripchart(testSN0points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testSQ1$units,ylim=c(0,800),xlab="SQ=yes")
stripchart(testSQ1points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testSQ0$units,ylim=c(0,800),xlab="SQ=no")
stripchart(testSQ0points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testTS1$units,ylim=c(0,800),xlab="TS=yes")
stripchart(testTS1points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)
boxplot(testTS0$units,ylim=c(0,800),xlab="TS=no")
stripchart(testTS0points,vertical = TRUE, method = "jitter", pch = 18, col = c("red"),bg = "bisque", add = TRUE,cex=0.5)

###主要天氣無母數分析
install.packages("nortest")
library(nortest)
head(test13)
weatherFG1a<-subset(test13,codesumFG=="1")
weatherFG0a<-subset(test13,codesumFG=="0")
weatherFG1<-weatherFG1a[,"units"]
weatherFG0<-weatherFG0a[,"units"]
ad.test(weatherFG1)
ad.test(weatherFG0)
length(weatherFG1)
length(weatherFG0)
summary(weatherFG1)
summary(weatherFG0)
wilcox.test(weatherFG1,weatherFG0 )
plot(ecdf(weatherFG1),col=2,cex=0.1)
plot(ecdf(weatherFG0), do.points=FALSE,col=4, verticals=TRUE, add=TRUE,cex=0.1)

weatherGR1a<-subset(test13,codesumGR=="1")
weatherGR0a<-subset(test13,codesumGR=="0")
weatherGR1<-weatherGR1a[,"units"]
weatherGR0<-weatherGR0a[,"units"]
shapiro.test(weatherGR1)
ad.test(weatherGR0)
length(weatherGR1)
length(weatherGR0)
summary(weatherGR1)
summary(weatherGR0)
wilcox.test(weatherGR1,weatherGR0 )
plot(ecdf(weatherGR1),xlim=range(0,500),col=2)
plot(ecdf(weatherGR0), do.points=FALSE,col=4, verticals=TRUE, add=TRUE)

weatherDU1a<-subset(test13,codesumDU=="1")
weatherDU0a<-subset(test13,codesumDU=="0")
weatherDU1<-weatherDU1a[,"units"]
weatherDU0<-weatherDU0a[,"units"]
shapiro.test(weatherGR1)
ad.test(weatherDU0)
length(weatherDU1)
length(weatherDU0)
summary(weatherDU1)
summary(weatherDU0)
wilcox.test(weatherDU1,weatherDU0 )
plot(ecdf(weatherDU1),xlim=range(0,500),col=2,cex=0.1)
plot(ecdf(weatherDU0), do.points=FALSE,col=4,cex=0.1, verticals=TRUE, add=TRUE)

weatherSN1a<-subset(test13,codesumSN=="1")
weatherSN0a<-subset(test13,codesumSN=="0")
weatherSN1<-weatherSN1a[,"units"]
weatherSN0<-weatherSN0a[,"units"]
ad.test(weatherSN1)
ad.test(weatherSN0)
length(weatherSN1)
length(weatherSN0)
summary(weatherSN1)
summary(weatherSN0)
wilcox.test(weatherSN1,weatherSN0 )
plot(ecdf(weatherSN1),xlim=range(0,500),col=2)
plot(ecdf(weatherSN0), do.points=FALSE,,col=4, verticals=TRUE, add=TRUE)
weatherSQ1a<-subset(test13,codesumSQ=="1")
weatherSQ0a<-subset(test13,codesumSQ=="0")
weatherSQ1<-weatherSQ1a[,"units"]
weatherSQ0<-weatherSQ0a[,"units"]
ad.test(weatherSQ1)
ad.test(weatherSQ0)
length(weatherSQ1)
length(weatherSQ0)
summary(weatherSQ1)
summary(weatherSQ0)
wilcox.test(weatherSQ1,weatherSQ0 )
plot(ecdf(weatherSQ1),xlim=range(0,500),col=2)
plot(ecdf(weatherSQ0), do.points=FALSE,col=4, verticals=TRUE, add=TRUE)
weatherTS1a<-subset(test13,codesumTS=="1")
weatherTS0a<-subset(test13,codesumTS=="0")
weatherTS1<-weatherTS1a[,"units"]
weatherTS0<-weatherTS0a[,"units"]
ad.test(weatherTS1)
ad.test(weatherTS0)
length(weatherTS1)
length(weatherTS0)
summary(weatherTS1)
summary(weatherTS0)
wilcox.test(weatherTS1,weatherTS0 )
plot(ecdf(weatherTS1),xlim=range(0,600),col=2,cex=0.1)
plot(ecdf(weatherTS0), do.points=FALSE, verticals=TRUE, add=TRUE,col=4,cex=0.1)
weather011a<-subset(test13,codesum01=="1")
weather010a<-subset(test13,codesum01=="0")
weather011<-weather011a[,"units"]
weather010<-weather010a[,"units"]
ad.test(weather011)
ad.test(weather010)
length(weather011)
length(weather010)
summary(weather011)
summary(weather010)
wilcox.test(weather011,weather010 )
plot(ecdf(weather011),xlim=range(0,500),cex=0.08,col=2)
plot(ecdf(weather010), do.points=FALSE, verticals=TRUE, add=TRUE,cex=0.08,col=4)

weatherext1a<-subset(test13,codesumext=="1")
weatherext0a<-subset(test13,codesumext=="0")
weatherext1<-weatherext1a[,"units"]
weatherext0<-weatherext0a[,"units"]
ad.test(weatherext1)
ad.test(weatherext0)
length(weatherext1)
length(weatherext0)
summary(weatherext1)
summary(weatherext0)
wilcox.test(weatherext1,weatherext0 )
plot(ecdf(weatherext1),xlim=range(0,500),cex=0.01,col=2)
plot(ecdf(weatherext0), do.points=FALSE, verticals=TRUE, add=TRUE,cex=0.1,col=4
####負二項回歸
yy<-(test13$units)
yy[yy==0]=1
y<-log(yy)
test13 <-test13[which(test13$units!="5613"),] 
test13 <-test13[which(test13$units!="3405"),]
test11<-test13
codDU<-as.factor(test11$codesumDU)
codFG<-as.factor(test11$codesumFG)
codGR<-as.factor(test11$codesumGR)
codSN<-as.factor(test11$codesumSN)
codSQ<-as.factor(test11$codesumSQ)
codTS<-as.factor(test11$codesumTS)
test11$tavg[test11$tavg=="M"]=65
test11$avgspeed[test11$avgspeed=="M"]=10.2
tavg1<-as.numeric(test11$tavg)
avgspeed1<-as.numeric(test11$avgspeed)
teslm<-glm(y~codDU+codFG+codGR+codSN*tavg1+codSQ*tavg1*avgspeed1+codTS*avgspeed1)
step(teslm,direction="backward")
model<-glm(y~codDU + codFG + codSN + codSQ + codTS)
plot(model)
test11$tavg[test11$tavg=="M"]=65
test11$avgspeed[test11$avgspeed=="M"]=10.2
install.packages("MASS")
library(MASS)

codDU<-as.factor(sales11.train$codesumDU)
codFG<-as.factor(sales11.train$codesumFG)
codGR<-as.factor(sales11.train$codesumGR)
codSN<-as.factor(sales11.train$codesumSN)
codSQ<-as.factor(sales11.train$codesumSQ)
codTS<-as.factor(sales11.train$codesumTS)
tavg1<-as.numeric(sales11.train$tavg)
avgspeed1<-as.numeric(sales11.train$avgspeed)
nbmodel<- glm.nb(y~codFG+codDU+codGR+codSN*tavg1+codSQ*tavg1*avgspeed1+codTS*avgspeed1)
summary(nbmodel)
test<-round(exp(predict(nbmodel)))
summary(test)
par(mfrow=c(2,2))
plot(nbmodel)
ttst<-round(exp(predict(nbmodel)))
thead(test13)
str(test13)
summary(ttst)
a<-(ttst-test11$units)
hist(a)
par(mfrow=c(1,1))
summary(test11)
length(codFG)
weathertavg1a<-subset(test13,tavg>="65")
weathertavg0a<-subset(test13,tavg<"65")
weathertavg1<-weathertavg1a[,"units"]
weathertavg0<-weathertavg0a[,"units"]
ad.test(weathertavg1)
ad.test(weathertavg0)
length(weathertavg1)
length(weathertavg0)
summary(weathertavg1)
summary(weathertavg0)
wilcox.test(weathertavg1,weathertavg0 )
plot(ecdf(weathertavg1),xlim=range(0,500),cex=0.01,col=2)
plot(ecdf(weathertavg0), do.points=FALSE, verticals=TRUE, add=TRUE,cex=0.01,col=4)


install.packages("psc1")
library(psc1)
install.packages("boot")
librarutils:::menuInstallPkgs()y(boot)
y

nbmodel<- lm(y~codFG+codDU+codGR+codSN*tavg1+codSQ*tavg1*avgspeed1+codTS*avgspeed1)
summary(nbmodel)
cv.10 <- cv.glm(test11 ,nbmodel, K = 10)
cv.10
summary(cv.10)
names(cv.10)
length(y)
install.packages("boot")
library(boot)
nbmodel<- glm(y~codFG+codDU+codGR+codSN*tavg1+codSQ*tavg1*avgspeed1+codTS*avgspeed1,family= binomial)

n<-0.1*nrow(test11)
test11.index=sample(1:nrow(test11),n)
sales11.train=test11[-test11.index,]
sales11.test<-test11[test11.index,]
real<-sales11.test$units

predict<-predict(nbmodel,newdata=sales11.test)
a<-predict-real
length(a)
aaa<-sum(a^2)
aaa/41598
######### weekday
weekend <- test13[which(test13$weekday=="6"|test13$weekday=="7"),]
weekday <- test13[which(test13$weekday=="1"|test13$weekday=="2"|test13$weekday=="3"|test13$weekday=="4"|test13$weekday=="5"),]

ad.test(weekend$units)
ad.test(weekday$units)
wilcox.test(weekend$units,weekday$units)
par(mfrow=c(1,2))
plot(ecdf(weekend$units),xlim=range(0,500),col=2,cex=0.1,main="平日與假日的數量累積cdf圖")
plot(ecdf(weekday$units), do.points=FALSE,col=4,cex=0.1, verticals=TRUE, add=TRUE)
plot(ecdf(weekend$units),xlim=range(0,1000),col=2,cex=0.1,main="平日與假日的數量累積cdf圖")
plot(ecdf(weekday$units), do.points=FALSE,col=4,cex=0.1, verticals=TRUE, add=TRUE)
