setwd("/Users/muhtasim/Desktop/STAT530/HWs/HW5")
data=read.csv("HW3_2020_clean.csv",header=T)
##problem1a
y=data$Calories
x1=data$Weight.lbs
x2=data$Height.Inches
x3=data$Protein
x4=data$Carbohydrates
x5=data$Calcium
x6=data$gender

par(mfrow=c(3,2))

lw1=loess(y ~ x1,data, span=.6)
plot(y ~ x1, data=data)
j1 <- order(x1)
lines(x1[j1],lw1$fitted[j1],col="red3",lwd=3)

lw2=loess(y ~ x2,data, span=.65)
plot(y ~ x2, data=data)
j2 <- order(x2)
lines(x2[j2],lw2$fitted[j2],col="olivedrab3",lwd=3)

lw3=loess(y ~ x3,data, span=.65)
plot(y ~ x3, data=data)
j3 <- order(x3)
lines(x3[j3],lw3$fitted[j3],col="deepskyblue3",lwd=3)

lw4=loess(y ~ x4,data, span=.65)
plot(y ~ x4, data=data)
j4 <- order(x4)
lines(x4[j4],lw4$fitted[j4],col="gold",lwd=3)

lw5=loess(y ~ x5,data, span=.65)
plot(y ~ x5, data=data)
j5 <- order(x5)
lines(x5[j5],lw5$fitted[j5],col="mediumorchid3",lwd=3)

lw6=loess(y ~ x5,data, span=.65)
plot(y ~ x6, data=data)
j6 <- order(x6)
lines(x6[j6],lw6$fitted[j6],col="darkorange",lwd=3)

par(mfrow=c(1,1))

##problem1b
#MLR
fit1 = lm(Calories ~ Weight.lbs + Height.Inches + Protein + Carbohydrates
          + Calcium + gender, data = data)
#residual vs fitted plot
lwfit=loess(fit1$residuals ~ fit1$fitted.values, data, span=.65)
plot(fit1$fitted.values, fit1$residuals)
jfit = order(fit1$fitted.values)
lines(fit1$fitted.values[jfit],lwfit$fitted[jfit],col="hotpink33",lwd=3)


##problem2a
library("readxl")
nlindata=read_excel("Hw5_Nlindata.xls")
y=nlindata$damage
x=nlindata$time
##starting values a=intercept, b=slope/intercept
model=nls(y~a-b*exp(-c*x),start=list(a=15,b=10,c=0.5))
plot(x,y)
lines(x,predict(model),col="red",lty=2,lwd=3)
summary(model)
library(nlstools)
cf=confint2(model,level=0.95)
cf


##problem2b
#bootstrap by Vasili
dat<-read.csv("Hw5_Nlindata.csv", header = TRUE, sep = ",", na.strings = " ", colClasses = c('numeric', 'numeric'))
library(data.table)
library(boot)
model.fun<- function(dt){
  fit<-nls(damage~a-(b*exp(-c*time)), start = list(a=0,b=0.01, c=0.01), data = dt)
  df<-dt
  df<-data.frame(df,fitted(fit),residuals(fit))
  colnames(df)<-c("time", "damage", "fitted", "resid")
  
  
  fun<-function(df, inds){
    
    library(data.table)
    bootDamage=df$fitted +df$resid[inds]
    df<-data.frame(df,bootDamage)
    colnames(df)<-c("time", "damage", "fitted", "resid", "bootDamage")
    tryCatch(coef(nls(bootDamage~a-(b*exp(-c*time)),
                      start=list(a=0,b=0.01,c=0.01), data=df)),
             error=function(e)c("a"= NA, "b"= NA, "c"= NA))
  }
  
  
  kk<-boot(df, fun, R=1000)
  print(boot.ci(kk, type="bca", index = 1) )
  print(boot.ci(kk, type="bca", index = 2) )
  print(boot.ci(kk, type="bca", index = 3) )
  res0<-kk$t0
  res1<- apply(kk$t, 2, sd, na.rm= TRUE)
  res2<- res0 - colMeans(kk$t, na.rm = TRUE)
  return(as.list(setNames(c(sum(!is.na(kk$t[,1])), res0, res1, res2), c("n","a","b","c","SD_a","SD_b","SD_c","Bias_a", "Bias_b", "Bias_c"))))
  
}
models<-model.fun(dat)
models 


##problem2c
#linear fit
lin=lm(y~x,data=nlindata)
#lowess
lw=loess(y ~ x,nlindata, span=.75)
j=order(x)
#plotting
plot(x,y, xlab="Time", ylab="Damage")
abline(lin,col="deepskyblue3",lty=4,lwd=2)   #linear
lines(x,predict(model),col="olivedrab3",lty=2,lwd=2)  #nonlinear
lines(x[j],lw$fitted[j],col="hotpink3",lty=1,lwd=2)   #lowess