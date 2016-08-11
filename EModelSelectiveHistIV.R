library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#data row num
DATA_NUM=2000

#read data file
histPrc<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep=""),header=T,sep=",",nrows=DATA_NUM)
histIV<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep=""),header=T,sep=",",nrows=DATA_NUM)

#show Realized and Impled Volatility
cat("Realized Vol(30d anlzd)",annuual.daily.volatility(histPrc$Close[1:30])$anlzd*100,"IV",histIV$Close[1])
cat("Realized Vol(60d anlzd)",annuual.daily.volatility(histPrc$Close[1:60])$anlzd*100,"IV",histIV$Close[1])
cat("Realized Vol(120d anlzd)",annuual.daily.volatility(histPrc$Close[1:120])$anlzd*100,"IV",histIV$Close[1])
cat("Realized Vol(200d anlzd)",annuual.daily.volatility(histPrc$Close[1:200])$anlzd*100,"IV",histIV$Close[1])

##
# Price2IVIDX called from saveP2IVReg
#  histPrc : vector such as histPrc$Close, histIVDf : vector such as histIV$Close

Price2IVIDX <- function(histPrc,histIV,dataNum,xDayInt,start_day=1){
  
  PCxdCtC<- PCndCtC(hist=histPrc,n=xDayInt)
  IVCFxdCtC<-IVCFndCtC(iv=histIV,n=xDayInt)
  
  #Regression
  PCxdCtC=PCxdCtC[start_day:(start_day+dataNum)]
  na.omit(PCxdCtC)  %>% as.vector() -> PCxdCtC
  IVCFxdCtC=IVCFxdCtC[start_day:(start_day+dataNum)]
  na.omit(IVCFxdCtC)  %>% as.vector() -> IVCFxdCtC
  
  PCxdCtC=PCxdCtC[1:min(length(PCxdCtC),length(IVCFxdCtC))]
  IVCFxdCtC=IVCFxdCtC[1:min(length(PCxdCtC),length(IVCFxdCtC))]
  P2IVxd <- data.frame(PCxdCtC=PCxdCtC, IVCFxdCtC=IVCFxdCtC)
  
  co=cor(PCxdCtC[start_day:min((start_day+dataNum),length(PCxdCtC))],
         IVCFxdCtC[start_day:min((start_day+dataNum),length(IVCFxdCtC))])
  
  #returned list
  ret_names<-c("P2IVxd","cor","PCxdCtC","IVCFxdCtC")
  ret <- vector("list",length(ret_names))
  ret[[1]]<-P2IVxd
  ret[[2]]<-co
  ret[[3]]<-PCxdCtC
  ret[[4]]<-IVCFxdCtC
  names(ret)<-ret_names
  return(ret)
}

##
# save the regression result.
saveP2IVReg<-function(histPrc,histIV,dataNum,xDayInt,start_day=1){
  tmp=Price2IVIDX(histPrc,histIV,dataNum,xDayInt,start_day)
  P2IVxd=tmp$P2IVxd
  co=tmp$cor
  print(co)
  
  #linear regression
  norns.lm<-lm(IVCFxdCtC~PCxdCtC, data=P2IVxd)
  summary(norns.lm)
  save.PC2IV(model=norns.lm,
             PC=paste("PC",xDayInt,"dCtC",sep=""),
             IVC=paste("IVCF",xDayInt,"dCtC",sep=""),
             cor=co,
             pcstat=c(mean(tmp$PCxdCtC),sd(tmp$PCxdCtC)),
             ivstat=c(mean(tmp$IVCFxdCtC),sd(tmp$IVCFxdCtC)))
 
  #returned list
  ret_names<-c("P2IVxd","lm")
  ret <- vector("list",length(ret_names))
  ret[[1]]<-P2IVxd
  ret[[2]]<-norns.lm
  names(ret)<-ret_names
  return(ret)
}

######
## 5d
xDayInt=5
tmp=saveP2IVReg(histPrc$Close,histIV$Close,min(504,DATA_NUM),xDayInt)
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC5dCtC_IVCF5dCtC

#####
##   3d
xDayInt=3
tmp=saveP2IVReg(histPrc$Close,histIV$Close,min(504,DATA_NUM),xDayInt)
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC3dCtC_IVCF3dCtC

#####
##   7d
xDayInt=7
tmp=saveP2IVReg(histPrc$Close,histIV$Close,min(504,DATA_NUM),xDayInt)
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC7dCtC_IVCF7dCtC

#####
##  12d
xDayInt=12
tmp=saveP2IVReg(histPrc$Close,histIV$Close,min(504,DATA_NUM),xDayInt)
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC12dCtC_IVCF12dCtC

#####
##  18d
xDayInt=18
tmp=saveP2IVReg(histPrc$Close,histIV$Close,min(504,DATA_NUM),xDayInt)
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC18dCtC_IVCF18dCtC

#####
##   1d
xDayInt=1
tmp=saveP2IVReg(histPrc$Close,histIV$Close,min(504,DATA_NUM),xDayInt)
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC1dCtC_IVCF1dCtC
