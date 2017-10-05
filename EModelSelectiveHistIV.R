library(RQuantLib)
library(ggplot2)
library(dplyr)
library(digest)
library(hash)
library(MASS)
library(KernSmooth)
library(gsl)
library(PearsonDS)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#Data Num.
DATA_NUM=252*15 # about x years equivalent 

#read data file
histPrc<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep=""),header=T,stringsAsFactors=F,sep=",")
histIV<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep=""),header=T,stringsAsFactors=F,sep=",")

#inner joing for modify data inconsistency
dplyr::inner_join(histPrc, histIV, by = "Date")->injoinPrcIV
histPrc=injoinPrcIV$Close.x
histIV=injoinPrcIV$Close.y
Date=injoinPrcIV$Date

#show Realized and Impled Volatility
cat("Realized Vol(30d anlzd)",annuual.daily.volatility(histPrc[1:30])$anlzd*100,"IV",histIV[1])
cat("Realized Vol(60d anlzd)",annuual.daily.volatility(histPrc[1:60])$anlzd*100,"IV",histIV[1])
cat("Realized Vol(120d anlzd)",annuual.daily.volatility(histPrc[1:120])$anlzd*100,"IV",histIV[1])
cat("Realized Vol(200d anlzd)",annuual.daily.volatility(histPrc[1:200])$anlzd*100,"IV",histIV[1])

#selective parmeters For SPX
IS_SELECTIVE_HISTIV_REGR=T
IS_SELECTIVE_WEIGHT_ESTM=T
a_low=0.9
d_low=2
a_high=1.1
d_high=2
#Smooth Spline df
DF_P2IVREG_SSPL=5.5

#selective parmeters For RUT
if(Underying_Symbol_G=="RUT"){
  IS_SELECTIVE_HISTIV_REGR=T
  IS_SELECTIVE_WEIGHT_ESTM=T
  a_low=0.75
  d_low=3
  a_high=1.3
  d_high=3
  #Smooth Spline df
  DF_P2IVREG_SSPL=5.5
}

#####
## Expected HV predict

# select suffix
effectiv_suffix=selectSuffixToPredictHV(histIV,a_low,d_low,a_high,d_high)

# HVIVR statistic
HVIVR=getHVIVRStat(histPrc,histIV,xDayVol=20,effectiv_suffix)
mean(HVIVR)
sd(HVIVR)
mean(HVIVR[1:100])
sd(HVIVR[1:100])

# HVIVR statistic for all histIV data. effectiv_suffix are given as below.
HVIVR=getHVIVRStat(histPrc,histIV,xDayVol=20,effectiv_suffix=seq(1:length(histIV)))
mean(HVIVR)
sd(HVIVR)
mean(HVIVR[1:100])
sd(HVIVR[1:100])

cat("HV_IV_Adjust_Ratio mean",mean(HVIVR[1:100]),"\n")
HV_IV_Adjust_Ratio=mean(HVIVR[1:100])+sd(HVIVR[1:100])
cat("HV_IV_Adjust_Ratio mean+1sd",HV_IV_Adjust_Ratio,"\n")
HV_IV_Adjust_Ratio=(mean(HVIVR[1:100])+mean(HVIVR[1:100])+sd(HVIVR[1:100]))/2
cat("HV_IV_Adjust_Ratio mean+1sd",HV_IV_Adjust_Ratio,"\n")

DVIV=getDVIVStat(histIV,xDayVol=dviv_caldays,effectiv_suffix,isDebug=F)
AVIV=DVIV*sqrt(252)
#total data AVIV 
annuual.daily.volatility(histIV)$anlzd
#first dviv_caldays AVIV
annuual.daily.volatility(histIV[1:dviv_caldays])$anlzd

mean(AVIV)
mean(AVIV[1:100])

#"ExpIVChange_Multiple coeffecient
cat("ExpIVChange_Multiple",mean(AVIV[1:100])/annuual.daily.volatility(histIV[1:dviv_caldays])$anlzd,"\n")
cat("ExpIVChange_Multiple",mean(DVIV[1:100])/annuual.daily.volatility(histIV[1:dviv_caldays])$daily,"\n")

#####
##   P2IV REGRESSION

######
## 5d
xDayInt=5
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  #suffix for PCxdCtC adn IVCFxdCtC which have shifted xDayInt toward past
  #which means, for example, suffix 1 for PCxdCtC  means 1+xDayInt for corresponding histIV data.
  #use selectSuffixToPredictHV to get the suffix for histIV(histPrc) data
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC5dCtC_IVCF5dCtC

#Non Linear Regression
tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL)
tmp$model
if(IS_SELECTIVE_HISTIV_REGR){
  #suffix for PCxdCtC adn IVCFxdCtC which have shifted xDayInt toward past
  #which means, for example, suffix 1 for PCxdCtC  means 1+xDayInt for corresponding histIV data.
  #use selectSuffixToPredictHV to get the suffix for histIV(histPrc) data
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_line(data=data.frame(PCxdCtC=tmp$model$x,IVCFxdCtC=tmp$model$y),aes(PCxdCtC,IVCFxdCtC),color="red")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.NLPC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC5dCtC_NL_IVCF5dCtC

#####
##   3d
xDayInt=3
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC3dCtC_IVCF3dCtC

#Non Linear Regression
tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL)
tmp$model
if(IS_SELECTIVE_HISTIV_REGR){
  #suffix for PCxdCtC adn IVCFxdCtC which have shifted xDayInt toward past
  #which means, for example, suffix 1 for PCxdCtC  means 1+xDayInt for corresponding histIV data.
  #use selectSuffixToPredictHV to get the suffix for histIV(histPrc) data
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_line(data=data.frame(PCxdCtC=tmp$model$x,IVCFxdCtC=tmp$model$y),aes(PCxdCtC,IVCFxdCtC),color="red")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.NLPC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC3dCtC_NL_IVCF3dCtC

#####
##   7d
xDayInt=7
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC7dCtC_IVCF7dCtC

#Non Linear Regression
tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL)
tmp$model
if(IS_SELECTIVE_HISTIV_REGR){
  #suffix for PCxdCtC adn IVCFxdCtC which have shifted xDayInt toward past
  #which means, for example, suffix 1 for PCxdCtC  means 1+xDayInt for corresponding histIV data.
  #use selectSuffixToPredictHV to get the suffix for histIV(histPrc) data
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_line(data=data.frame(PCxdCtC=tmp$model$x,IVCFxdCtC=tmp$model$y),aes(PCxdCtC,IVCFxdCtC),color="red")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.NLPC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC7dCtC_NL_IVCF7dCtC

#####
##  12d
xDayInt=12
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC12dCtC_IVCF12dCtC

#Non Linear Regression
tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL)
tmp$model
if(IS_SELECTIVE_HISTIV_REGR){
  #suffix for PCxdCtC adn IVCFxdCtC which have shifted xDayInt toward past
  #which means, for example, suffix 1 for PCxdCtC  means 1+xDayInt for corresponding histIV data.
  #use selectSuffixToPredictHV to get the suffix for histIV(histPrc) data
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_line(data=data.frame(PCxdCtC=tmp$model$x,IVCFxdCtC=tmp$model$y),aes(PCxdCtC,IVCFxdCtC),color="red")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.NLPC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC12dCtC_NL_IVCF12dCtC

#####
##  18d
xDayInt=18
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC18dCtC_IVCF18dCtC

#Non Linear Regression
tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL)
tmp$model
if(IS_SELECTIVE_HISTIV_REGR){
  #suffix for PCxdCtC adn IVCFxdCtC which have shifted xDayInt toward past
  #which means, for example, suffix 1 for PCxdCtC  means 1+xDayInt for corresponding histIV data.
  #use selectSuffixToPredictHV to get the suffix for histIV(histPrc) data
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_line(data=data.frame(PCxdCtC=tmp$model$x,IVCFxdCtC=tmp$model$y),aes(PCxdCtC,IVCFxdCtC),color="red")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.NLPC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC18dCtC_NL_IVCF18dCtC

#####
##   1d
xDayInt=1
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC1dCtC_IVCF1dCtC

#Non Linear Regression
tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL)
tmp$model
if(IS_SELECTIVE_HISTIV_REGR){
  #suffix for PCxdCtC adn IVCFxdCtC which have shifted xDayInt toward past
  #which means, for example, suffix 1 for PCxdCtC  means 1+xDayInt for corresponding histIV data.
  #use selectSuffixToPredictHV to get the suffix for histIV(histPrc) data
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVNonLReg(histPrc,histIV,DATA_NUM,xDayInt,df=DF_P2IVREG_SSPL,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.08)+
    geom_line(data=data.frame(PCxdCtC=tmp$model$x,IVCFxdCtC=tmp$model$y),aes(PCxdCtC,IVCFxdCtC),color="red")+
    xlim(min(tmp$P2IVxd$PCxdCtC)*1.2,max(tmp$P2IVxd$PCxdCtC)*1.2)+
    ylim(min(tmp$P2IVxd$IVCFxdCtC)*1.2,max(tmp$P2IVxd$IVCFxdCtC)*1.2)
)
#Load test
load.NLPC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC1dCtC_NL_IVCF1dCtC
