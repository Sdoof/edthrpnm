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

#####
## Return Distribution Estimates

##  Histgram
# xDayInt=EvalFuncSetting$holdDays
# tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
# tmp$lm
# if(IS_SELECTIVE_WEIGHT_ESTM){
#   selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
#   tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
# }
# h <- dpih(tmp$P2IVxd$PCxdCtC)
# bins <- seq(min(tmp$P2IVxd$PCxdCtC)-3*h/2, max(tmp$P2IVxd$PCxdCtC)+3*h/2, by=h)
# hist(tmp$P2IVxd$PCxdCtC, breaks=bins)
# rm(h)
# 
# #randam generate
# tmp_data=rpearson(20000,moments=empMoments(tmp$P2IVxd$PCxdCtC))
# empMoments(tmp_data)
# #histgram
# h <- dpih(tmp_data)
# bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
# hist(tmp_data, breaks=bins)
# #density estimation
# est_density=dpearson(seq(
#   (-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
#   EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
#   by=EvalFuncSetting$UdlStepPct),
#   moments=empMoments(tmp$P2IVxd$PCxdCtC))
# 
# est_density=est_density/sum(est_density)
# 
# frame()
# plot(seq(
#   (-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
#   EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
#   by=EvalFuncSetting$UdlStepPct),
#   est_density,col="blue")
# 
# cat("c(");cat(est_density,sep="$");cat(")")

#####
##  moment estimation and moment transformation for desirable "should be headged" Distribution

# moment_org=empMoments(tmp$P2IVxd$PCxdCtC)
# #just show anualized mean and sd
# cat("mean original anlzd",moment_org["mean"]*252/EvalFuncSetting$holdDays,"\n")
# cat("vol original anlzd",sqrt(moment_org["variance"])*sqrt(252/EvalFuncSetting$holdDays),"\n")

###
##  transormed moment mean neutral
moment_trnsfm=moment_org

#drift transformed (annualized)
# dfift_trnsfm=0
# moment_trnsfm["mean"]=dfift_trnsfm/(252/EvalFuncSetting$holdDays)

#volatility transformed (annualized)
# vol_trnsfm=sqrt(moment_org["variance"])*sqrt(252/EvalFuncSetting$holdDays)*1
# moment_trnsfm["variance"]=(vol_trnsfm/sqrt(252/EvalFuncSetting$holdDays))^2
# (moment_org)
# (moment_trnsfm)
# #randam generate
# tmp_data=rpearson(20000,moments=moment_trnsfm)
# empMoments(tmp_data)
# #histgram
# h <- dpih(tmp_data)
# bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
# hist(tmp_data, breaks=bins)
# #density estimation
# est_density=dpearson(seq(
#   (-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
#   EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
#   by=EvalFuncSetting$UdlStepPct),
#   moments=moment_trnsfm)
# 
# est_density=est_density/sum(est_density)
# 
# cat("c(");cat(est_density,sep="$");cat(")")

###
##  transormed moment opposite mean
moment_trnsfm=moment_org

#drift transformed (annualized)
# dfift_trnsfm_ration=1
# moment_trnsfm["mean"]=moment_org["mean"]*(-1)*dfift_trnsfm_ration
# (moment_org)
# (moment_trnsfm)
#randam generate
# tmp_data=rpearson(20000,moments=moment_trnsfm)
# empMoments(tmp_data)
#histgram
# h <- dpih(tmp_data)
# bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
# hist(tmp_data, breaks=bins)
#density estimation
# est_density=dpearson(seq(
#   (-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
#   EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
#   by=EvalFuncSetting$UdlStepPct),
#   moments=moment_trnsfm)
# 
# est_density=est_density/sum(est_density)
# 
# cat("c(");cat(est_density,sep="$");cat(")")

##
# show IVCFxdCtC distribution for info
# h <- dpih(tmp$P2IVxd$IVCFxdCtC)
# bins <- seq(min(tmp$P2IVxd$IVCFxdCtC)-3*h/2, max(tmp$P2IVxd$IVCFxdCtC)+3*h/2, by=h)
# hist(tmp$P2IVxd$IVCFxdCtC, breaks=bins)
#random generation
# moment_IVCF=empMoments(tmp$P2IVxd$IVCFxdCtC)

#just show anualized mean and sd
# cat("mean original anlzd",moment_IVCF["mean"]*252/EvalFuncSetting$holdDays,"\n")
# cat("vol original anlzd",sqrt(moment_IVCF["variance"])*sqrt(252/EvalFuncSetting$holdDays),"\n")
# tmp_data=rpearson(20000,moments=moment_IVCF)
# empMoments(tmp_data)
#histgram
# h <- dpih(tmp_data)
# bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
# hist(tmp_data, breaks=bins)

#optimize EvalFuncSetting$UdlStepPct,EvalFuncSetting$UdlStepNum
#weight below min_effective_rate is rounded to 0 on evaluation function
# min_effective_rate=(1/500)
#show effetive weight component
# as.numeric(est_density>min_effective_rate)
#effective(relevant) price range from center
# newPriceRangeFromCenter=
#   max(EvalFuncSetting$UdlStepNum-min(which(as.numeric(est_density>min_effective_rate)==1)),
#       (EvalFuncSetting$UdlStepNum*2+1)-max(which(as.numeric(est_density>min_effective_rate)==1)))*
#   EvalFuncSetting$UdlStepPct
#if EvalFuncSetting$UdlStepNum is fixed
# newUdlStepPct=newPriceRangeFromCenter/EvalFuncSetting$UdlStepNum
# cat("new UdlStepPct",newUdlStepPct,"new UdlStepNum",EvalFuncSetting$UdlStepNum)

##
# Histgram One Day
# xDayInt=1
# tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
# tmp$lm
# if(IS_SELECTIVE_WEIGHT_ESTM){
#   selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
#   tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
# }
# h <- dpih(tmp$P2IVxd$PCxdCtC)
# bins <- seq(min(tmp$P2IVxd$PCxdCtC)-3*h/2, max(tmp$P2IVxd$PCxdCtC)+3*h/2, by=h)
# hist(tmp$P2IVxd$PCxdCtC, breaks=bins)
# rm(h)

#randam generate
# tmp_data=rpearson(20000,moments=empMoments(tmp$P2IVxd$PCxdCtC))
# empMoments(tmp_data)
#histgram
# h <- dpih(tmp_data)
# bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
# hist(tmp_data, breaks=bins)
#density estimation
# est_density=dpearson(seq(
#   (-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
#   EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
#   by=EvalFuncSetting$UdlStepPct),
#   moments=empMoments(tmp$P2IVxd$PCxdCtC))
# est_density=est_density/sum(est_density)
# 
# frame()
# plot(seq(
#   (-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
#   EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
#   by=EvalFuncSetting$UdlStepPct),
#   est_density,col="blue")
# 
# cat("c(");cat(est_density,sep="$");cat(")")

#####
##  moment estimation and moment transformation for desirable "should be headged" Distribution

# moment_org=empMoments(tmp$P2IVxd$PCxdCtC)
# (moment_org)
#just show anualized mean and sd
# cat("dift original",moment_org["mean"]*252,"\n")
# cat("vol original",sqrt(moment_org["variance"])*sqrt(252),"\n")

##
# transormed moment
# moment_trnsfm=moment_org

#drift transformed (annualized)
# dfift_trnsfm=0
# moment_trnsfm["mean"]=dfift_trnsfm/(252)

#volatility transformed (annualized)
# vol_trnsfm=sqrt(moment_org["variance"])*sqrt(252)*1
# moment_trnsfm["variance"]=(vol_trnsfm/sqrt(252))^2
# (moment_org)
# (moment_trnsfm)

#randam generate
# tmp_data=rpearson(20000,moments=moment_trnsfm)
# empMoments(tmp_data)
#histgram
# h <- dpih(tmp_data)
# bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
# hist(tmp_data, breaks=bins)
#density estimation
# est_density=dpearson(seq(
#   (-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
#   EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
#   by=EvalFuncSetting$UdlStepPct),
#   moments=moment_trnsfm)
# est_density=est_density/sum(est_density)
# 
# cat("c(");cat(est_density,sep="$");cat(")")

###
##  transormed moment opposite mean
# moment_trnsfm=moment_org

#drift transformed (annualized)
# dfift_trnsfm_ration=1
# moment_trnsfm["mean"]=moment_org["mean"]*(-1)*dfift_trnsfm_ration
# (moment_org)
# (moment_trnsfm)
#randam generate
# tmp_data=rpearson(20000,moments=moment_trnsfm)
# empMoments(tmp_data)
#histgram
# h <- dpih(tmp_data)
# bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
# hist(tmp_data, breaks=bins)
#density estimation
# est_density=dpearson(seq(
#   (-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
#   EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
#   by=EvalFuncSetting$UdlStepPct),
#   moments=moment_trnsfm)
# 
# est_density=est_density/sum(est_density)
# 
# cat("c(");cat(est_density,sep="$");cat(")")


