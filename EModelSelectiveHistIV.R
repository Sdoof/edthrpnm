library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
library(sn)
library(MASS)
library(KernSmooth)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#Data Num.
DATA_NUM=252*10 # about x years equivalent 

#read data file
histPrc<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep=""),header=T,sep=",")
histIV<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep=""),header=T,sep=",")

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

#selective parmeters
IS_SELECTIVE_HISTIV_REGR=T
IS_SELECTIVE_WEIGHT_ESTM=T
a_low=0.8
d_low=5
a_high=1.25
d_high=5

##
# select Suffix to icnlude the valid IV data and make the selected suffix suitable to 
# apply to vector created by PCndCtC or IVCFndCtC functions.
selectSuffixForValidIV <- function(histIV,xDayInt,a_low,d_low,a_high,d_high){
  theIV=histIV[1]
  SelectInclude=(histIV>=theIV*a_low | histIV>=(theIV-d_low))&(histIV<=theIV*a_high | histIV<=(theIV+d_high))
  
  SelectIncludeShift=rep(0,length(SelectInclude))#numeric((length(SelectInclude)-xDayInt))
  SelectIncludeShift[(xDayInt+1):length(SelectInclude)]=SelectInclude[(xDayInt+1):length(SelectInclude)]
  which(SelectIncludeShift==1)->suffix_slctd
  suffix_slctd=suffix_slctd-xDayInt
  return(suffix_slctd)
}

##
# Price2IVIDX called from saveP2IVReg
#  histPrc : vector such as histPrc$Close, histIVDf : vector such as histIV$Close
Price2IVIDX <- function(histPrc,histIV,dataNum,xDayInt,start_day=1,effectiv_suffix=0){
  
  PCxdCtC<- PCndCtC(hist=histPrc,n=xDayInt)
  IVCFxdCtC<-IVCFndCtC(iv=histIV,n=xDayInt)
  
  if(length(effectiv_suffix)>2){
    PCxdCtC[effectiv_suffix]->tmp
    na.omit(tmp) %>% as.vector() -> PCxdCtC
    IVCFxdCtC[effectiv_suffix]->tmp
    na.omit(tmp) %>% as.vector() -> IVCFxdCtC
  }
  
  cat("effectiv_suffix PCxdCtC",length(PCxdCtC),"\n")
  cat("effectiv_suffix IVCFxdCtC",length(IVCFxdCtC),"\n")
  
  if(length(PCxdCtC)<=(start_day+dataNum))
    dataNum=length(PCxdCtC)-start_day
  
  #Regression
  PCxdCtC=PCxdCtC[start_day:(start_day+dataNum-1)]
  na.omit(PCxdCtC)  %>% as.vector() -> PCxdCtC
  IVCFxdCtC=IVCFxdCtC[start_day:(start_day+dataNum-1)]
  na.omit(IVCFxdCtC)  %>% as.vector() -> IVCFxdCtC
  
  PCxdCtC=PCxdCtC[1:min(length(PCxdCtC),length(IVCFxdCtC))]
  IVCFxdCtC=IVCFxdCtC[1:min(length(PCxdCtC),length(IVCFxdCtC))]
  
  cat("dataNum PCxdCtC",length(PCxdCtC),"\n")
  cat("dataNum IVCFxdCtC",length(IVCFxdCtC),"\n")
  
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
saveP2IVReg<-function(histPrc,histIV,dataNum,xDayInt,start_day=1,effectiv_suffix=0){
  tmp=Price2IVIDX(histPrc,histIV,dataNum,xDayInt,start_day,effectiv_suffix=effectiv_suffix)
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
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  #suffix for PCxdCtC adn IVCFxdCtC which have shifted xDayInt toward past
  #which means, for example, suffix 1 for PCxdCtC  means 1+xDayInt for corresponding histIV data.
  #use selectSuffixToPredictHV to get the suffix for histIV(histPrc) data
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.2))
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC5dCtC_IVCF5dCtC

#####
##   3d
xDayInt=3
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.2))
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC3dCtC_IVCF3dCtC

#####
##   7d
xDayInt=7
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.2))
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC7dCtC_IVCF7dCtC

#####
##  12d
xDayInt=12
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.2))
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC12dCtC_IVCF12dCtC

#####
##  18d
xDayInt=18
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.2))
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC18dCtC_IVCF18dCtC

#####
##   1d
xDayInt=1
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point(alpha=0.2))
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC1dCtC_IVCF1dCtC

####
# expected HV predict

##
# select Suffix to icnlude the valid IV data
selectSuffixToPredictHV <- function(histIV,a_low,d_low,a_high,d_high){
  theIV=histIV[1]
  SelectInclude=(histIV>=theIV*a_low | histIV>=(theIV-d_low))&(histIV<=theIV*a_high | histIV<=(theIV+d_high))
  
  which(SelectInclude==TRUE)->suffix_slctd
  
  return(suffix_slctd)
}

##
# realized HV/IV ratio statistic of selected subdata
getHVIVRStat <- function(histPrc,histIV,xDayVol,effectiv_suffix,isDebug=F){
  HVIVR_V=rep(0,times=length(effectiv_suffix))
  hvivr_suff=1
  # histPrc[i+xdays], +(xdays)(->) means data backs to xdays PAST
  # histPrc[i-xdays], -(xdays)(<-) means data looks forward xdays FUTURE
  for(i in 1:length(effectiv_suffix)){
    if(effectiv_suffix[i]>=xDayVol){
      histPrc[effectiv_suffix[i]:(effectiv_suffix[i]+xDayVol)]
      #toward the FUTURE
      HVIVR=annuual.daily.volatility( histPrc[(effectiv_suffix[i]-xDayVol):effectiv_suffix[i]] )$anlzd/
        (histIV[(effectiv_suffix[i])]/100)
      if(isDebug)
        cat("at",i,
            "HV",annuual.daily.volatility( histPrc[(effectiv_suffix[i]-xDayVol):effectiv_suffix[i]] )$anlzd,
            "IV",histIV[(effectiv_suffix[i])]/100,
            "HVIVR",HVIVR,"\n")
      HVIVR_V[hvivr_suff]=HVIVR
      hvivr_suff=hvivr_suff+1
    }
  }
  HVIVR=HVIVR_V[1:(hvivr_suff-1)]
  return(HVIVR)
}

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

##
# Daily Volatility of Implied Volatility statistic of selected subdata
getDVIVStat <- function(histIV,xDayVol,effectiv_suffix,isDebug=F){
  DVIV_V=rep(0,times=length(effectiv_suffix))
  dviv_suff=1
  # histIV[i+xdays], +(xdays)(->) means data backs to xdays PAST
  # histIV[i-xdays], -(xdays)(<-) means data looks forward xdays FUTURE
  for(i in 1:length(effectiv_suffix)) {
    if(effectiv_suffix[i]<=length(histIV)-xDayVol){
      histIV[effectiv_suffix[i]:(effectiv_suffix[i]+xDayVol)]
      #toward the past
      DVIV=annuual.daily.volatility( histIV[effectiv_suffix[i]:(effectiv_suffix[i]+xDayVol)] )$daily
      if(isDebug)
        cat("at",effectiv_suffix[i],
            "DVIV",DVIV,"AVIV",DVIV*sqrt(252),
            "IV",histIV[(effectiv_suffix[i])],
            "\n")
      DVIV_V[dviv_suff]=DVIV
      dviv_suff=dviv_suff+1
    }
  }
  DVIV=DVIV_V[1:(dviv_suff-1)]
  return(DVIV)
}

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
## Return DistributionEstimates

##  Histgram
xDayInt=EvalFuncSetting$holdDays
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_WEIGHT_ESTM){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
h <- dpih(tmp$P2IVxd$PCxdCtC)
bins <- seq(min(tmp$P2IVxd$PCxdCtC)-3*h/2, max(tmp$P2IVxd$PCxdCtC)+3*h/2, by=h)
hist(tmp$P2IVxd$PCxdCtC, breaks=bins)
rm(h)

est_density=density(tmp$P2IVxd$PCxdCtC,
                    adjust = max(max(abs(tmp$P2IVxd$PCxdCtC))/(EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),1),
                    from=(-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
                    to=EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
                    n=(2*EvalFuncSetting$UdlStepNum)+1)

lines(est_density,xlim=c(-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
                         EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
      ylim=c(0,1))
frame()

plot(est_density$x,est_density$y,col="blue")
lines(est_density,xlim=c(-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
                         EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
      ylim=c(0,1))


est_weight=est_density$y/sum(est_density$y)

frame()
plot(est_density$x,est_weight,col="blue")

anlzd_sd<-histIV[1]/100*HV_IV_Adjust_Ratio
sd_hd<-(anlzd_sd/sqrt(252/EvalFuncSetting$holdDays))
sknm_weight<-dsn(est_density$x,
                 xi=EvalFuncSetting$holdDays/252*EvalFuncSetting$Weight_Drift,
                 alpha=EvalFuncSetting$Weight_Skew*sd_hd,
                 omega=sd_hd)
sknm_weight=sknm_weight/sum(sknm_weight)
par(new=T)
plot(est_density$x,sknm_weight,col="red")

print(est_weight)
print(sknm_weight)

cat("c(");cat(est_weight,sep="$");cat(")")

min(tmp$P2IVxd$PCxdCtC)
#which.min(tmp$P2IVxd$PCxdCtC)
Date[which.min(tmp$P2IVxd$PCxdCtC)]
tmp$P2IVxd$IVCFxdCtC[which.min(tmp$P2IVxd$PCxdCtC)]
#sd, mean and skewnewss
sd(tmp$P2IVxd$PCxdCtC)
mean(tmp$P2IVxd$PCxdCtC)
mean((tmp$P2IVxd$PCxdCtC-mean(tmp$P2IVxd$PCxdCtC))^3)/(sd(tmp$P2IVxd$PCxdCtC)^3)
#annualized sd and mean
cat("sd anlzd",sd(tmp$P2IVxd$PCxdCtC)*sqrt(225/xDayInt),"\n")
cat("drift anlzd",mean(tmp$P2IVxd$PCxdCtC)*(225/xDayInt),"\n")
#Skewness as a multiple of SD
cat("skew",
    mean((tmp$P2IVxd$PCxdCtC-mean(tmp$P2IVxd$PCxdCtC))^3),"\n")

##
# Histgram One Day
xDayInt=1
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_WEIGHT_ESTM){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
h <- dpih(tmp$P2IVxd$PCxdCtC)
bins <- seq(min(tmp$P2IVxd$PCxdCtC)-3*h/2, max(tmp$P2IVxd$PCxdCtC)+3*h/2, by=h)
hist(tmp$P2IVxd$PCxdCtC, breaks=bins)
rm(h)

est_density=density(tmp$P2IVxd$PCxdCtC,
                    adjust = max(max(abs(tmp$P2IVxd$PCxdCtC))/(EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),1),
                    from=(-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
                    to=EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
                    n=(2*EvalFuncSetting$UdlStepNum)+1)

lines(est_density,xlim=c(-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
                         EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
      ylim=c(0,1))
frame()

plot(est_density$x,est_density$y,col="blue")
lines(est_density,xlim=c(-EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum,
                         EvalFuncSetting$UdlStepPct*EvalFuncSetting$UdlStepNum),
      ylim=c(0,1))


est_weight=est_density$y/sum(est_density$y)

frame()
plot(est_density$x,est_weight,col="blue")

HV_IV_Adjust_Ratio
anlzd_sd<-histIV[1]/100*HV_IV_Adjust_Ratio
sd_hd<-(anlzd_sd/sqrt(252/1))
sknm_weight<-dsn(est_density$x,
                 xi=EvalFuncSetting$holdDays/252*EvalFuncSetting$Weight_Drift,
                 alpha=EvalFuncSetting$Weight_Skew*sd_hd,
                 omega=sd_hd)
sknm_weight=sknm_weight/sum(sknm_weight)
par(new=T)
plot(est_density$x,sknm_weight,col="red")

print(est_weight)
print(sknm_weight)

cat("c(");cat(est_weight,sep="$");cat(")")

min(tmp$P2IVxd$PCxdCtC)
#which.min(tmp$P2IVxd$PCxdCtC)
Date[which.min(tmp$P2IVxd$PCxdCtC)]
tmp$P2IVxd$IVCFxdCtC[which.min(tmp$P2IVxd$PCxdCtC)]
#sd, mean and skewnewss
sd(tmp$P2IVxd$PCxdCtC)
mean(tmp$P2IVxd$PCxdCtC)
mean((tmp$P2IVxd$PCxdCtC-mean(tmp$P2IVxd$PCxdCtC))^3)/(sd(tmp$P2IVxd$PCxdCtC)^3)
#annualized sd and mean
cat("sd anlzd",sd(tmp$P2IVxd$PCxdCtC)*sqrt(225/xDayInt),"\n")
cat("drift anlzd",mean(tmp$P2IVxd$PCxdCtC)*(225/xDayInt),"\n")
#Skewness as a multiple of SD
cat("skew",
    mean((tmp$P2IVxd$PCxdCtC-mean(tmp$P2IVxd$PCxdCtC))^3),"\n")

