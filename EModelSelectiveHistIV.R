library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#Data Num.
DATA_NUM=504 # about 2 years equivalent 

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
a_low=0.8
d_low=5
a_high=1.25
d_high=5

##
# select Suffix to icnlude the day data
selectSuffixForValidIV <- function(histIV,xDayInt,a_low,d_low,a_high,d_high){
  theIV=histIV[1]
  SelectInclude=(histIV>=theIV*a_low | histIV>=(theIV-d_low))&(histIV<=theIV*a_high | histIV<=(theIV+d_high))
  
  SelectIncludeShift=numeric((length(SelectInclude)-xDayInt))
  
  SelectIncludeShift[1:xDayInt]=SelectInclude[1:xDayInt]
  for(i in 1:(length(SelectInclude)-xDayInt)){
    SelectIncludeShift[i+xDayInt]=SelectInclude[i]|SelectInclude[i+xDayInt]
  }
  which(SelectIncludeShift==1)->suffix_slctd
  
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
  
  print(length(PCxdCtC))
  print(length(IVCFxdCtC))
  
  if(length(PCxdCtC)<=(start_day+dataNum))
    dataNum=length(PCxdCtC)-start_day
  
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
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC5dCtC_IVCF5dCtC

#####
##   3d
xDayInt=3
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC3dCtC_IVCF3dCtC

#####
##   7d
xDayInt=7
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC7dCtC_IVCF7dCtC

#####
##  12d
xDayInt=12
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC12dCtC_IVCF12dCtC

#####
##  18d
xDayInt=18
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC18dCtC_IVCF18dCtC

#####
##   1d
xDayInt=1
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
if(IS_SELECTIVE_HISTIV_REGR){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}
(gg<-ggplot(tmp$P2IVxd,aes(x=PCxdCtC,y=IVCFxdCtC))+geom_point())
gg+geom_abline(intercept=tmp$lm$coefficient[1],slope=tmp$lm$coefficient[2],color="orange")
#Load test
load.PC2IV(PC=paste("PC",xDayInt,"dCtC",sep=""),IVC=paste("IVCF",xDayInt,"dCtC",sep=""))
PC1dCtC_IVCF1dCtC


####
# expected HV predict

selectSuffixToPredictHV <- function(histIV,a_low,d_low,a_high,d_high){
  theIV=histIV[1]
  SelectInclude=(histIV>=theIV*a_low | histIV>=(theIV-d_low))&(histIV<=theIV*a_high | histIV<=(theIV+d_high))
  
  which(SelectInclude==TRUE)->suffix_slctd
  
  return(suffix_slctd)
}

effectiv_suffix=selectSuffixToPredictHV(histIV,a_low,d_low,a_high,d_high)
#effectiv_suffix=suffix_slctd
rm(suffix_slctd)

getHVIVRStat <- function(histPrc,histIV,xDayVol,effectiv_suffix){
  HVIVR_V=rep(0:length(effectiv_suffix))
  hvivr_suff=1
  for(i in 1:length(effectiv_suffix)){
    if(effectiv_suffix[i]>=xDayVol){
      histPrc[effectiv_suffix[i]:(effectiv_suffix[i]+xDayVol)]
      HVIVR=annuual.daily.volatility( histPrc[(effectiv_suffix[i]-xDayVol):effectiv_suffix[i]] )$anlzd/
        (histIV[(effectiv_suffix[i])]/100)
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

HVIVR=getHVIVRStat(histPrc,histIV,xDayVol=20,effectiv_suffix)

mean(HVIVR)
sd(HVIVR)
mean(HVIVR[1:100])
sd(HVIVR[1:100])


