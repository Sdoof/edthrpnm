library(RQuantLib)
library(ggplot2)
library(dplyr)
library(pracma)
library(digest)
library(hash)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#Initial Population evalfunc value
InitialPopThresh=3.0

#Debug, Detail Mode
IS_DEBUG_MODE=F
IS_DETAL_MODE=F

##Spread Type Definition
BULL_VERTICAL_SPREAD_TYPE=1
BEAR_VERTICAL_SPREAD_TYPE=(-1)

#DIAGONAL Type Definition
DIAGONAL_TYPE_LONG=1
DIAGONAL_TYPE_SHORT=(-1)

#sample type Definition
IRON_CONDOR_SMPLING=1
DIAGONAL_SMPLING=2
DOUBLE_DIAGONAL_SMPLING=3
IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING=4
IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING=5
CALL_BEAR_SPREAD_SMPLING=6
CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING=7
CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING=8
PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING=9
POOL_PLUS_SINGLE_DIAGONAL_SMPLING=10
POOL_PLUS_DOUBLE_DIAGONAL_SMPLING=11
CALL_BEAR_SPREAD_SMPLING=12
PUT_BULL_SPREAD_SMPLING=13

SpreadTypeNames<-vector("list",9)
SpreadTypeNames[[IRON_CONDOR_SMPLING]]="IRON_CONDOR"
SpreadTypeNames[[DIAGONAL_SMPLING]]="DIAGONAL"
SpreadTypeNames[[DOUBLE_DIAGONAL_SMPLING]]="DOUBLE_DIAGONAL"
SpreadTypeNames[[IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING]]="IRON_CONDOR_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING]]="IRON_CONDOR_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[CALL_BEAR_SPREAD_SMPLING]]="CALL_BEAR_SPREAD"
SpreadTypeNames[[CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING]]="CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]="CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]="PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[POOL_PLUS_SINGLE_DIAGONAL_SMPLING]]="POOL_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[POOL_PLUS_DOUBLE_DIAGONAL_SMPLING]]="POOL_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[CALL_BEAR_SPREAD_SMPLING]]="CALL_BEAR_SPREAD"
SpreadTypeNames[[PUT_BULL_SPREAD_SMPLING]]="PUT_BULL_SPREAD"

SpreadTypeToDir<-vector("list",length(SpreadTypeNames))
SpreadTypeToDir[[IRON_CONDOR_SMPLING]]=1
SpreadTypeToDir[[DIAGONAL_SMPLING]]=0
SpreadTypeToDir[[DOUBLE_DIAGONAL_SMPLING]]=2
SpreadTypeToDir[[IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING]]=0
SpreadTypeToDir[[IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING]]=0
SpreadTypeToDir[[CALL_BEAR_SPREAD_SMPLING]]=0
SpreadTypeToDir[[CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING]]=0
SpreadTypeToDir[[CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]=3
SpreadTypeToDir[[PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]=3
SpreadTypeToDir[[POOL_PLUS_SINGLE_DIAGONAL_SMPLING]]=2
SpreadTypeToDir[[POOL_PLUS_DOUBLE_DIAGONAL_SMPLING]]=1

#Which directory(folder) this instance belongs
dirInstance=1
if(length(grep("2", ConfigFileName_G))<1 && length(grep("3", ConfigFileName_G))<1 )
  dirInstance=1
if(length(grep("2", ConfigFileName_G))>=1)
  dirInstance=2
if(length(grep("3", ConfigFileName_G))>=1)
  dirInstance=3
#dirInstance=3

#Check Option
(opchain)

#functions used for this script only
createOutFname<-function(targetExpDate,targetExpDate_f,targetExpDate_b,spreadRatio,EvalFuncSetting){
  outFname=paste(".\\ResultData\\",Underying_Symbol_G,"-",SpreadTypeNames[[sampleSpreadType]],"-",spreadRatio[1],spreadRatio[2],spreadRatio[3],"-",
                 EvalFuncSetting$UdlStepPct*1000,"x",EvalFuncSetting$UdlStepNum,"-",EvalFuncSetting$holdDays,"d-",EvalFuncSetting$Profit_Coef,"_",EvalFuncSetting$AdvEffect_Coef,"-",
                 EvalFuncSetting$DrctlEffect_Coef,"_",EvalFuncSetting$MaxLoss_Coef,"-",
                 "LLt$",EvalFuncSetting$LossLimitPrice,"-",
                 "DTh$",EvalFuncSetting$Delta_Thresh_Plus[1],"_",EvalFuncSetting$Delta_Thresh_Minus[1],"-",
                 "VTh$",EvalFuncSetting$Vega_Thresh_Plus[1],"_",EvalFuncSetting$Vega_Thresh_Minus[1],"-",
                 "HIVR$",EvalFuncSetting$HV_IV_Adjust_Ratio,"-",
                 "cvx$",ifelse(EvalFuncSetting$EvalConvex,"T","F"),"-",
                 "VDrt$",mean(EvalFuncSetting$Vega_Direct_Prf),"-",
                 "GkA$",ifelse(EvalFuncSetting$GreekEfctOnHldD,"T","F"),"-",
                 "dft$",EvalFuncSetting$Weight_Drift*100,"-",
                 "DHg$",ifelse(EvalFuncSetting$DeltaHedge,"T","F"),"-",
                 "sgd$",EvalFuncSetting$SigmoidA_Numerator,"_",EvalFuncSetting$SigmoidA_Denominator,"-",
                 "tgD$",format(as.Date(targetExpDate,format="%Y/%m/%d"),"%m%d"),"_",format(as.Date(targetExpDate_f,format="%Y/%m/%d"),"%m%d"),"_",format(as.Date(targetExpDate_b,format="%Y/%m/%d"),"%m%d"),
                 ".csv",sep="")
  return(outFname)
}

#### Sampling

###IRON_CONDOR Candidates for Combination
sampleSpreadType=IRON_CONDOR_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==dirInstance)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=targetExpDate
  targetExpDate_b=targetExpDate
  spreadRatio=c(1,1,1)
  totalPopNum=200
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=min(UNACCEPTABLEVAL-0.1,InitialPopThresh*1.5),outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>% #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
 
  #pool setting
  tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(min(20,max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
}

### DOUBLE DIAGONAL Candiates for Combination
sampleSpreadType=DOUBLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==dirInstance)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=30000
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)

  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>% #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  #pool setting
  tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(min(1000,max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
}

###CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL
sampleSpreadType=CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==dirInstance)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=20000
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  ##
  #  First CALL_BEAR_SPREAD_SMPLING sampling
  sampleSpreadType=CALL_BEAR_SPREAD_SMPLING
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)

  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  originalWeight_Drift=EvalFuncSetting$Weight_Drift
  EvalFuncSetting$Weight_Drift=0.0
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=100,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=min(UNACCEPTABLEVAL-0.1,InitialPopThresh*2),outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  EvalFuncSetting$Weight_Drift=originalWeight_Drift
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>% #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
 
   #pool setting
  tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(min(20,max(4,nrow(.)))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  ##
  #  Second POOL_PLUS_DOUBLE_DIAGONAL_SMPLING sampling, then saved as CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING sampling
  
  # sampleSpreadType for output file name
  sampleSpreadType=CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  # sampleSpreadType for sampling
  sampleSpreadType=POOL_PLUS_DOUBLE_DIAGONAL_SMPLING
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*2
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  # sampleSpreadType CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING again
  sampleSpreadType=CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  #remove pools
  rm(pools)
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>% #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #spread ratio 2
  # spreadRatio=c(2,1,1)
  # 
  # outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  # 
  #sampling
  # originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  # EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  # 
  # sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
  #            targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
  #            spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  # 
  # EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

###PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
sampleSpreadType=PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==dirInstance)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=30000
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  ##
  #  First PUT_BULL_SPREAD_SMPLING sampling
  sampleSpreadType=PUT_BULL_SPREAD_SMPLING
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  originalWeight_Drift=EvalFuncSetting$Weight_Drift
  EvalFuncSetting$Weight_Drift=0.0
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=100,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=min(UNACCEPTABLEVAL-0.1,InitialPopThresh*2),outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  EvalFuncSetting$Weight_Drift=originalWeight_Drift
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>% #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  #pool setting
  tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(min(40,max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))

  ##
  #  Second POOL_PLUS_DOUBLE_DIAGONAL_SMPLING sampling, then saved as PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING sampling
  
  # sampleSpreadType for output file name
  sampleSpreadType=PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  # sampleSpreadType for sampling
  sampleSpreadType=POOL_PLUS_DOUBLE_DIAGONAL_SMPLING
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*2
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  sampleSpreadType=PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  
  # sampleSpreadType PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING again
  sampleSpreadType=PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  #remove pools
  rm(pools)
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>% #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #spread ratio 2
  # spreadRatio=c(2,1,1)
  # 
  # outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  # 
  #sampling
  # originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  # EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  # 
  # sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
  #            targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
  #            spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  # 
  # EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

###POOL_PLUS_DOUBLE_DIAGONAL_SMPLING
sampleSpreadType=POOL_PLUS_DOUBLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==dirInstance)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=30000
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*2
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>% #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #spread ratio 2
  # spreadRatio=c(2,1,1)
  # 
  # outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  # originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  # EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  # 
  # sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
  #            targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
  #            spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  # 
  # EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

### POOL_PLUS_SINGLE_DIAGONAL_SMPLING
sampleSpreadType=POOL_PLUS_SINGLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==dirInstance)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=15000
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*2
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>% #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #spread ratio 2
  # spreadRatio=c(2,1,1)
  # 
  # outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  # originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  # EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  # 
  # sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
  #            targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
  #            spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  # 
  # EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

### DIAGONAL Candiates for Combination
sampleSpreadType=DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==dirInstance)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=200
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=min(UNACCEPTABLEVAL-0.1,InitialPopThresh*1.5),outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>%  #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
}

###CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL
sampleSpreadType=CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==dirInstance)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=30000
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% arrange(.[,length(opchain$Position)+1])  %>% #select(.,1:length(opchain$Position)) %>% 
    distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #spread ratio 2
  #spreadRatio=c(2,1,1)
  
  #outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  #originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  #EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  #sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
  #           targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
  #           spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  #EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}


