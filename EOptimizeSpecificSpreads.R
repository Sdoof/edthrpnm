library(RQuantLib)
library(ggplot2)
library(dplyr)
library(pracma)
source('./ESourceRCode.R',encoding = 'UTF-8')

##specify each time
TARGET_EXPDATE="2016/5/19"
TARGET_EXPDATE_FRONT="2016/5/19"
TARGET_EXPDATE_BACK="2016/6/16"

##Spread Type Definition
BULL_VERTICAL_SPREAD_TYPE=1
BEAR_VERTICAL_SPREAD_TYPE=-1

#DIAGONAL Type Definition
DIAGONAL_TYPE_LONG=1
DIAGONAL_TYPE_SHORT=-1

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

SpreadTypeNames<-vector("list",9)
SpreadTypeNames[[IRON_CONDOR_SMPLING]]="IRON_CONDOR"
SpreadTypeNames[[DIAGONAL_SMPLING]]="DIAGONAL"
SpreadTypeNames[[DOUBLE_DIAGONAL_SMPLING]]="DOUBLE_DIAGONAL"
SpreadTypeNames[[IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING]]="IRON_CONDOR_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING]]="IRON_CONDOR_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[CALL_BEAR_SPREAD_SMPLING]]="CALL_BEAR_SPREAD"
SpreadTypeNames[[CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING]]="CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]="CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]="PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING"

SpreadTypeToDir<-vector("list",length(SpreadTypeNames))
SpreadTypeToDir[[IRON_CONDOR_SMPLING]]=1
SpreadTypeToDir[[DIAGONAL_SMPLING]]=1
SpreadTypeToDir[[DOUBLE_DIAGONAL_SMPLING]]=2
SpreadTypeToDir[[IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING]]=0
SpreadTypeToDir[[IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING]]=0
SpreadTypeToDir[[CALL_BEAR_SPREAD_SMPLING]]=0
SpreadTypeToDir[[CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING]]=0
SpreadTypeToDir[[CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]=3
SpreadTypeToDir[[PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]=3

#Which directory(folder) this instance belongs
dirInstance=1
if(length(grep("2", ConfigFileName_G))<1 && length(grep("3", ConfigFileName_G))<1 )
  dirInstance=1
if(length(grep("2", ConfigFileName_G))>=1)
  dirInstance=2
if(length(grep("3", ConfigFileName_G))>=1)
  dirInstance=3

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
### DOUBLE DIAGONAL Candiates for Combination
sampleSpreadType=DOUBLE_DIAGONAL_SMPLING
if(SpreadTypeToDir[[sampleSpreadType]]==dirInstance){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=70000
  InitialPopThresh=2.5
  # EvalFuncSetting$holdDays=12
  # EvalFuncSetting$Profit_Coef=0.5
  # EvalFuncSetting$AdvEffect_Coef=0.5
  # EvalFuncSetting$DrctlEffect_Coef=0.6
  # EvalFuncSetting$MaxLoss_Coef=0.4
  EvalFuncSetting$Delta_Thresh_Minus=rep(-10,times=10)
  EvalFuncSetting$Delta_Thresh_Plus=rep(10,times=10)
  EvalFuncSetting$Vega_Thresh_Minus=rep(-100,times=10)
  EvalFuncSetting$Vega_Thresh_Plus=rep(100,times=10)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)

  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

###IRON_CONDOR Candidates for Combination
sampleSpreadType=IRON_CONDOR_SMPLING
if(SpreadTypeToDir[[sampleSpreadType]]==dirInstance){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=targetExpDate
  targetExpDate_b=targetExpDate
  spreadRatio=c(1,1,1)
  totalPopNum=20000
  InitialPopThresh=2.5
  # EvalFuncSetting$holdDays=12
  # EvalFuncSetting$Profit_Coef=0.5
  # EvalFuncSetting$AdvEffect_Coef=0.5
  # EvalFuncSetting$DrctlEffect_Coef=0.6
  # EvalFuncSetting$MaxLoss_Coef=0.4
  EvalFuncSetting$Delta_Thresh_Minus=rep(-15,times=10)
  EvalFuncSetting$Delta_Thresh_Plus=rep(15,times=10)
  EvalFuncSetting$Vega_Thresh_Minus=rep(-200,times=10)
  EvalFuncSetting$Vega_Thresh_Plus=rep(200,times=10)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)

  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

### DIAGONAL Candiates for Combination
sampleSpreadType=DIAGONAL_SMPLING
if(SpreadTypeToDir[[sampleSpreadType]]==dirInstance){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=5000
  InitialPopThresh=2.5
  # EvalFuncSetting$holdDays=12
  # EvalFuncSetting$Profit_Coef=0.5
  # EvalFuncSetting$AdvEffect_Coef=0.5
  # EvalFuncSetting$DrctlEffect_Coef=0.6
  # EvalFuncSetting$MaxLoss_Coef=0.4
  EvalFuncSetting$Delta_Thresh_Minus=rep(-10,times=10)
  EvalFuncSetting$Delta_Thresh_Plus=rep(10,times=10)
  EvalFuncSetting$Vega_Thresh_Minus=rep(-100,times=10)
  EvalFuncSetting$Vega_Thresh_Plus=rep(100,times=10)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

###CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL
sampleSpreadType=CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING
if(SpreadTypeToDir[[sampleSpreadType]]==dirInstance){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=60000
  InitialPopThresh=2.5
  # EvalFuncSetting$holdDays=12
  # EvalFuncSetting$Profit_Coef=0.5
  # EvalFuncSetting$AdvEffect_Coef=0.5
  # EvalFuncSetting$DrctlEffect_Coef=0.6
  # EvalFuncSetting$MaxLoss_Coef=0.4
  EvalFuncSetting$Delta_Thresh_Minus=rep(-1,times=10)
  EvalFuncSetting$Delta_Thresh_Plus=rep(1,times=10)
  EvalFuncSetting$Vega_Thresh_Minus=rep(-10,times=10)
  EvalFuncSetting$Vega_Thresh_Plus=rep(10,times=10)
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #spread ratio 2
  spreadRatio=c(2,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

###CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL
sampleSpreadType=CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
if(SpreadTypeToDir[[sampleSpreadType]]==dirInstance){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=200000
  InitialPopThresh=2.5
  # EvalFuncSetting$holdDays=12
  # EvalFuncSetting$Profit_Coef=0.5
  # EvalFuncSetting$AdvEffect_Coef=0.5
  # EvalFuncSetting$DrctlEffect_Coef=0.6
  # EvalFuncSetting$MaxLoss_Coef=0.4
  EvalFuncSetting$Delta_Thresh_Minus=rep(-1,times=10)
  EvalFuncSetting$Delta_Thresh_Plus=rep(1,times=10)
  EvalFuncSetting$Vega_Thresh_Minus=rep(-10,times=10)
  EvalFuncSetting$Vega_Thresh_Plus=rep(10,times=10)
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)

  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
 
  #spread ratio 2
  spreadRatio=c(2,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

###PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
sampleSpreadType=PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
if(SpreadTypeToDir[[sampleSpreadType]]==dirInstance){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=200000
  InitialPopThresh=2.5
  # EvalFuncSetting$holdDays=12
  # EvalFuncSetting$Profit_Coef=0.5
  # EvalFuncSetting$AdvEffect_Coef=0.5
  # EvalFuncSetting$DrctlEffect_Coef=0.6
  # EvalFuncSetting$MaxLoss_Coef=0.4
  EvalFuncSetting$Delta_Thresh_Minus=rep(-1,times=10)
  EvalFuncSetting$Delta_Thresh_Plus=rep(1,times=10)
  EvalFuncSetting$Vega_Thresh_Minus=rep(-10,times=10)
  EvalFuncSetting$Vega_Thresh_Plus=rep(10,times=10)
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #spread ratio 2
  spreadRatio=c(2,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}


### DOUBLE DIAGONAL Candiates for Completion
sampleSpreadType=DOUBLE_DIAGONAL_SMPLING
if(SpreadTypeToDir[[sampleSpreadType]]==dirInstance){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=70000
  InitialPopThresh=2.5
  # EvalFuncSetting$holdDays=12
  # EvalFuncSetting$Profit_Coef=1
  # EvalFuncSetting$AdvEffect_Coef=0
  # EvalFuncSetting$DrctlEffect_Coef=0.6
  # EvalFuncSetting$MaxLoss_Coef=0.4
  EvalFuncSetting$Delta_Thresh_Minus=rep(-1,times=10)
  EvalFuncSetting$Delta_Thresh_Plus=rep(1,times=10)
  EvalFuncSetting$Vega_Thresh_Minus=rep(-30,times=10)
  EvalFuncSetting$Vega_Thresh_Plus=rep(30,times=10)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)

  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  #EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  EvalFuncSetting$LossLimitPrice=(-3000)*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

###IRON_CONDOR for Completion
sampleSpreadType=IRON_CONDOR_SMPLING
if(SpreadTypeToDir[[sampleSpreadType]]==dirInstance){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=targetExpDate
  targetExpDate_b=targetExpDate
  spreadRatio=c(1,1,1)
  totalPopNum=20000
  InitialPopThresh=2.5
  # EvalFuncSetting$holdDays=12
  # EvalFuncSetting$Profit_Coef=1
  # EvalFuncSetting$AdvEffect_Coef=0
  # EvalFuncSetting$DrctlEffect_Coef=0.6
  # EvalFuncSetting$MaxLoss_Coef=0.4
  EvalFuncSetting$DeltaHedge=T
  EvalFuncSetting$Delta_Thresh_Minus=rep(-1,times=10)
  EvalFuncSetting$Delta_Thresh_Plus=rep(1,times=10)
  EvalFuncSetting$Vega_Thresh_Minus=rep(-200,times=10)
  EvalFuncSetting$Vega_Thresh_Plus=rep(200,times=10)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  #EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  EvalFuncSetting$LossLimitPrice=(-3000)*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}
