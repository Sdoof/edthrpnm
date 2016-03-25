library(RQuantLib)
library(ggplot2)
library(dplyr)
library(pracma)
source('./ESourceRCode.R',encoding = 'UTF-8')

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

#Check Option
(opchain)

#### Sampling
### DOUBLE DIAGONAL Candiates for Combination
sampleSpreadType=DOUBLE_DIAGONAL_SMPLING
targetExpDate="2016/5/19"
targetExpDate_f=targetExpDate
targetExpDate_b="2016/6/16"
spreadRatio=c(1,1,1)
totalPopNum=10000
InitialPopThresh=2.5
EvalFuncSetting$holdDays=12
EvalFuncSetting$Profit_Coef=1
EvalFuncSetting$AdvEffect_Coef=0
EvalFuncSetting$DrctlEffect_Coef=0.6
EvalFuncSetting$MaxLoss_Coef=0.4
EvalFuncSetting$Delta_Thresh_Minus=rep(-10,times=10)
EvalFuncSetting$Delta_Thresh_Plus=rep(10,times=10)
EvalFuncSetting$Vega_Thresh_Minus=rep(-100,times=10)
EvalFuncSetting$Vega_Thresh_Plus=rep(100,times=10)
#EvalFuncSetting$Vega_Thresh_Plus=rep(2,times=10)

outFname=paste(".\\ResultData\\",Underying_Symbol_G,"_SpreadResult-",sampleSpreadType[1],"-",spreadRatio[1],spreadRatio[2],spreadRatio[3],
               "-",EvalFuncSetting$holdDays,"-",EvalFuncSetting$Profit_Coef,"_",EvalFuncSetting$AdvEffect_Coef,"-",
               EvalFuncSetting$Delta_Thresh_Plus[1],"_",EvalFuncSetting$Delta_Thresh_Minus[1],"-",
               EvalFuncSetting$Vega_Thresh_Plus[1],"_",EvalFuncSetting$Vega_Thresh_Minus[1],"-",
               format(as.Date(targetExpDate,format="%Y/%m/%d"),"%Y%m%d"),"_",
               format(as.Date(targetExpDate_f,format="%Y/%m/%d"),"%Y%m%d"),"_",format(as.Date(targetExpDate_b,format="%Y/%m/%d"),"%Y%m%d"),
               ".csv",sep="")

sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
           targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
           spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)

###IRON_CONDOR Candidates for Combination
sampleSpreadType=IRON_CONDOR_SMPLING
targetExpDate="2016/5/19"
targetExpDate_f=targetExpDate
targetExpDate_b=targetExpDate
spreadRatio=c(1,1,1)
totalPopNum=50000
InitialPopThresh=2.5
EvalFuncSetting$holdDays=12
EvalFuncSetting$Profit_Coef=1
EvalFuncSetting$AdvEffect_Coef=0
EvalFuncSetting$DrctlEffect_Coef=0.6
EvalFuncSetting$MaxLoss_Coef=0.4
EvalFuncSetting$Delta_Thresh_Minus=rep(-15,times=10)
EvalFuncSetting$Delta_Thresh_Plus=rep(15,times=10)
EvalFuncSetting$Vega_Thresh_Minus=rep(-200,times=10)
EvalFuncSetting$Vega_Thresh_Plus=rep(200,times=10)

outFname=paste(".\\ResultData\\",Underying_Symbol_G,"_SpreadResult-",sampleSpreadType[1],"-",spreadRatio[1],spreadRatio[2],spreadRatio[3],
               "-",EvalFuncSetting$holdDays,"-",EvalFuncSetting$Profit_Coef,"_",EvalFuncSetting$AdvEffect_Coef,"-",
               EvalFuncSetting$Delta_Thresh_Plus[1],"_",EvalFuncSetting$Delta_Thresh_Minus[1],"-",
               EvalFuncSetting$Vega_Thresh_Plus[1],"_",EvalFuncSetting$Vega_Thresh_Minus[1],"-",
               format(as.Date(targetExpDate,format="%Y/%m/%d"),"%Y%m%d"),"_",
               format(as.Date(targetExpDate_f,format="%Y/%m/%d"),"%Y%m%d"),"_",format(as.Date(targetExpDate_b,format="%Y/%m/%d"),"%Y%m%d"),
               ".csv",sep="")

sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
           targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
           spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)

###CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONA
sampleSpreadType=CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING
targetExpDate="2016/5/19"
targetExpDate_f=targetExpDate
targetExpDate_b="2016/6/16"
spreadRatio=c(1,1,1)
totalPopNum=30000
InitialPopThresh=2.5
EvalFuncSetting$holdDays=10
EvalFuncSetting$Profit_Coef=0.5
EvalFuncSetting$AdvEffect_Coef=0.5
EvalFuncSetting$DrctlEffect_Coef=0.6
EvalFuncSetting$MaxLoss_Coef=0.4
EvalFuncSetting$Delta_Thresh_Minus=rep(-5,times=10)
EvalFuncSetting$Delta_Thresh_Plus=rep(5,times=10)
EvalFuncSetting$Vega_Thresh_Minus=rep(-10,times=10)
EvalFuncSetting$Vega_Thresh_Plus=rep(10,times=10)

outFname=paste(".\\ResultData\\",Underying_Symbol_G,"_SpreadResult-",sampleSpreadType[1],"-",spreadRatio[1],spreadRatio[2],spreadRatio[3],
               "-",EvalFuncSetting$holdDays,"-",EvalFuncSetting$Profit_Coef,"_",EvalFuncSetting$AdvEffect_Coef,"-",
               EvalFuncSetting$Delta_Thresh_Plus[1],"_",EvalFuncSetting$Delta_Thresh_Minus[1],"-",
               EvalFuncSetting$Vega_Thresh_Plus[1],"_",EvalFuncSetting$Vega_Thresh_Minus[1],"-",
               format(as.Date(targetExpDate,format="%Y/%m/%d"),"%Y%m%d"),"_",
               format(as.Date(targetExpDate_f,format="%Y/%m/%d"),"%Y%m%d"),"_",format(as.Date(targetExpDate_b,format="%Y/%m/%d"),"%Y%m%d"),
               ".csv",sep="")

sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
           targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
           spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)

###CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL
sampleSpreadType=CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
targetExpDate="2016/5/19"
targetExpDate_f=targetExpDate
targetExpDate_b="2016/6/16"
spreadRatio=c(1,1,1)
totalPopNum=30000
InitialPopThresh=2.5
EvalFuncSetting$holdDays=10
EvalFuncSetting$Profit_Coef=0.5
EvalFuncSetting$AdvEffect_Coef=0.5
EvalFuncSetting$DrctlEffect_Coef=0.6
EvalFuncSetting$MaxLoss_Coef=0.4
EvalFuncSetting$Delta_Thresh_Minus=rep(-5,times=10)
EvalFuncSetting$Delta_Thresh_Plus=rep(5,times=10)
EvalFuncSetting$Vega_Thresh_Minus=rep(-30,times=10)
EvalFuncSetting$Vega_Thresh_Plus=rep(30,times=10)

outFname=paste(".\\ResultData\\",Underying_Symbol_G,"_SpreadResult-",sampleSpreadType[1],"-",spreadRatio[1],spreadRatio[2],spreadRatio[3],
               "-",EvalFuncSetting$holdDays,"-",EvalFuncSetting$Profit_Coef,"_",EvalFuncSetting$AdvEffect_Coef,"-",
               EvalFuncSetting$Delta_Thresh_Plus[1],"_",EvalFuncSetting$Delta_Thresh_Minus[1],"-",
               EvalFuncSetting$Vega_Thresh_Plus[1],"_",EvalFuncSetting$Vega_Thresh_Minus[1],"-",
               format(as.Date(targetExpDate,format="%Y/%m/%d"),"%Y%m%d"),"_",
               format(as.Date(targetExpDate_f,format="%Y/%m/%d"),"%Y%m%d"),"_",format(as.Date(targetExpDate_b,format="%Y/%m/%d"),"%Y%m%d"),
               ".csv",sep="")

sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
           targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
           spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)


### DOUBLE DIAGONAL Candiates for Completion
# sampleSpreadType=DOUBLE_DIAGONAL_SMPLING
# targetExpDate="2016/5/19"
# targetExpDate_f=targetExpDate
# targetExpDate_b="2016/6/16"
# spreadRatio=c(1,1,1)
# totalPopNum=10000
# InitialPopThresh=2.5
# EvalFuncSetting$holdDays=12
# EvalFuncSetting$Profit_Coef=1
# EvalFuncSetting$AdvEffect_Coef=0
# EvalFuncSetting$DrctlEffect_Coef=0.6
# EvalFuncSetting$MaxLoss_Coef=0.4
# EvalFuncSetting$Delta_Thresh_Minus=rep(-2,times=10)
# EvalFuncSetting$Delta_Thresh_Plus=rep(1,times=10)
# EvalFuncSetting$Vega_Thresh_Minus=rep(-30,times=10)
# EvalFuncSetting$Vega_Thresh_Plus=rep(30,times=10)
# #EvalFuncSetting$Vega_Thresh_Plus=rep(2,times=10)
# 
# outFname=paste(".\\ResultData\\",Underying_Symbol_G,"_SpreadResult-",sampleSpreadType[1],"-",spreadRatio[1],spreadRatio[2],spreadRatio[3],
#                "-",EvalFuncSetting$holdDays,"-",EvalFuncSetting$Profit_Coef,"_",EvalFuncSetting$AdvEffect_Coef,"-",
#                EvalFuncSetting$Delta_Thresh_Plus[1],"_",EvalFuncSetting$Delta_Thresh_Minus[1],"-",
#                EvalFuncSetting$Vega_Thresh_Plus[1],"_",EvalFuncSetting$Vega_Thresh_Minus[1],"-",
#                format(as.Date(targetExpDate,format="%Y/%m/%d"),"%Y%m%d"),"_",
#                format(as.Date(targetExpDate_f,format="%Y/%m/%d"),"%Y%m%d"),"_",format(as.Date(targetExpDate_b,format="%Y/%m/%d"),"%Y%m%d"),
#                ".csv",sep="")
# 
# sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
#            targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
#            spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)

###IRON_CONDOR for Completion
# sampleSpreadType=IRON_CONDOR_SMPLING
# targetExpDate="2016/5/19"
# targetExpDate_f=targetExpDate
# targetExpDate_b=targetExpDate
# spreadRatio=c(1,1,1)
# totalPopNum=50000
# InitialPopThresh=2.5
# EvalFuncSetting$holdDays=12
# EvalFuncSetting$Profit_Coef=1
# EvalFuncSetting$AdvEffect_Coef=0
# EvalFuncSetting$DrctlEffect_Coef=0.6
# EvalFuncSetting$MaxLoss_Coef=0.4
# EvalFuncSetting$Delta_Thresh_Minus=rep(-3,times=10)
# EvalFuncSetting$Delta_Thresh_Plus=rep(3,times=10)
# EvalFuncSetting$Vega_Thresh_Minus=rep(-200,times=10)
# EvalFuncSetting$Vega_Thresh_Plus=rep(200,times=10)
# 
# outFname=paste(".\\ResultData\\",Underying_Symbol_G,"_SpreadResult-",sampleSpreadType[1],"-",spreadRatio[1],spreadRatio[2],spreadRatio[3],
#                "-",EvalFuncSetting$holdDays,"-",EvalFuncSetting$Profit_Coef,"_",EvalFuncSetting$AdvEffect_Coef,"-",
#                EvalFuncSetting$Delta_Thresh_Plus[1],"_",EvalFuncSetting$Delta_Thresh_Minus[1],"-",
#                EvalFuncSetting$Vega_Thresh_Plus[1],"_",EvalFuncSetting$Vega_Thresh_Minus[1],"-",
#                format(as.Date(targetExpDate,format="%Y/%m/%d"),"%Y%m%d"),"_",
#                format(as.Date(targetExpDate_f,format="%Y/%m/%d"),"%Y%m%d"),"_",format(as.Date(targetExpDate_b,format="%Y/%m/%d"),"%Y%m%d"),
#                ".csv",sep="")
# 
# sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
#            targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
#            spreadRatio=spreadRatio,InitialPopThresh=InitialPopThresh,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)


# ##Simple Test
# y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
#                        verticalType=BULL_VERTICAL_SPREAD_TYPE,
#                        targetExpDate="2016/5/19",isDebug=T,isDetail=T)
# hollowNonZeroPosition(y)
# 
# y=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
#                        verticalType=BEAR_VERTICAL_SPREAD_TYPE,
#                        targetExpDate="2016/5/19",isDebug=T,isDetail=T)
# hollowNonZeroPosition(y)
# 
# z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
#                        diagonalType=DIAGONAL_TYPE_LONG,
#                        targetExpDate_f="2016/5/19",targetExpDate_b="2016/6/16",
#                        isDebug=FALSE,isDetail=FALSE)
# hollowNonZeroPosition(z)
# 
# y=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
#                        diagonalType=DIAGONAL_TYPE_LONG,
#                        targetExpDate_f="2016/5/19",targetExpDate_b="2016/6/16",
#                        isDebug=FALSE,isDetail=FALSE)
# hollowNonZeroPosition(y)
# 
# y=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
#                        diagonalType=DIAGONAL_TYPE_SHORT,
#                        targetExpDate_f="2016/5/19",targetExpDate_b="2016/6/16",
#                        isDebug=FALSE,isDetail=FALSE)
# hollowNonZeroPosition(y)
# 
# y=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
#                        diagonalType=DIAGONAL_TYPE_SHORT,
#                        targetExpDate_f="2016/5/19",targetExpDate_b="2016/6/16",
#                        isDebug=FALSE,isDetail=FALSE)
# hollowNonZeroPosition(y)
# 
# ##Iron Condor Sample
# y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
#                        verticalType=BULL_VERTICAL_SPREAD_TYPE,
#                        targetExpDate="2016/5/19",isDebug=T,isDetail=T)
# z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
#                        verticalType=BEAR_VERTICAL_SPREAD_TYPE,
#                        targetExpDate="2016/5/19",isDebug=T,isDetail=T)
# x<-y+z
# hollowNonZeroPosition(x)

