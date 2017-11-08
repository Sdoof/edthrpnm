#First determine Which directory(folder) this instance belongs
currentdir=getwd()
dirInstance=""
if(length(grep("2",currentdir))>=1)
  dirInstance=2
if(length(grep("3",currentdir))>=1)
  dirInstance=3
if(length(grep("4",currentdir))>=1)
  dirInstance=4
if(length(grep("5",currentdir))>=1)
  dirInstance=5
if(length(grep("6",currentdir))>=1)
  dirInstance=6
if(length(grep("7",currentdir))>=1)
  dirInstance=7
if(length(grep("8",currentdir))>=1)
  dirInstance=8

#Config File
ConfigFileName_G=paste("ConfigParameters",dirInstance,".csv",sep="")
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")
###Global 変数及び定数.

##Eeach Target File
TARGET_EXPDATE=ConfigParameters["TARGET_EXPDATE",1]
TARGET_EXPDATE_FRONT=ConfigParameters["TARGET_EXPDATE_FRONT",1]
TARGET_EXPDATE_BACK=ConfigParameters["TARGET_EXPDATE_BACK",1]

#Definition
OpType_Put_G=as.numeric(ConfigParameters["OpType_Put_G",1])
OpType_Call_G=as.numeric(ConfigParameters["OpType_Call_G",1])

#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

#Calendar
CALENDAR_G=ConfigParameters["CALENDAR_G",1]

# Possibly read from File
riskFreeRate_G=as.numeric(ConfigParameters["riskFreeRate_G",1])
divYld_G=as.numeric(ConfigParameters["divYld_G",1])

#Holding Period
holdDays=as.numeric(ConfigParameters["holdDays",1])
#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays=as.numeric(ConfigParameters["dviv_caldays",1])
#Multipler of Position
PosMultip=as.numeric(ConfigParameters["PosMultip",1])

#Spread Type Specified
SpreadTypeSpecified=max(0,as.numeric(ConfigParameters["Spread_Type",1]))

#Evalfunc unaccepted value
UNACCEPTABLEVAL=9999999

#Limit Closeness for Skewness Calculation
TimeToExp_Limit_Closeness_G=as.numeric(ConfigParameters["TimeToExp_Limit_Closeness_G",1])

#EvalFuncSetting
EvFNames <- c("holdDays","ConditonalProfitEval","UdlStepNum","UdlStepPct","Posnum","CombineTargetGeneration",
              "LossLimitPrice",
              "HV_IV_Adjust_Ratio","PriceRangeDist","Weight_Explicit","Weight_Explicit_1D","EvalSigmoidFunc","EvalEconomicValue",
              "UnitMinProfit",
              "Delta_Thresh_Minus","Delta_Thresh_Plus","Vega_Thresh_Minus","Vega_Thresh_Plus",
              "Delta_Direct_Prf","Vega_Direct_Prf","Delta_Neutral_Offset","Vega_Neutral_Offset",
              "Profit_Coef","AdvEffect_Coef","AllEffect_Coef","DrctlEffect_Coef","MaxLoss_Coef",
              "SigmoidA_Numerator","SigmoidA_Denominator",
              "ExpIVChange_Multiple",
              "EvalConvex","ConvexNormalizeByProfit","ConvexEvalInCoef",
              "UseSortinoRatio",
              "Eval1DayDist","Eval1DayDistWeightROnHoldDay","Coef1DayDist",
              "DeltaHedge","GreekEfctOnHldD")

EvalFuncSetting<-vector("list",length(EvFNames))

EvalFuncSetting[[1]]<-as.numeric(ConfigParameters["holdDays",1])
EvalFuncSetting[[2]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_ConditonalProfitEval",1])==1,TRUE,FALSE)
EvalFuncSetting[[3]]<-as.numeric(ConfigParameters["EvalFnc_UdlStepNum",1])
EvalFuncSetting[[4]]<-as.numeric(ConfigParameters["EvalFnc_UdlStepPct",1])
EvalFuncSetting[[5]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Posnum",1])))
EvalFuncSetting[[6]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_CombineTargetGeneration",1])))
EvalFuncSetting[[7]]<-as.numeric(ConfigParameters["EvalFnc_LossLimitPrice",1])
EvalFuncSetting[[8]]<-as.numeric(ConfigParameters["EvalFnc_HV_IV_Adjust_Ratio",1])
EvalFuncSetting[[9]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_PriceRangeDist",1])))
EvalFuncSetting[[10]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Weight_Explicit",1])))
EvalFuncSetting[[11]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Weight_Explicit_1D",1])))
EvalFuncSetting[[12]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_EvalSigmoidFunc",1])==1,TRUE,FALSE)
EvalFuncSetting[[13]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_EvalEconomicValue",1])==1,TRUE,FALSE)
EvalFuncSetting[[14]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_UnitMinProfit",1])))
EvalFuncSetting[[15]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Thresh_Minus",1])))
EvalFuncSetting[[16]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Thresh_Plus",1])))
EvalFuncSetting[[17]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Thresh_Minus",1])))
EvalFuncSetting[[18]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Thresh_Plus",1])))
EvalFuncSetting[[19]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Direct_Prf",1])))
EvalFuncSetting[[20]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Direct_Prf",1])))
EvalFuncSetting[[21]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Neutral_Offset",1])))
EvalFuncSetting[[22]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Neutral_Offset",1])))
EvalFuncSetting[[23]]<-as.numeric(ConfigParameters["EvalFnc_Profit_Coef",1])
EvalFuncSetting[[24]]<-as.numeric(ConfigParameters["EvalFnc_AdvEffect_Coef",1])
EvalFuncSetting[[25]]<-as.numeric(ConfigParameters["EvalFnc_AllEffect_Coef",1]) 
EvalFuncSetting[[26]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_DrctlEffect_Coef",1])))
EvalFuncSetting[[27]]<-as.numeric(ConfigParameters["EvalFnc_MaxLoss_Coef",1])
EvalFuncSetting[[28]]<-as.numeric(ConfigParameters["EvalFnc_SigmoidA_Numerator",1])
EvalFuncSetting[[29]]<-as.numeric(ConfigParameters["EvalFnc_SigmoidA_Denominator",1])
EvalFuncSetting[[30]]<-as.numeric(ConfigParameters["EvalFnc_ExpIVChange_Multiple",1])
EvalFuncSetting[[31]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_EvalConvex",1])==1,TRUE,FALSE)
EvalFuncSetting[[32]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_ConvexNormalizeByProfit",1])==1,TRUE,FALSE)
EvalFuncSetting[[33]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_ConvexEvalInCoef",1])))
EvalFuncSetting[[34]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_UseSortinoRatio",1])==1,TRUE,FALSE)
EvalFuncSetting[[35]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_Eval1DayDist",1])==1,TRUE,FALSE)
EvalFuncSetting[[36]]<-as.numeric(ConfigParameters["EvalFnc_Eval1DayDistWeightROnHoldDay",1])
EvalFuncSetting[[37]]<-as.numeric(ConfigParameters["EvalFnc_Coef1DayDist",1])
EvalFuncSetting[[38]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_DeltaHedgeToEvalProfit",1])==1,TRUE,FALSE)
EvalFuncSetting[[39]]<-as.numeric(ConfigParameters["EvalFnc_GreekEffectEvalOnHoldDay",1])

names(EvalFuncSetting)<-EvFNames
names(EvalFuncSetting$DrctlEffect_Coef) <- c("DeltaECoef","VegaECoef") 
names(EvalFuncSetting$ConvexEvalInCoef) <- c("InCoefSD","InCoefMaxLoss") 

#MuLtipler for creating initial populaitons
Optimize_ml=as.numeric(ConfigParameters["Optimize_ml",1])

#Parameters for Combinational Optimization
InitialPopCreateLoopNum<-as.numeric(ConfigParameters["Optimize_InitialPopCreateLoopNum",1])
InitialPopThresh=as.numeric(ConfigParameters["Optimize_InitialPopThresh",1])
PopN=eval(parse(text=gsub("\\$",",",ConfigParameters["Optimize_PopN",1])))
TopN=eval(parse(text=gsub("\\$",",",ConfigParameters["Optimize_TopN",1])))
ThreshN=eval(parse(text=gsub("\\$",",",ConfigParameters["Optimize_Thresh",1])))

#Search Combined(2Cb,3Cb,etc) Spreads?
Combined_Spread=ifelse(as.numeric(ConfigParameters["Optimize_Combined_Spread",1])==1,TRUE,FALSE)

##Historical Implied Volatility Data
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",stringsAsFactors=F,nrows=100);rm(rf)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% #dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(opchain$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

##
# opchain,position,histIV,iniPos must exist as Global Variables.


#Option Chain and Position Data. Here we use UDL_Positions_Pre
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
tmp_datafile=paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre",dirInstance,".csv",sep="")
if(file.exists(tmp_datafile))
  rf<-tmp_datafile

#型を厳密に指定して読み込む場合
# headset <- read.table(rf,header=T,sep=",",stringsAsFactors=F,nrows=1)
# classes <- sapply(headset, class)
# classes[names(classes)]<-"numeric"
# classes[names(classes) %in% c("Date","ExpDate","ContactName")] <- "character"
# classes[names(classes) %in% c("TYPE","Strike","Position")] <- "integer"
# opchain<-read.table(rf,header=T,sep=",",stringsAsFactors=F,colClasses=classes) ; rm(headset)
opchain<-read.table(rf,header=T,sep=",",stringsAsFactors=F)

#get position where opchain$Position!=0
opchain %>% dplyr::filter(Position!=0) -> position

## iniPos must exist as Global Varibles to get the 
iniPos<-opchain$Position

#Global shread Hash
POSITION_OPTIM_HASH=hash()

#Data Setup. Provisioning
#Load Regression and Correlation Parameters
load.PC2IV(PC="PC1dCtC",IVC="IVCF1dCtC")
PC1dCtC_IVCF1dCtC
load.PC2IV(PC="PC3dCtC",IVC="IVCF3dCtC")
PC3dCtC_IVCF3dCtC
load.PC2IV(PC="PC5dCtC",IVC="IVCF5dCtC")
PC5dCtC_IVCF5dCtC
load.PC2IV(PC="PC7dCtC",IVC="IVCF7dCtC")
PC7dCtC_IVCF7dCtC
load.PC2IV(PC="PC12dCtC",IVC="IVCF12dCtC")
PC12dCtC_IVCF12dCtC
load.PC2IV(PC="PC18dCtC",IVC="IVCF18dCtC")
PC18dCtC_IVCF18dCtC
load.NLPC2IV(PC="PC1dCtC",IVC="IVCF1dCtC")
PC1dCtC_NL_IVCF1dCtC
load.NLPC2IV(PC="PC3dCtC",IVC="IVCF3dCtC")
PC3dCtC_NL_IVCF3dCtC
load.NLPC2IV(PC="PC5dCtC",IVC="IVCF5dCtC")
PC5dCtC_NL_IVCF5dCtC
load.NLPC2IV(PC="PC7dCtC",IVC="IVCF7dCtC")
PC7dCtC_NL_IVCF7dCtC
load.NLPC2IV(PC="PC12dCtC",IVC="IVCF12dCtC")
PC12dCtC_NL_IVCF12dCtC
load.NLPC2IV(PC="PC18dCtC",IVC="IVCF18dCtC")
PC18dCtC_NL_IVCF18dCtC
load.Skew()
SkewModel
load.Skew("_Put")
SkewModel_Put
load.Skew("_Call")
SkewModel_Call
#ATMIV.f.IVIDX.f
load.ATMIV.f.IVIDX.f(optype=OpType_Put_G,up_dn=10,days=1)
PutIVUp_ATMIV.f.IVIDX.f_1D
load.ATMIV.f.IVIDX.f(optype=OpType_Put_G,up_dn=-10,days=1)
PutIVDown_ATMIV.f.IVIDX.f_1D$model
load.ATMIV.f.IVIDX.f(optype=OpType_Call_G,up_dn=10,days=1) 
CallIVUp_ATMIV.f.IVIDX.f_1D
load.ATMIV.f.IVIDX.f(optype=OpType_Call_G,up_dn=(-10),days=1)
CallIVDown_ATMIV.f.IVIDX.f_1D
#ATMIDXIV.f
load.ATMIDXIV.f(OpType_Put_G)
load.ATMIDXIV.f(OpType_Call_G)
#VCone
load.VCone(optype=OpType_Put_G)
PutVCone
load.VCone(optype=OpType_Call_G)
CallVCone
#Obsolete
#load.IVChg(OpType_Put_G,10)
#PutIVChgUp
#load.IVChg(OpType_Put_G,-10)
#PutIVChgDown
#load.IVChg(OpType_Call_G,10)
#CallIVChgUp
#load.IVChg(OpType_Call_G,-10)
#CallIVChgDown