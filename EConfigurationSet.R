#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")
###Global 変数及び定数.

##Eeach Target File
TARGET_EXPDATE="2016/5/31"
TARGET_EXPDATE_FRONT="2016/6/16"
TARGET_EXPDATE_BACK="2016/7/14"

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

#Skewness Calculation
TimeToExp_Limit_Closeness_G=as.numeric(ConfigParameters["TimeToExp_Limit_Closeness_G",1])

#EvalFuncSetting
EvFNames <- c("holdDays","UdlStepNum","UdlStepPct","Posnum","Tail_rate","LossLimitPrice",
              "HV_IV_Adjust_Ratio","Weight_Drift","Delta_Thresh_Minus","Delta_Thresh_Plus","Vega_Thresh_Minus","Vega_Thresh_Plus",
              "Delta_Direct_Prf","Vega_Direct_Prf","Delta_Neutral_Offset","Vega_Neutral_Offset",
              "Profit_Coef","AdvEffect_Coef","AllEffect_Coef","DrctlEffect_Coef","MaxLoss_Coef",
              "SigmoidA_Numerator","SigmoidA_Denominator","ThetaEffectPositive","EvalConvex","DeltaHedge","GreekEfctOnHldD")
EvalFuncSetting<-vector("list",length(EvFNames))

EvalFuncSetting[[1]]<-as.numeric(ConfigParameters["holdDays",1])
EvalFuncSetting[[2]]<-as.numeric(ConfigParameters["EvalFnc_UdlStepNum",1])
EvalFuncSetting[[3]]<-as.numeric(ConfigParameters["EvalFnc_UdlStepPct",1])
EvalFuncSetting[[4]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Posnum",1])))
EvalFuncSetting[[5]]<-as.numeric(ConfigParameters["EvalFnc_Tail_rate",1]) 
EvalFuncSetting[[6]]<-as.numeric(ConfigParameters["EvalFnc_LossLimitPrice",1]) 
EvalFuncSetting[[7]]<-as.numeric(ConfigParameters["EvalFnc_HV_IV_Adjust_Ratio",1])
EvalFuncSetting[[8]]<-as.numeric(ConfigParameters["EvalFnc_Weight_Drift",1])
EvalFuncSetting[[9]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Thresh_Minus",1])))
EvalFuncSetting[[10]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Thresh_Plus",1])))
EvalFuncSetting[[11]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Thresh_Minus",1])))
EvalFuncSetting[[12]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Thresh_Plus",1])))
EvalFuncSetting[[13]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Direct_Prf",1])))
EvalFuncSetting[[14]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Direct_Prf",1])))
EvalFuncSetting[[15]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Neutral_Offset",1])))
EvalFuncSetting[[16]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Neutral_Offset",1])))
EvalFuncSetting[[17]]<-as.numeric(ConfigParameters["EvalFnc_Profit_Coef",1])
EvalFuncSetting[[18]]<-as.numeric(ConfigParameters["EvalFnc_AdvEffect_Coef",1])
EvalFuncSetting[[19]]<-as.numeric(ConfigParameters["EvalFnc_AllEffect_Coef",1]) 
EvalFuncSetting[[20]]<-as.numeric(ConfigParameters["EvalFnc_DrctlEffect_Coef",1])
EvalFuncSetting[[21]]<-as.numeric(ConfigParameters["EvalFnc_MaxLoss_Coef",1])
EvalFuncSetting[[22]]<-as.numeric(ConfigParameters["EvalFnc_SigmoidA_Numerator",1])
EvalFuncSetting[[23]]<-as.numeric(ConfigParameters["EvalFnc_SigmoidA_Denominator",1])
EvalFuncSetting[[24]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_ThetaEffectPositive",1])==1,TRUE,FALSE)
EvalFuncSetting[[25]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_EvalConvex",1])==1,TRUE,FALSE)
EvalFuncSetting[[26]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_DeltaHedgeToEvalProfit",1])==1,TRUE,FALSE)
EvalFuncSetting[[27]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_GreekEffectEvalOnHoldDay",1])==1,TRUE,FALSE)

names(EvalFuncSetting)<-EvFNames
rm(EvFNames)

#MuLtipler for creating initial populaitons
Optimize_ml=as.numeric(ConfigParameters["Optimize_ml",1])

#Parameters for Combinational Optimization
InitialPopCreateLoopNum<-as.numeric(ConfigParameters["Optimize_InitialPopCreateLoopNum",1])
InitialPopThresh=as.numeric(ConfigParameters["Optimize_InitialPopThresh",1])
TopN_1=as.numeric(ConfigParameters["Optimize_TopN_1",1])
PopN_1=as.numeric(ConfigParameters["Optimize_PopN_1",1])
Thresh_1=as.numeric(ConfigParameters["Optimize_Thresh_1",1])
TopN_2=as.numeric(ConfigParameters["Optimize_TopN_2",1])
PopN_2=as.numeric(ConfigParameters["Optimize_PopN_2",1])
Thresh_2=as.numeric(ConfigParameters["Optimize_Thresh_2",1])

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
load.Skew()
SkewModel
load.Skew("_Put")
SkewModel_Put
load.Skew("_Call")
SkewModel_Call
load.VCone(optype=OpType_Put_G)
PutVCone
load.VCone(optype=OpType_Call_G)
CallVCone
load.IVChg(OpType_Put_G,10)
PutIVChgUp
load.IVChg(OpType_Put_G,-10)
PutIVChgDown
load.IVChg(OpType_Call_G,10)
CallIVChgUp
load.IVChg(OpType_Call_G,-10)
CallIVChgDown

#creating initial population
