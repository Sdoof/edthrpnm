library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
library(pracma)

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")
###Global 変数及び定数.
#Calendar
CALENDAR_G=ConfigParameters["CALENDAR_G",1]

# Possibly read from File
riskFreeRate_G=as.numeric(ConfigParameters["riskFreeRate_G",1])
divYld_G=as.numeric(ConfigParameters["divYld_G",1])

#Definition
OpType_Put_G=as.numeric(ConfigParameters["OpType_Put_G",1])
OpType_Call_G=as.numeric(ConfigParameters["OpType_Call_G",1])

#Skewness Calculation
TimeToExp_Limit_Closeness_G=as.numeric(ConfigParameters["TimeToExp_Limit_Closeness_G",1])
#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

#Holding Period
#holdDays<-3*252/365 #Trading Days. This should be correct.
holdDays=as.numeric(ConfigParameters["holdDays",1])
#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays=as.numeric(ConfigParameters["dviv_caldays",1])
#Multipler of Position
PosMultip=as.numeric(ConfigParameters["PosMultip",1])

#EvalFuncSetting
EvalFuncSetting<-list(as.numeric(ConfigParameters["EvalFnc_UdlStepNum",1]))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_UdlStepPct",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_Maxposnum",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_Tail_rate",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_LossLimitPrice",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_HV_IV_Adjust_Ratio",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_SigmoidA_Profit",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_SigmoidA_AllEffect",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(ifelse(as.numeric(ConfigParameters["EvalFnc_ThetaEffectPositive",1])==1,TRUE,FALSE)))
names(EvalFuncSetting)<-c("UdlStepNum","UdlStepPct","Maxposnum","Tail_rate","LossLimitPrice",
                          "HV_IV_Adjust_Ratio","SigmoidA_Profit","SigmoidA_AllEffect","ThetaEffectPositive")

#Option Chain and Position Data. Here we use UDL_Positions_Pre ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",")
#filtering. deleting unnecessary column
opchain %>% dplyr::select(-(contains('Frac',ignore.case=TRUE)),
                          -(IV)) %>% as.data.frame() -> opchain
#only OOM targeted
#opchain %>% dplyr::filter(HowfarOOM>=0) -> opchain
#assiging initial Position?
#get position where opchain$Position!=0
opchain %>% dplyr::filter(Position!=0) -> position

##Historical Implied Volatility Data ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",nrows=1000);rm(rf)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(opchain$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

#Initial and evaluation vector -
iniPos<-opchain$Position
iniPos<-rep(0,length(iniPos))
evaPos<-opchain$Position

#test sample value -----------
#best_result<-1.0
#obj_Income_sgmd(x=evaPos,isDebug=TRUE,isDetail=TRUE,isFileout=FALSE)

#Data Setup. Provisioning
#Load Regression and Correlation Parameters ---------------
load.PC2IV(PC="PC3dCtC",IVC="IVCF3dCtC")
PC3dCtC_IVCF3dCtC
load.PC2IV(PC="PC5dCtC",IVC="IVCF5dCtC")
PC5dCtC_IVCF5dCtC
load.PC2IV(PC="PC7dCtC",IVC="IVCF7dCtC")
PC7dCtC_IVCF7dCtC
load.PC2IV(PC="PC1dCtC",IVC="IVCF1dCtC")
PC1dCtC_IVCF1dCtC
#load.PC2IV(PC="PC1dCtO",IVC="IVCF1dCtO")
#PC1dCtO_IVCF1dCtO
load.Skew()
SkewModel
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

#sigmoid function  ------
for(tmp in 1:8){
  create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=6,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-08P6C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=5,calln=3,ml=2,
                                          fname=paste(".\\ResultData\\inipop-08P5C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=4,calln=4,ml=2,
                                          fname=paste(".\\ResultData\\inipop-08P4C4-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=5,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-07P5C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=4,calln=3,ml=2,
                                          fname=paste(".\\ResultData\\inipop-07P4C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=2000,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=4,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-06P4C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=1500,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=3,calln=3,ml=2,
                                          fname=paste(".\\ResultData\\inipop-06P3C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=1000,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=6,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-06P6C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=2000,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=3,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-05P3C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=1500,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=5,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-05P5C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=1000,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=4,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-04P4C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=800,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=2,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-04P2C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=500,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=3,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-03P3C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=2,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-02P2C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=2.0,putn=0,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-02P0C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
};rm(tmp)

#1Cb.csv
st <- "powershell.exe .\\shell\\cmd1.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd2.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\1Cb-.csv \" "
system(st) ;rm(st)

#creating candidate pool for combined search --------

# tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-1000-08P6C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
#                                 pnum=0,nrows=-1,skip=0,method=1)
# tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\1Cb.csv",sep=""),
#                                 pnum=0,nrows=-1,skip=0,method=1)
tmp<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
tmp %>% dplyr::arrange(tmp[,(length(iniPos)+1)]) %>% dplyr::distinct() -> tmp
#tmp %>% filter(.[,length(iniPos)+1]<1.15) -> tmp
tmp %>% arrange(.[,length(iniPos)+1]) %>% head(3000) -> tmp
pools<-list(list(c(1,0,0),tmp)) #No.[[1]]

# or when all results are mixed together regardress of the number of Putn and Calln, pools[[1]] should be set as
# c(1,0,0) <- c(1Cb{=exact}, Putn not spicified, Calln not spicified)

rm(tmp)

#combined population serach --------------

### 2(exact x exact) Combinations (2Cb)
#
create_combined_population(popnum=30000,EvalFuncSetting,thresh=1.7,plelem=c(1,1),fname=paste(".\\ResultData\\combine-Result-1Cb+1Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)
#2Cb.csv
st <- "powershell.exe .\\shell\\cmd3.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd4.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\2Cb-.csv \" "
system(st) ;rm(st)

### 3(exact x exact x exact) Combinations (3Cb)

#adjust combined candidate population considering combinational explostion
# tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\1Cb.csv",sep=""),
#                                 pnum=0,nrows=-1,skip=0,method=1)
tmp<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
tmp %>% dplyr::arrange(tmp[,(length(iniPos)+1)]) %>% dplyr::distinct() -> tmp
#tmp %>% filter(.[,length(iniPos)+1]<1.08) -> tmp
tmp %>% arrange(.[,length(iniPos)+1]) %>% head(600) -> tmp

pools<-list(list(c(1,0,0),tmp)) #No.[[1]] again

create_combined_population(popnum=10000,EvalFuncSetting,thresh=1.7,plelem=c(1,1,1),fname=paste(".\\ResultData\\combine-Result-1Cb+1Cb+1Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)
#3Cb.csv
st <- "powershell.exe .\\shell\\cmd5.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd6.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\3Cb-.csv \" "
system(st) ;rm(st)

### 2x2(2Cbx2Cb) and 3x3(3Cbx3cb) Combinations
###create nested combine candidate pool 
# poolidx<-length(pools)+1
# 
# tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\2Cb.csv",sep=""),
#                                             pnum=0,nrows=-1,skip=0,method=1)
# tmp %>% filter(.[,length(iniPos)+1]<1.2) -> tmp
# tmp %>% arrange(.[,length(iniPos)+1]) %>% head(3000) -> tmp

###c(2Cb, Putn not spicified, Calln not spicified)
# pools[poolidx]<-list(list(c(2,0,0),tmp)) ; poolidx<-poolidx+1 #No.[[4]] or No.[[2]]
# 
# 
# tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\3Cb.csv",sep=""),
#                                 pnum=0,nrows=-1,skip=0,method=1)
# tmp %>% filter(.[,length(iniPos)+1]<1.2) -> tmp
# tmp %>% arrange(.[,length(iniPos)+1]) %>% head(3000) -> tmp
###c(3Cb, Putn not spicified, Calln not spicified)
# pools[poolidx]<-list(list(c(3,0,0),tmp)) ; poolidx<-poolidx+1 #No.[[5]] or No.[[3]]
# rm(poolidx,tmp)

###Creating Population again
### 2Cbx2cb Combination search
# create_combined_population(popnum=20000,thresh=2.0,plelem=c(2,2),fname=paste(".\\ResultData\\combine-Result-2Cb(+1Cb+1Cb)2Cb(+1Cb+1Cb)-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
#                            isFileout=TRUE,isDebug=FALSE,maxposn=8) 
###4Cb.csv
# st <- "powershell.exe .\\shell\\cmd7.ps1"
# system(st)
# st <- "powershell.exe .\\shell\\cmd8.ps1"
# system(st)
# st <- "powershell.exe -Command \" del .\\ResultData\\4Cb-.csv \" "
# system(st) ;rm(st)

### 3Cbx3Cb Combination search
# create_combined_population(popnum=20000,thresh=2.0,plelem=c(3,3),fname=paste(".\\ResultData\\combine-Result-3Cb(+1Cb+1Cb+1Cb)3Cb(+1Cb+1Cb+1Cb)-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
#                            isFileout=TRUE,isDebug=FALSE,maxposn=8) 
###6Cb.csv
# st <- "powershell.exe .\\shell\\cmd9.ps1"
# system(st)
# st <- "powershell.exe .\\shell\\cmd10.ps1"
# system(st)
# st <- "powershell.exe -Command \" del .\\ResultData\\6Cb-.csv \" "
# system(st) ;rm(st)

rm(pools)

#Cleaning
rm(iniPos,evaPos)
rm(best_result,holdDays,dviv_caldays,PosMultip)
rm(opchain,histIV,position)
rm(CallIVChgDown,CallIVChgUp,CallVCone,PutIVChgDown,PutIVChgUp,PutVCone,SkewModel)
rm(PC1dCtC_IVCF1dCtC,PC3dCtC_IVCF3dCtC,PC5dCtC_IVCF5dCtC,PC7dCtC_IVCF7dCtC)
rm(ConfigFileName_G,ConfigParameters,EvalFuncSetting)
rm(CALENDAR_G,OpType_Call_G,OpType_Put_G,ResultFiles_Path_G,TimeToExp_Limit_Closeness_G)
rm(DataFiles_Path_G,Underying_Symbol_G,divYld_G,riskFreeRate_G)
