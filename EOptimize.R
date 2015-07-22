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
EvalFuncSetting<-list(holdDays)
EvalFuncSetting<-c(EvalFuncSetting,as.numeric(ConfigParameters["EvalFnc_UdlStepNum",1]))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_UdlStepPct",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_Maxposnum",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_Tail_rate",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_LossLimitPrice",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_HV_IV_Adjust_Ratio",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_Delta_Thresh_Minus",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_Delta_Thresh_Plus",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_Profit_Coef",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_AllEffect_Coef",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_AdvEffect_Coef",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_DrctlEffect_Coef",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_SigmoidA_Numerator",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(as.numeric(ConfigParameters["EvalFnc_SigmoidA_Denominator",1])))
EvalFuncSetting<-c(EvalFuncSetting,list(ifelse(as.numeric(ConfigParameters["EvalFnc_ThetaEffectPositive",1])==1,TRUE,FALSE)))

names(EvalFuncSetting)<-c("holdDays","UdlStepNum","UdlStepPct","Maxposnum","Tail_rate","LossLimitPrice","HV_IV_Adjust_Ratio",
                          "Delta_Thresh_Minus","Delta_Thresh_Plus",
                          "Profit_Coef","AllEffect_Coef","AdvEffect_Coef","DrctlEffect_Coef","SigmoidA_Numerator","SigmoidA_Denominator",
                          "ThetaEffectPositive")

#Parameters for Combinational Optimization
InitialPopCreateLoopNum<-as.numeric(ConfigParameters["Optimize_InitialPopCreateLoopNum",1])
InitialPopThresh=as.numeric(ConfigParameters["Optimize_InitialPopThresh",1])
TopN_1=as.numeric(ConfigParameters["Optimize_TopN_1",1])
PopN_1=as.numeric(ConfigParameters["Optimize_PopN_1",1])
Thresh_1=as.numeric(ConfigParameters["Optimize_Thresh_1",1])
TopN_2=as.numeric(ConfigParameters["Optimize_TopN_2",1])
PopN_2=as.numeric(ConfigParameters["Optimize_PopN_2",1])
Thresh_2=as.numeric(ConfigParameters["Optimize_Thresh_2",1])

#Option Chain and Position Data. Here we use UDL_Positions_Pre
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",")
#filtering. deleting unnecessary column
opchain %>% dplyr::select(-(contains('Frac',ignore.case=TRUE)),
                          -(IV)) %>% as.data.frame() -> opchain
#get position where opchain$Position!=0
opchain %>% dplyr::filter(Position!=0) -> position

##Historical Implied Volatility Data ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",nrows=1000);rm(rf)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(opchain$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

#Initial and evaluation vector
iniPos<-opchain$Position
iniPos<-rep(0,length(iniPos))
evaPos<-opchain$Position

#Data Setup. Provisioning
#Load Regression and Correlation Parameters
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
for(tmp in 1:InitialPopCreateLoopNum){
  # create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=6,calln=2,ml=1,
  #                                           fname=paste(".\\ResultData\\inipop-08P6C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
  #                                           isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  #create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=5,calln=3,ml=1,
  #                                        fname=paste(".\\ResultData\\inipop-08P5C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
  #                                        isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  # create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=4,calln=4,ml=1,
  #                                         fname=paste(".\\ResultData\\inipop-08P4C4-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
  #                                         isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  # create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=5,calln=2,ml=1,
  #                                         fname=paste(".\\ResultData\\inipop-07P5C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
  #                                         isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  # create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=4,calln=3,ml=1,
  #                                         fname=paste(".\\ResultData\\inipop-07P4C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
  #                                         isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=2000,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=4,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-06P4C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=1500,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=3,calln=3,ml=2,
                                          fname=paste(".\\ResultData\\inipop-06P3C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=1000,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=2,calln=4,ml=2,
                                          fname=paste(".\\ResultData\\inipop-06P2C4-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=2000,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=3,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-05P3C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=1500,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=2,calln=3,ml=2,
                                          fname=paste(".\\ResultData\\inipop-05P2C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=800,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=4,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-04P4C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=800,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=2,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-04P2C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=500,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=3,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-03P3C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=300,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=0,calln=3,ml=2,
                                          fname=paste(".\\ResultData\\inipop-03P0C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=2,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-02P2C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
  create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,putn=0,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-02P0C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),PosMultip,
                                          isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
};rm(tmp)

#1Cb.csv
st <- "powershell.exe .\\shell\\cmd1.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd2.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\1Cb-.csv \" "
system(st) ;rm(st)

#creating candidate pool for combined search
 tmp<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
 tmp %>% dplyr::arrange(tmp[,(length(iniPos)+1)]) %>% dplyr::distinct() -> tmp
 tmp %>% arrange(.[,length(iniPos)+1]) %>% head(TopN_1) -> tmp
 pools<-list(list(c(1,0,0),tmp)) #No.[[1]]

# or when all results are mixed together regardress of the number of Putn and Calln, pools[[1]] should be set as
# c(1,0,0) <- c(1Cb{=exact}, Putn not spicified, Calln not spicified)
 rm(tmp)

#combined population serach

### 2(exact x exact) Combinations (2Cb)
#
 create_combined_population(popnum=PopN_1,EvalFuncSetting,thresh=Thresh_1,plelem=c(1,1),fname=paste(".\\ResultData\\combine-Result-1Cb+1Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                            isFileout=TRUE,isDebug=FALSE,maxposn=EvalFuncSetting$Maxposnum,PosMultip=PosMultip)

#2Cb.csv
 st <- "powershell.exe .\\shell\\cmd3.ps1"
 system(st)
 st <- "powershell.exe .\\shell\\cmd4.ps1"
 system(st)
 st <- "powershell.exe -Command \" del .\\ResultData\\2Cb-.csv \" "
 system(st) ;rm(st)

### 3(exact x exact x exact) Combinations (3Cb)

#adjust combined candidate population considering combinational explostion
 tmp<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
 tmp %>% dplyr::arrange(tmp[,(length(iniPos)+1)]) %>% dplyr::distinct() -> tmp
 tmp %>% arrange(.[,length(iniPos)+1]) %>% head(TopN_2) -> tmp
 
 pools<-list(list(c(1,0,0),tmp)) #No.[[1]] again
 rm(tmp)
 
 create_combined_population(popnum=PopN_2,EvalFuncSetting,thresh=Thresh_2,plelem=c(1,1,1),fname=paste(".\\ResultData\\combine-Result-1Cb+1Cb+1Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                            isFileout=TRUE,isDebug=FALSE,maxposn=EvalFuncSetting$Maxposnum,PosMultip=PosMultip)
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

#rm(pools)


###
##
# Result Post Proceccing

#HV_IV_Adjust_Ratio
HV_IV_Adjust_Ratio=as.numeric(ConfigParameters["EvalFnc_HV_IV_Adjust_Ratio",1])

#Threshhold 足切りライン
Thresh_Score1=as.numeric(ConfigParameters["ResultProcess_Thresh_Score1",1])
Thresh_Score2=as.numeric(ConfigParameters["ResultProcess_Thresh_Score2",1])
Thresh_Score3=as.numeric(ConfigParameters["ResultProcess_Thresh_Score3",1])

#ophcain 
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",",stringsAsFactors=FALSE)
#filtering. deleting unnecessary column
opchain %>% dplyr::select(-(contains('Frac',ignore.case=TRUE)),
                          -(IV)) %>% as.data.frame() -> opchain
#only OOM targeted
#opchain %>% dplyr::filter(HowfarOOM>=0) -> opchain
rm(rf)
#iniPos
iniPos<-opchain$Position

#create put and call pos num of the specified spread
getPutCallnOfthePosition<-function(x){
  type<-opchain$TYPE
  putpos<-as.numeric(type==OpType_Put_G)
  putn<-sum( as.numeric((putpos*x)!=0) )
  callpos<-as.numeric(type==OpType_Call_G)
  calln<-sum( as.numeric((callpos*x)!=0) )
  return (c(putn,calln))
}

##
# Exact (1Cb)
#res1<-createCombineCandidatePool(fname=paste(ResultFiles_Path_G,"1Cb.csv",sep=""),
#                                pnum=0,nrows=-1,skip=0,method=1)
res1<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
res1 %>% dplyr::arrange(res1[,(length(iniPos)+1)]) %>% dplyr::distinct() -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<Thresh_Score1) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
res1 -> total_res

##
# 2Cb
res1<-read.table(paste(ResultFiles_Path_G,"2Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
res1 %>% dplyr::arrange(res1[,(length(iniPos)+1)]) %>% dplyr::distinct() -> res1
res1 %>% select(0:length(iniPos)+1) -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<Thresh_1) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1]) %>% distinct() -> total_res
rm(res1)

##
#  3Cb
res1<-read.table(paste(ResultFiles_Path_G,"3Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
res1 %>% dplyr::arrange(res1[,(length(iniPos)+1)]) %>% dplyr::distinct() -> res1
res1 %>% select(0:length(iniPos)+1) -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<Thresh_2) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1]) %>% distinct() -> total_res
rm(res1)

# res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
# tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
# res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
#factorと認識されたときの変換 #res1$V1<-as.numeric(as.character(res1$V1))
#full join
# full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

##Historical Implied Volatility Data
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",nrows=1000);rm(rf)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(opchain$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

# Writing to files based on option legs total number
total_res %>% mutate(posn=(putn+calln)) -> total_res
total_res %>%  filter(posn==6) %>% filter(.[,length(iniPos)+1]<2.0) ->tmp_fil 
total_res %>%  filter(posn==5|posn==4) %>% filter(.[,length(iniPos)+1]<2.0) ->tmp_fil2
total_res %>%  filter(posn<=3) %>% filter(.[,length(iniPos)+1]<2.0) ->tmp_fil3

## Advantageous Effect

#create put and call pos num of the specified spread
getPositionWithGreeks<-function(tmp_fil){
  tmp_fil[,1:length(iniPos)] %>% rowwise() %>% 
    do(theGreks=getPositionGreeks(hollowNonZeroPosition(unlist(.)),multi=PosMultip,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)) -> tmp
  tmp_fil$theGreks<-tmp$theGreks
  tmp_fil %>% rowwise() %>% do(Delta=.$theGreks$Delta,DeltaEffect=.$theGreks$DeltaEffect,VegaEffect=.$theGreks$VegaEffect,
                               ThetaEffect=.$theGreks$ThetaEffect,GammaEffect=.$theGreks$GammaEffect) -> tmp2
  tmp_fil$Delta<-unlist(tmp2$Delta)
  tmp_fil$DeltaEffect<-unlist(tmp2$DeltaEffect)
  tmp_fil$ThetaEffect<-unlist(tmp2$ThetaEffect)
  tmp_fil$GammaEffect<-unlist(tmp2$GammaEffect)
  tmp_fil$VegaEffect<-unlist(tmp2$VegaEffect)
  tmp_fil$theGreks<-NULL
  tmp_fil$AdvEffect<-unlist(tmp2$ThetaEffect)+unlist(tmp2$GammaEffect)
  
  return(tmp_fil)
}

getPositionWithGreeks(tmp_fil) %>% arrange(desc(AdvEffect)) -> tmp_fil
getPositionWithGreeks(tmp_fil2) %>% arrange(desc(AdvEffect)) -> tmp_fil2
getPositionWithGreeks(tmp_fil3) %>% arrange(desc(AdvEffect)) -> tmp_fil3

##
#  Filtering

# Thrshhold

#read again and again
ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")
Thresh_AdvEffect=as.numeric(ConfigParameters["ResultProcess_Thresh_AdvEffect",1])
F_Thrsh_Params_Names<-c("Score1","Score2","Score3","Delta1_Minus","Delta2_Minus","Delta3_Minus",
                        "Delta1_Plus","Delta2_Plus","Delta3_Plus","Thrsh_VegaE1","Thrsh_VegaE2","Thrsh_VegaE3",
                        "F_TopN1","F_TopN2","F_TopN3")
F_Thrsh_Params<- vector("list",length(F_Thrsh_Params_Names))
F_Thrsh_Params[[1]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Score1",1])
F_Thrsh_Params[[2]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Score2",1])
F_Thrsh_Params[[3]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Score3",1])
F_Thrsh_Params[[4]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta1_Minus",1])
F_Thrsh_Params[[5]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta2_Minus",1])
F_Thrsh_Params[[6]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta3_Minus",1])
F_Thrsh_Params[[7]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta1_Plus",1])
F_Thrsh_Params[[8]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta2_Plus",1])
F_Thrsh_Params[[9]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta3_Plus",1])
F_Thrsh_Params[[10]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_VegaE1",1])
F_Thrsh_Params[[11]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_VegaE2",1])
F_Thrsh_Params[[12]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_VegaE3",1])
F_Thrsh_Params[[13]]<-as.numeric(ConfigParameters["ResultProcess_F_TopN1",1])
F_Thrsh_Params[[14]]<-as.numeric(ConfigParameters["ResultProcess_F_TopN2",1])
F_Thrsh_Params[[15]]<-as.numeric(ConfigParameters["ResultProcess_F_TopN3",1])
names(F_Thrsh_Params)<-F_Thrsh_Params_Names ; rm(F_Thrsh_Params_Names) 
(Thresh_AdvEffect)
(F_Thrsh_Params)

tmp_fil %>% 
  #arrange(desc(AdvEffect)) %>% filter(.[,length(iniPos)+1]<F_Thrsh_Params$Score1) %>% 
  arrange(.[,length(iniPos)+1]) %>% filter(AdvEffect>Thresh_AdvEffect) %>% 
  filter(Delta>F_Thrsh_Params$Delta1_Minus,Delta<F_Thrsh_Params$Delta1_Plus) %>% 
  filter(VegaEffect>F_Thrsh_Params$Thrsh_VegaE1) %>% top_n(F_Thrsh_Params$F_TopN1) -> tmp_fil_w ; print(tmp_fil_w)

tmp_fil2 %>% 
  #arrange(desc(AdvEffect)) %>%  filter(.[,length(iniPos)+1]<F_Thrsh_Params$Score2) %>% 
  arrange(.[,length(iniPos)+1]) %>% filter(AdvEffect>Thresh_AdvEffect) %>% 
  filter(Delta>F_Thrsh_Params$Delta2_Minus) %>% filter(Delta<F_Thrsh_Params$Delta2_Plus) %>%
  filter(VegaEffect>F_Thrsh_Params$Thrsh_VegaE2) %>% top_n(F_Thrsh_Params$F_TopN2) -> tmp_fil_w2 ; print(tmp_fil_w2)

tmp_fil3 %>% 
  #arrange(desc(AdvEffect)) %>%  filter(.[,length(iniPos)+1]<F_Thrsh_Params$Score3) %>% 
  arrange(.[,length(iniPos)+1]) %>% filter(AdvEffect>Thresh_AdvEffect) %>% 
  filter(Delta>F_Thrsh_Params$Delta3_Minus) %>% filter(Delta<F_Thrsh_Params$Delta3_Plus) %>%
  filter(VegaEffect>F_Thrsh_Params$Thrsh_VegaE3) %>% top_n(F_Thrsh_Params$F_TopN3) -> tmp_fil_w3 ; print(tmp_fil_w3)

## Save to a file
write.table(tmp_fil_w,paste(ResultFiles_Path_G,"EvalCnd.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)
write.table(tmp_fil_w2,paste(ResultFiles_Path_G,"EvalCnd2.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)
write.table(tmp_fil_w3,paste(ResultFiles_Path_G,"EvalCnd3.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)
rm(tmp_fil_w,tmp_fil_w2,tmp_fil_w3)

rm(getPutCallnOfthePosition,getPositionWithGreeks)

##
# finally remove these variables
rm(iniPos,evaPos)
rm(holdDays,dviv_caldays,divYld_G,riskFreeRate_G,PosMultip)
rm(opchain,histIV,position)
rm(InitialPopCreateLoopNum,InitialPopThresh,TopN_1,PopN_1,Thresh_1,TopN_2,PopN_2,Thresh_2)
rm(CallIVChgDown,CallIVChgUp,CallVCone,PutIVChgDown,PutIVChgUp,PutVCone,SkewModel,F_Thrsh_Params)
rm(PC1dCtC_IVCF1dCtC,PC3dCtC_IVCF3dCtC,PC5dCtC_IVCF5dCtC,PC7dCtC_IVCF7dCtC)
rm(ConfigFileName_G,ConfigParameters,EvalFuncSetting,)
rm(CALENDAR_G,OpType_Call_G,OpType_Put_G,DataFiles_Path_G,ResultFiles_Path_G,TimeToExp_Limit_Closeness_G,Underying_Symbol_G)

rm(tmp_fil,tmp_fil2,tmp_fil3,total_res)
rm(HV_IV_Adjust_Ratio,Thresh_Score1,Thresh_Score2,Thresh_Score3,Thresh_AdvEffect)


