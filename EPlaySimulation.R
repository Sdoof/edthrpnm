library(RQuantLib)
library(ggplot2)
library(dplyr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")
#Calendar
CALENDAR_G=ConfigParameters["CALENDAR_G",1]

# Possibly read from File
riskFreeRate_G=as.numeric(ConfigParameters["riskFreeRate_G",1])
divYld_G=as.numeric(ConfigParameters["divYld_G",1])

#Definition
OpType_Put_G=as.numeric(ConfigParameters["OpType_Put_G",1])
OpType_Call_G=as.numeric(ConfigParameters["OpType_Call_G",1])

#IV deviation
Sim_IV_DEVIATION=as.numeric(ConfigParameters["Sim_IV_DEVIATION",1])

#Skewness Calculation
TimeToExp_Limit_Closeness_G=as.numeric(ConfigParameters["TimeToExp_Limit_Closeness_G",1])

#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

##
#  ERskRtnEval -------------
#holdDays Trading Days. GreeksEffect are calculated based on this holding days.
holdDays=as.numeric(ConfigParameters["holdDays",1])

#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays=as.numeric(ConfigParameters["dviv_caldays",1])

#Multipler of Position
PosMultip=as.numeric(ConfigParameters["PosMultip",1])

#Evaluatin Table Position start
evalPosStart=as.numeric(ConfigParameters["SimEvalPosStart",1])

#Evaluatin Table Position end
evalPosEnd=as.numeric(ConfigParameters["SimEvalPosEnd",1])

#Option Chain and Position Data. Here we use UDL_Positions_Pre ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",")

##Historical Implied Volatility Data
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",nrows=1000);rm(rf)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(opchain$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

##Spreads to be evaluated loaded
rf<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep="")
evalPositions<-read.table(rf,header=F,sep=",",colClasses="numeric")

if(length(evalPositions)>length(opchain$Position)){
  evalPositions %>% distinct() -> evalPositions 
};rm(rf)

# Top n Spreads
evalPositions %>% slice(evalPosStart:evalPosEnd) -> evalPositions

# Num of each Stimulation
StimultaionNum=as.numeric(ConfigParameters["SimultaionNum",1])

#Max duration (days) of the Stimulation
MaxStimDay=as.numeric(ConfigParameters["MaxSimDay",1])

#create pertubation combinations of these parameters
# Parameter Tables--
# mu_udly sigma_udly mu_iv sigma_iv weight
#    →(Ntrl)  →(Ntrl)  →     auto     w1
#                      ↑     auto     w2
#                      ↓     auto     w3
#    →        ↓        →     auto     w4
#                      ↑     auto     w5
#                      ↓     auto     w6
#    ↓        ↑        →     auto     w7
#                      ↑     auto     w8
#                      ↓     auto     w9
#    ↑        →        →     auto     w10
#                      ↑     auto     w11
#                      ↓     auto     w12
#
# mu_udly(Ntrl)     : (1.0)^(1/252)-1
# sigma_udly(Ntrl)  : histIV[1,]$IVIDX/sqrt(252)*0.95 0.95:HVとIVの調整項目。
# mu_iv(Ntrl)       : (1.0)^(1/252)-1
# sigma_iv(auto cal): VoV*sqrt(1-Co(UDLY,IV)^2)
# 
# w1-w12の重みを対象となるUDLYやマーケットの見通しに基づき定める
#
# wx(!=0)の重みを与えられたシナリオについてstimulationを行い、weightで重み付けられた結果
# を最終評価値とする
mu_udly_drift_up=as.numeric(ConfigParameters["Sim_Mu_udly_drift_up",1])
mu_udly_drift_down=as.numeric(ConfigParameters["Sim_Mu_udly_drift_down",1])
sigma_udly_drift_up=as.numeric(ConfigParameters["Sim_Sigma_udly_drift_up",1])
sigma_udly_drift_down=as.numeric(ConfigParameters["Sim_Sigma_udly_drift_down",1])
HV_IV_Adjust_Ratio=as.numeric(ConfigParameters["Sim_HV_IV_Adjust_Ratio",1])
mu_iv_drift_up=as.numeric(ConfigParameters["Sim_Mu_iv_drift_up",1])
mu_iv_drift_down=as.numeric(ConfigParameters["Sim_Mu_iv_drift_down",1])

# scenario_weight<-c(c(1.0,0.8,0.8),c(0.6,0.4,0.6),c(0.7,1.0,0.2),c(0.4,0.4,0.4))
# scenario_weight<-scenario_weight/sum(scenario_weight)
scenario_weight<-eval(parse(text=gsub("-",",",ConfigParameters["SimScenarioWeight",1])))

#Result CSV file 
out_text_file<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_result.csv",sep="")

#Load Regression and Correlation Parameters
load.PC2IV(PC="PC3dCtC",IVC="IVCF3dCtC")
PC3dCtC_IVCF3dCtC
load.PC2IV(PC="PC5dCtC",IVC="IVCF5dCtC")
PC5dCtC_IVCF5dCtC
load.PC2IV(PC="PC7dCtC",IVC="IVCF7dCtC")
PC7dCtC_IVCF7dCtC
load.PC2IV(PC="PC1dCtC",IVC="IVCF1dCtC")
PC1dCtC_IVCF1dCtC
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

for(Counter in evalPosStart:evalPosEnd){
  evaPos<-evalPositions[Counter-evalPosStart+1,1:length(opchain$Position)]
  evaPos<-unlist(evaPos)
  print(evaPos)
  write(Counter,out_text_file,append=T)
  cat(evaPos,"\n",file=out_text_file,sep=",",append=TRUE)
  
  opchain$Position<-unlist(evaPos)
  opchain %>% dplyr::filter(Position!=0) -> position
 
  #API code chunk
  writeIbAPITicket(out_text_file,position,sep="$")
  
  ##
  # Creating Pertubation Combination Data Frame
  mu_udly<-c(rep(((1.0)^(1/252)-1),times=6),rep(((1.0-mu_udly_drift_down)^(1/252)-1),times=3),rep(((1.0+mu_udly_drift_up)^(1/252)-1),times=3))
  sigma_udly<-c(rep(histIV[1,]$IVIDX/sqrt(252)*HV_IV_Adjust_Ratio,times=3),
                rep(histIV[1,]$IVIDX/sqrt(252)*(1.0-sigma_udly_drift_down)*HV_IV_Adjust_Ratio,times=3),
                rep(histIV[1,]$IVIDX/sqrt(252)*(1.0+sigma_udly_drift_up)*HV_IV_Adjust_Ratio,times=3),
                rep(histIV[1,]$IVIDX/sqrt(252)*HV_IV_Adjust_Ratio,times=3))
  mu_iv<-c(rep(((1.0)^(1/252)-1),times=1),rep(((1.0+mu_iv_drift_up)^(1/252)-1),times=1),rep(((1.0-mu_iv_drift_down)^(1/252)-1),times=1))
  mu_iv<-rep(mu_iv,times=4)
  sigma_iv<-rep(annuual.daily.volatility(histIV$IVIDX)$anlzd*sqrt(1-PC1dCtC_IVCF1dCtC$cor*PC1dCtC_IVCF1dCtC$cor)/sqrt(252),
                times=12)
  weight<-scenario_weight
  
  modelScenario<-data.frame(mu_udly=mu_udly,sigma_udly=sigma_udly,mu_iv=mu_iv,sigma_iv=sigma_iv,weight=weight)
  rm(mu_udly,sigma_udly,mu_iv,sigma_iv,weight)
  
  modelScenario %>% rowwise() %>% do(stimrslt=Simulate(position=position,StimultaionNum=StimultaionNum,MaxStimDay=MaxStimDay,PosMultip=PosMultip,hdd=holdDays,
                                                        mu_udly=.$mu_udly,sigma_udly=.$sigma_udly,
                                                        mu_iv=.$mu_iv,sigma_iv=.$sigma_iv,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio,IV_DEVIATION=Sim_IV_DEVIATION)) -> modelStimRawlist
  
  modelStimRawlist %>% rowwise() %>% do(resdf=getStimResultDataFrame(.$stimrslt,StimultaionNum)) -> tmp
  modelScenario$resdf<-tmp$resdf ; rm(tmp)
  
  
  
  #profit statistic
  modelScenario %>% rowwise() %>% do(min_profit=min(.$resdf$profit)) -> tmp
  unlist(tmp)->modelScenario$min_profit;rm(tmp)
  
  modelScenario %>% rowwise() %>% do(min_profit=max(.$resdf$profit)) -> tmp
  unlist(tmp)->modelScenario$max_profit;rm(tmp)
  
  modelScenario %>% rowwise() %>% do(mean_profit=mean(.$resdf$profit)) -> tmp
  unlist(tmp)->modelScenario$mean_profit;rm(tmp)
  
  modelScenario %>% rowwise() %>% do(median_profit=median(.$resdf$profit)) -> tmp
  unlist(tmp)->modelScenario$median_profit;rm(tmp)
  
  modelScenario %>% rowwise() %>% do(profit_sd=sd(.$resdf$profit)) -> tmp
  unlist(tmp)->modelScenario$profit_sd;rm(tmp)
  
  fn<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_modelStimRawlist_",Counter,sep="")
  save(modelStimRawlist,file=fn)
  
  fn<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_modelScenario_",Counter,sep="")
  save(modelScenario,file=fn)
  rm(fn)
  
  print(position)
  
  write.table(position %>% select(Date,ExpDate,TYPE,Strike,Position,ContactName,UDLY,OrigIV,Delta),
              out_text_file,quote=T,row.names=F,append=T,sep=",")
  
  #write.table(opchain %>% select(Date,ExpDate,TYPE,Strike,Position,ContactName,UDLY,OrigIV,Delta),
  #            out_text_file,quote=T,row.names=F,append=T,sep=",")
  
  modelScenario %>% dplyr::select(-(resdf)) -> tmp
  print(tmp)
  write.table(tmp,out_text_file,quote=T,row.names=F,append=T,sep=",")
  write.table(data.frame(min_profit=min(tmp$min_profit),max_profit=max(tmp$max_profit),expected_profit=sum(tmp$weight*tmp$mean_profit)),
              out_text_file,quote=T,row.names=F,append=T,sep=",")
  #initialEvalScore
  modelStimRawlist$stimrslt[[1]][[1]]$IniEvalScore %>% mutate(AdvtgEffect=ThetaEffect+GammaEffect,DirectalEffect=DeltaEffect+VegaEffect) -> tmp
  write.table(tmp,out_text_file,quote=T,row.names=F,append=T,sep=",")
  rm(tmp)
}
rm(evaPos,Counter)
rm(modelScenario,modelStimRawlist)

#all cleanings.
rm(histIV,position,MaxStimDay,StimultaionNum,out_text_file)
rm(evalPosStart,evalPosEnd)
rm(mu_udly_drift_up,mu_udly_drift_down,sigma_udly_drift_up,sigma_udly_drift_down)
rm(HV_IV_Adjust_Ratio,Sim_IV_DEVIATION,mu_iv_drift_up,mu_iv_drift_down,scenario_weight)
rm(evalPositions,opchain)

#Parameters Cleaning
rm(CallIVChgDown,CallIVChgUp,CallVCone,PutIVChgDown,PutIVChgUp,PutVCone)
rm(PC1dCtC_IVCF1dCtC,PC3dCtC_IVCF3dCtC,PC5dCtC_IVCF5dCtC,PC7dCtC_IVCF7dCtC,SkewModel)
rm(ConfigFileName_G,ConfigParameters)
rm(riskFreeRate_G,divYld_G,OpType_Put_G,OpType_Call_G,TimeToExp_Limit_Closeness_G)
rm(CALENDAR_G,Underying_Symbol_G,DataFiles_Path_G,ResultFiles_Path_G,holdDays,dviv_caldays,PosMultip)

source('./EPlaybackAdjust.R',encoding = 'UTF-8')

