library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)

##
#  EOptionOprEurop -------------

###Global 変数及び定数.
# Possibly read from File
riskFreeRate_G=0.01
divYld_G=0.0

#Definition
OpType_Put_G=1
OpType_Call_G=-1
#Skewness Calculation
TimeToExp_Limit_Closeness_G=0.3
#File
Underying_Symbol_G="RUT"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"
ResultFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\ResultData\\"

##
#  ERskRtnEval -------------
#holdDays Trading Days. This should be correct.
holdDays<-3

#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays<-20

#Multipler of Position
PosMultip<-100

#Option Chain and Position Data. Here we use UDL_Positions_Pre ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",",stringsAsFactors=FALSE)
#filtering. deleting unnecessary column
opchain %>% dplyr::select(-(contains('Frac',ignore.case=TRUE)),
                          -(IV)) %>% as.data.frame() -> opchain
#only OOM targeted
opchain %>% dplyr::filter(HowfarOOM>=0) -> opchain


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

##Historical Implied Volatility Data ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",stringsAsFactors=FALSE,nrows=1999);rm(rf)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(position$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

##Spreads to be evaluated loaded
rf<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep="")
evalPositions<-read.table(rf,header=F,sep=",")
length(opchain$Position)
if(length(evalPositions)>length(opchain$Position)){
  evalPositions %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% distinct() -> evalPositions 
};rm(rf)
# Top n Spreads
evalPositions %>% arrange(.[,length(opchain$Position)+1]) %>% head(100) -> evalPositions

#First spread
opchain$Position<-unlist(evalPositions[1,1:length(opchain$Position)])

#Get the position where opchain$Position!=0
opchain %>% dplyr::filter(Position!=0) -> position

## Stimulation

#parameter provisions

# Num of each Stimulation
StimultaionNum<-100

#Max duration (days) of the Stimulation
MaxStimDay<-14

#geometric brwon motion parameters

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
# sigma_udly(Ntrl)  : getIV_td(histIV[1,]$IVIDX)/sqrt(252)*0.95 0.95:HVとIVの調整項目。
# mu_iv(Ntrl)       : (1.0)^(1/252)-1
# sigma_iv(auto cal): max{VoV-abs(cor(IV,UDL)),0}
# 
# w1-w12の重みを対象となるUDLYやマーケットの見通しに基づき定める
#
# wx(!=0)の重みを与えられたシナリオについてstimulationを行い、weightで重み付けられた結果
# を最終評価値とする
mu_udly<-c(rep(((1.0)^(1/252)-1),times=6),rep(((0.85)^(1/252)-1),times=3),rep(((1.15)^(1/252)-1),times=3))
sigma_udly<-c(rep(getIV_td(histIV[1,]$IVIDX)/sqrt(252)*0.95,times=3),rep(getIV_td(histIV[1,]$IVIDX)/sqrt(252)*0.8*0.95,times=3),
  rep(getIV_td(histIV[1,]$IVIDX)/sqrt(252)*1.3*0.95,times=3),rep(getIV_td(histIV[1,]$IVIDX)/sqrt(252)*0.95,times=3))
mu_iv<-c(rep(((1.0)^(1/252)-1),times=1),rep(((1.25)^(1/252)-1),times=1),rep(((0.75)^(1/252)-1),times=1))
mu_iv<-rep(mu_iv,times=4)
#(annuual.daily.volatility(getIV_td(histIV$IVIDX))$anlzd-abs(PC1dCtC_IVCF1dCtC$cor))/sqrt(252)
sigma_iv<-rep(max(c(
  (annuual.daily.volatility(getIV_td(histIV$IVIDX))$anlzd-abs(PC1dCtC_IVCF1dCtC$cor))/sqrt(252),
  0)),times=12)
weight<-c(c(1.0,0.8,0.8),c(0.6,0.4,0.6),c(0.7,1.0,0.2),c(0.4,0.4,0.4))
weight<-weight/sum(weight)

modelScenario<-data.frame(mu_udly=mu_udly,sigma_udly=sigma_udly,mu_iv=mu_iv,sigma_iv=sigma_iv,weight=weight)
rm(mu_udly,sigma_udly,mu_iv,sigma_iv,weight)

#start stimulation
modelScenario %>% rowwise() %>% do(stimrslt=Stimulate(position=position,StimultaionNum=StimultaionNum,MaxStimDay=MaxStimDay,PosMultip=PosMultip,
                                                      mu_udly=.$mu_udly,sigma_udly=.$sigma_udly,
                                                      mu_iv=.$mu_iv,sigma_iv=.$sigma_iv)) -> modelStimRawlist

modelStimRawlist %>% rowwise() %>% do(resdf=getStimResultDataFrame(.$stimrslt,StimultaionNum)) -> tmp
modelScenario$resdf<-tmp$resdf ; rm(tmp)

#sorting
modelScenario %>% rowwise() %>% do(resdf=.$resdf %>% dplyr::arrange(profit)) -> tmp
tmp$resdf->modelScenario$resdf ; rm(tmp)

#profit statistic
modelScenario %>% rowwise() %>% do(min_profit=.$resdf[1,]$profit) -> tmp
unlist(tmp)->modelScenario$min_profit;rm(tmp)
modelScenario %>% rowwise() %>% do(min_profit=.$resdf[nrow(.$resdf),]$profit) -> tmp
unlist(tmp)->modelScenario$max_profit;rm(tmp)
modelScenario %>% rowwise() %>% do(mean_profit=mean(.$resdf$profit)) -> tmp
unlist(tmp)->modelScenario$mean_profit;rm(tmp)
modelScenario %>% rowwise() %>% do(median_profit=median(.$resdf$profit)) -> tmp
unlist(tmp)->modelScenario$median_profit;rm(tmp)
modelScenario %>% rowwise() %>% do(profit_sd=sd(.$resdf$profit)) -> tmp
unlist(tmp)->modelScenario$profit_sd;rm(tmp)

for(i in 1:nrow(modelScenario) ){
  pdf(file=paste(ResultFiles_Path_G,Underying_Symbol_G,"_report","1","_",i,".pdf",sep=""))
  resdf<-modelScenario$resdf[[i]]
  #udly price histgram
  gg <- ggplot(resdf,aes(x=udly))+geom_histogram(alpha=0.9,aes(y=..density..))+geom_density(size=1.0,adjust=0.8,colour="cyan2")+
    geom_point(x=mean(position$UDLY),y=0,size=6.0,width=3.0,colour="red",pch=3)+
    geom_point(x=mean(resdf$udly),y=0,size=6.0,colour="red",pch=1)+
    geom_point(x=median(resdf$udly),y=0,size=6.0,colour="orange",pch=1)+
    geom_point(x=mean(resdf$udly)+sd(resdf$udly),y=0,size=6.0,colour="darkgrey",pch=2)+
    geom_point(x=mean(resdf$udly)-sd(resdf$udly),y=0,size=6.0,colour="darkgrey",pch=2)
  print(gg)
  
  #payoff function
  gg <- ggplot(resdf,aes(x=udly,y=profit,colour=liqDay))+
    geom_point()+
   geom_point(x=mean(position$UDLY),y=0,size=6.0,colour="red",pch=3)+
   geom_point(x=mean(resdf$udly),y=0,size=6.0,colour="red",pch=1)+
   geom_point(x=median(resdf$udly),y=0,size=6.0,colour="orange",pch=1)+
   geom_point(x=mean(resdf$udly)+sd(resdf$udly),y=0,size=6.0,colour="darkgrey",pch=2)+
   geom_point(x=mean(resdf$udly)-sd(resdf$udly),y=0,size=6.0,colour="darkgrey",pch=2)
  print(gg)
  
  #profit histgram
  gg <- ggplot(resdf,aes(x=profit))+geom_histogram(alpha=0.9,aes(y=..density..))+geom_density(size=1.0,adjust=0.3,colour="cyan2")+
    geom_point(x=mean(resdf$profit),y=0,size=5.0,colour="red",pch=3)+
   geom_point(x=median(resdf$profit),y=0,size=5.0,colour="orange",pch=1)+
   geom_point(x=mean(resdf$profit)+sd(resdf$profit),y=0,size=5.0,colour="darkgrey",pch=2)+
   geom_point(x=mean(resdf$profit)-sd(resdf$profit),y=0,size=5.0,colour="darkgrey",pch=2)
  print(gg)
  dev.off()
};rm(i);rm(gg)


#all cleanings.
rm(stimRslt1,resdf)
rm(histIV,position,mu_udly,sigma_udly,mu_iv,sigma_iv,MaxStimDay,StimultaionNum)
rm(evalPositions,opchain)

#Parameters Cleaning
rm(CallIVChgDown,CallIVChgUp,CallVCone,PutIVChgDown,PutIVChgUp,PutVCone)
rm(PC1dCtC_IVCF1dCtC,PC3dCtC_IVCF3dCtC,PC5dCtC_IVCF5dCtC,PC7dCtC_IVCF7dCtC)
rm(PCIVndCtC,PCndCtC,SkewModel)
rm(riskFreeRate_G,divYld_G,OpType_Put_G,OpType_Call_G,TimeToExp_Limit_Closeness_G)
rm(Underying_Symbol_G,DataFiles_Path_G,ResultFiles_Path_G,holdDays,dviv_caldays,PosMultip)

