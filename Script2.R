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
#assiging initial Position?
#get position where opchain$Position!=0
opchain %>% dplyr::filter(Position!=0) -> position

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

#Stimulated Result File

rf<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep="")
evalPositions<-read.table(rf,header=F,sep=",")
length(opchain$Position)
if(length(evalPositions)>length(opchain$Position)){
  evalPositions %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% distinct() -> evalPositions 
};rm(rf)

evalPositions[1,1:length(opchain$Position)]

##
# Stimulation

#parameter provisions

# Num of each Stimulation
StimultaionNum<-1

#Max duration (days) of the Stimulation
MaxStimDay<-14

#geometric brwon motion parameters

#create pertubation combinations of these parameters
#mu_udly=(0.92)^(1/252)-1
mu_udly=(1.0)^(1/252)-1
#sigma_udly=0.25/sqrt(252)
sigma_udly<-getIV_td(histIV[1,]$IVIDX)/sqrt(252)*0.7 # 0.9 is coeffecient to estimite HV

mu_iv=(1.0)^(1/252)-1
#sigma_iv<-0.8/sqrt(252)
#(market_vov - cor(udly,ivid)) heuristic etc. sigma_iv<-(0.8-co)/sqrt(252)
sigma_iv=0.22/sqrt(252)

# start stimulating
stimRslt1<-Stimulate(position=position,StimultaionNum=StimultaionNum,MaxStimDay=MaxStimDay,PosMultip=PosMultip,
          mu_udly=mu_udly,sigma_udly=sigma_udly,
          mu_iv=mu_iv,sigma_iv=sigma_iv)

resdf<-getStimResultDataFrame(stimRslt1,StimultaionNum)

# max profit and corresponding Underlying Price
resdf %>% dplyr::arrange(profit) -> resdf
resdf[1,]$profit #minimum profit
resdf[nrow(resdf),]$profit #maximum profit
mean(resdf$profit) #mean profit
median(resdf$profit) #median profit
sd(resdf$profit) # profit standard deviation

#udly price histgram
gg <- ggplot(resdf,aes(x=udly))+geom_histogram(alpha=0.9,aes(y=..density..))+geom_density(size=1.0,adjust=0.8,colour="cyan2")
(gg+geom_point(x=mean(position$UDLY),y=0,size=6.0,width=3.0,colour="red",pch=3)
 +geom_point(x=mean(resdf$udly),y=0,size=6.0,colour="red",pch=1)
 +geom_point(x=median(resdf$udly),y=0,size=6.0,colour="orange",pch=1)
 +geom_point(x=mean(resdf$udly)+sd(resdf$udly),y=0,size=6.0,colour="darkgrey",pch=2)
 +geom_point(x=mean(resdf$udly)-sd(resdf$udly),y=0,size=6.0,colour="darkgrey",pch=2)
)
rm(gg)

#payoff function
gg <- ggplot(resdf,aes(x=udly,y=profit,colour=liqDay))
(gg+geom_point()
 +geom_point(x=mean(position$UDLY),y=0,size=6.0,colour="red",pch=3)
 +geom_point(x=mean(resdf$udly),y=0,size=6.0,colour="red",pch=1)
 +geom_point(x=median(resdf$udly),y=0,size=6.0,colour="orange",pch=1)
 +geom_point(x=mean(resdf$udly)+sd(resdf$udly),y=0,size=6.0,colour="darkgrey",pch=2)
 +geom_point(x=mean(resdf$udly)-sd(resdf$udly),y=0,size=6.0,colour="darkgrey",pch=2)
)
rm(gg)

#profit histgram
gg <- ggplot(resdf,aes(x=profit))+geom_histogram(alpha=0.9,aes(y=..density..))+geom_density(size=1.0,adjust=0.3,colour="cyan2")
(gg+geom_point(x=mean(resdf$profit),y=0,size=5.0,colour="red",pch=3)
 +geom_point(x=median(resdf$profit),y=0,size=5.0,colour="orange",pch=1)
 +geom_point(x=mean(resdf$profit)+sd(resdf$profit),y=0,size=5.0,colour="darkgrey",pch=2)
 +geom_point(x=mean(resdf$profit)-sd(resdf$profit),y=0,size=5.0,colour="darkgrey",pch=2)
) 
rm(gg)

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

