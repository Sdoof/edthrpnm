library(dplyr)
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
load.PC2IV(PC="PC1dCtO",IVC="IVCF1dCtO")
PC1dCtO_IVCF1dCtO
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

#Option Position Data. Here we use UDL_Positions_Pre
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
position<-read.table(rf,header=T,sep=",")
rm(rf)
  #filtering
position %>% dplyr::filter(Position!=0) %>%
  dplyr::select(-(starts_with('dummy',ignore.case=TRUE)),
                -(contains('Frac',ignore.case=TRUE)),
                -(IV)) %>% as.data.frame() -> position
HPD<-3
Theta<-sum(position$Position*position$Theta)
ThetaEffect<-HPD*Theta
rm(Theta,ThetaEffect)

#Volatility Index is in calinder days. So must adjust. 
#We calculate our greeks in trading days, so IVIDX must be 

#EPC<-position$UDLY*(exp(position$IVIDX*sqrt(HPD/365))-1)
expPriceChange<-mean(position$UDLY*(exp(position$IVIDX*sqrt(365/252)*sqrt(HPD/365))-1))
deltaEfct<-(-abs(sum(position$Delta)))*expPriceChange


