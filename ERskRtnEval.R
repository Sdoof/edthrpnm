library(dplyr)
#
#Holding Period
#holdDays<-3*252/365 #Trading Days. This should be correct.
holdDays<-3
#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays<-20
#Multipler of Position
PosMultip<-100

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

#get the position's total greek
getPosGreeks<-function(pos,greek,multi=PosMultip){
  pos_greek<-sum(pos*multi*greek)
  pos_greek
}

#Option Position Data. Here we use UDL_Positions_Pre
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
position<-read.table(rf,header=T,sep=",")
  #filtering
position %>% dplyr::filter(Position!=0) %>%
  dplyr::select(-(starts_with('dummy',ignore.case=TRUE)),
                -(contains('Frac',ignore.case=TRUE)),
                -(IV)) %>% as.data.frame() -> position

#Historical Implied Volatility Data
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") ;rm(rf)
histIV<-read.table(rf,header=T,sep=",",nrows=1999)
  #filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(position$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

#Calculating DTRRR (DELTA/THETA RISK/RETURN RATIO)

#Theta Effect
#theta<-sum(position$Position*position$Theta)
theta<-getPosGreeks(pos=position$Position,greek=position$Theta)
thetaEfct<-holdDays*theta  ;rm(theta)

#Delta Effect
#Volatility Index is in calinder days. So must adjust. 
#We calculate our greeks in trading days, so IVIDX must be 
(IVIDX_td<-position$IVIDX*sqrt(365/252))
(expPriceChange<-mean(position$UDLY*(exp(IVIDX_td*sqrt(holdDays/365))-1)))
#delta<-sum(position$Position*position$Delta)
delta<-getPosGreeks(pos=position$Position,greek=position$Delta)
deltaEfct<-(-abs(delta))*expPriceChange
rm(IVIDX_td,delta)
#Gamma Effect
#(gamma<-sum(position$Position*position$Gamma))
gamma<-getPosGreeks(pos=position$Position,greek=position$Gamma)
gammaEfct<-gamma*(expPriceChange^2)/2  ;rm(expPriceChange,gamma)
#DTRRR
(DTRRR<-(deltaEfct+gammaEfct)/thetaEfct)  ;rm(deltaEfct,gammaEfct,thetaEfct,DTRRR)

#Calculating VTRRR(VEGA/THETA RISK/RETURN RATIO)

#Theta Effect again
theta<-getPosGreeks(pos=position$Position,greek=position$Theta)
thetaEfct<-holdDays*theta  ;rm(theta)

#Expected Change in Impliev Volatility over the holding days.
dviv<-annuual.daily.volatility(histIV$IVIDX)$daily
expIVChange<-mean(position$IVIDX*(exp(dviv*sqrt(holdDays))-1)) ; rm(dviv)
  #Or use Annualized Volatility of Impled Volatility. Should be the same result.
  #aviv<-annuual.daily.volatility(histIV$IVIDX)$anlzd*sqrt(holdDays/252)
  #(expIVChange<-mean(position$IVIDX*(exp(aviv)-1)))  ;rm(aviv)
#Vega Effect
#Here we do not care the effect of Volga as we did the Gamma effect, is this really appropriate?
vega<-getPosGreeks(pos=position$Position,greek=position$Vega)
vegaEfct<-(-abs(vega))*(expIVChange*100) ;rm(vega)

#VTRRR
(VTRRR<-(vegaEfct/thetaEfct))   ;rm(thetaEfct,expIVChange,vegaEfct,VTRRR)
