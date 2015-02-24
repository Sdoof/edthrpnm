library(dplyr)
library(RQuantLib)
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

getIV_td<-function(ividx_cd){
  ividx_td <- ividx_cd*sqrt(365/252)
  ividx_td
}

getThetaEffect<-function(pos,greek,multi=PosMultip,hdd=holdDays){
  theta<-getPosGreeks(pos=position$Position,greek=position$Theta)
  thetaEfct<-holdDays*theta
  thetaEfct
}

getDeltaEffect<-function(pos,greek,UDLY,ividx_td,multi=PosMultip,hdd=holdDays){
  expPriceChange<-mean(UDLY*(exp(ividx_td*sqrt(hdd/365))-1))
  delta<-getPosGreeks(pos=pos,greek=greek)
  deltaEfct<-(-abs(delta))*expPriceChange
  deltaEfct
}

getGammaEffect<-function(pos,greek,UDLY,ividx_td,multi=PosMultip,hdd=holdDays){
  expPriceChange<-mean(UDLY*(exp(ividx_td*sqrt(hdd/365))-1))
  gamma<-getPosGreeks(pos=pos,greek=greek)
  gammaEfct<-gamma*(expPriceChange^2)/2
  gammaEfct
}

#Here we do not care the effect of Volga as we did the Gamma effect, is this really appropriate?
getVegaEffect<-function(pos,greek,ividx,dviv,multi=PosMultip,hdd=holdDays){
  expIVChange<-mean(ividx*(exp(dviv*sqrt(holdDays))-1))
  #Or use Annualized Volatility of Impled Volatility. Should be the same result.
  #  aviv<-annuual.daily.volatility(histIV$IVIDX)$anlzd*sqrt(holdDays/252)
  #  expIVChange<-mean(ividx*(exp(aviv)-1))
  vega<-getPosGreeks(pos=pos,greek=greek)
  vegaEffect<-(-abs(vega))*(expIVChange*100)
  vegaEffect
}

getDTRRR<-function(position,multi=PosMultip,hdd=holdDays){
  thetaEffect<-getThetaEffect(pos=position$Position,greek=position$Theta)
  
  deltaEffect<-getDeltaEffect(pos=position$Position,greek=position$Delta,
                              UDLY=position$UDLY,ividx_td=getIV_td(position$IVIDX))
  
  gammaEffect<-getGammaEffect(pos=position$Position,greek=position$Gamma,
                              UDLY=position$UDLY,ividx_td=getIV_td(position$IVIDX))  
  DTRRR<-(deltaEffect+gammaEffect)/thetaEffect
  DTRRR
}

getVTRRR<-function(position,ividx,dviv,multi=PosMultip,hdd=holdDays){
  thetaEffect<-getThetaEffect(pos=position$Position,greek=position$Theta)
  
  vegaEffect<-getVegaEffect(pos=position$Position,greek=position$Vega,
                            ividx=ividx,dviv=dviv)
  VTRRR<-vegaEffect/thetaEffect
  VTRRR
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
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",nrows=1999);rm(rf)
  #filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(position$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

#Calculating DTRRR (DELTA/THETA RISK/RETURN RATIO)
#Theta Effect
#getThetaEffect(pos=position$Position,greek=position$Theta)
#Delta Effect
  #Volatility Index is in calinder days. So must adjust. 
  #We calculate our greeks in trading days, so IVIDX must be 
#getDeltaEffect(pos=position$Position,greek=position$Delta,
#               UDLY=position$UDLY,ividx_td=getIV_td(position$IVIDX))
#Gamma Effect
#getGammaEffect(pos=position$Position,greek=position$Gamma,
#               UDLY=position$UDLY,ividx_td=getIV_td(position$IVIDX))
#DTRRR
getDTRRR(position=position)

#Calculating VTRRR(VEGA/THETA RISK/RETURN RATIO)

#Theta Effect again
#theta<-getPosGreeks(pos=position$Position,greek=position$Theta)
#thetaEfct<-holdDays*theta  ;rm(theta)

#Expected Change in Impliev Volatility over the holding days.
#dviv<-annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily
#expIVChange<-mean(getIV_td(position$IVIDX)*(exp(dviv*sqrt(holdDays))-1)) ; rm(dviv)
 
#Vega Effect
#vega<-getPosGreeks(pos=position$Position,greek=position$Vega)
#vegaEfct<-(-abs(vega))*(expIVChange*100) ;rm(vega)
#getVegaEffect(pos=position$Position,greek=position$Vega,
#             ividx=getIV_td(position$IVIDX),dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)
#VTRRR
#(VTRRR<-(vegaEfct/thetaEfct))   ;rm(thetaEfct,expIVChange,vegaEfct,VTRRR)

getVTRRR(position=position,ividx=getIV_td(position$IVIDX),dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)

#Factory of Volatility Level Regression Result
get.Volatility.Level.Regression<-function(Days=holdDays,ctoc=TRUE){
  if((!ctoc)*(Days==1)){
    return (PC1dCtO_IVCF1dCtO)
  }else if(ctoc*(Days==1)){
    return(PC1dCtC_IVCF1dCtC)
  } else if(ctoc*(Days==3)){
    return(PC3dCtC_IVCF3dCtC)
  }else if(ctoc*(Days==5)){
    return(PC5dCtC_IVCF5dCtC)
  }else if(ctoc*(Days==7)){
    return(PC7dCtC_IVCF7dCtC)
  }
}

#read from get.Volatility.Change.Regression.Result to get specific regression values.
get.VolChg<-function(model,month){
  chg<-predict(model,x=month)
  return(chg)
}

#get the ATMIV_pos/ATMIV_pre (Result of Vchg regression) as a vector for each position element
get.Volatility.Change.Regression.Result<-function(pos,up_dn){
  atmiv_chg<-           (pos$TYPE==OpType_Put_G)*(up_dn>=0)*(get.VolChg(model=PutIVChgUp,month=pos$TimeToExpDate))$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Call_G)*(up_dn>=0)*(get.VolChg(model=CallIVChgUp,month=pos$TimeToExpDate))$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Put_G)*(up_dn<0)*(get.VolChg(model=PutIVChgDown,month=pos$TimeToExpDate))$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Call_G)*(up_dn<0)*(get.VolChg(model=CallIVChgDown,month=pos$TimeToExpDate))$y
  atmiv_chg
}

#read from et.Volatility.Cone.Regression.Result to get specific regression values.
get.VCone<-function(model,month){
  cone<-predict(model,x=month)
  return(cone)
}

#get the ATMIV_pos/ATMIV_pre (Result of Vchg regression) as a vector for each position element
get.Volatility.Cone.Regression.Result<-function(optype,month){
  cone<-(optype==OpType_Put_G)*(get.VCone(model=PutVCone,month=month))$y
  cone<-cone+(optype==OpType_Call_G)*(get.VCone(model=CallVCone,month=month))$y
  cone
}

#when we get the Future Option, we must consider spot change and
#their forward(future) prices. here just treat stock or other (not future) option
get.UDLY.Changed.Price<-function(udly,chg_pct){
  change<-udly*chg_pct
  change
}

##Risk/Return calculation Scenario　-------------------------------
udly_chg_pct<-0.03

# UDLY_pre/UDLY_posに対するIVIDX_pre/IVIDX_pos
regression<-get.Volatility.Level.Regression()
ividx_chg_pct<-get.predicted.IVIDXChange(model=regression$model,xmin=udly_chg_pct,xmax=100,x_by=0)$IVIDXC
#IVIDX_pre/IVIDX_posに対するATMIV_pos/ATMIV_pre
#get.predicted.IVIDXChange(model=regression$model,
#                          xmin=min(c(-udly_chg_pct,udly_chg_pct)),
#                          xmax=max(c(-udly_chg_pct,udly_chg_pct)))
position$IVIDX<-position$IVIDX*(1+ividx_chg_pct)

## ATM IV change
position$ATMIV<-position$ATMIV*(1+ividx_chg_pct)*get.Volatility.Change.Regression.Result(position,ividx_chg_pct)

#Volatility Cone の影響。時間変化した分の影響を受ける。その比の分だけ比率変化
#上で求めたATMIV_pos <- ATMIV_pos*(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pre/(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pos
bdays_per_month<-252/12
TimeToExpDate_pos<-(position$TimeToExpDate*bdays_per_month-holdDays)/bdays_per_month
#get.Volatility.Cone.Regression.Result(position$TYPE,position$TimeToExpDate)
#get.Volatility.Cone.Regression.Result(position$TYPE,TimeToExpDate_pos)
position$ATMIV<-position$ATMIV *
  get.Volatility.Cone.Regression.Result(position$TYPE,TimeToExpDate_pos)/
  get.Volatility.Cone.Regression.Result(position$TYPE,position$TimeToExpDate)
#set new TimeToExpDate
position$TimeToExpDate<-TimeToExpDate_pos
#time advance
position$Date <- format(advance("UnitedStates/NYSE",dates=as.Date(position$Date,format="%Y/%m/%d"),
                              holdDays,0),"%Y/%m/%d")
#set new value to UDLY
position$UDLY <- position$UDLY+get.UDLY.Changed.Price(udly=position$UDLY,chg_pct=udly_chg_pct)
#set new value to HowfarOOM, Moneyness.Nm
position$Moneyness.Frac<-position$Strike/position$UDLY
position$HowfarOOM<-(1-position$Moneyness.Frac)*position$TYPE
position$Moneyness.Nm<-log(position$Moneyness.Frac)/position$ATMIV/sqrt(position$TimeToExpDate)
position$Moneyness.Frac<-NULL

#calculate IV_pos(OrigIV) using SkewModel based on model definition formula.

#calculate pption price and thier greeks

