library(dplyr)
library(RQuantLib)
library(DEoptim)
library(rgenoud)
library(Rsolnp)
library(DEoptimR)
library(mcga)
library(GenSA)
#library(powell)
#library(hydroPSO)
#library(nleqslv)
#library(dfoptim)

#Variables -----------
#Holding Period
#holdDays<-3*252/365 #Trading Days. This should be correct.
holdDays<-3
#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays<-20
#Multipler of Position
PosMultip<-100

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

#Rsk/Rtn greek related functions --------------
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
  theta<-getPosGreeks(pos=pos,greek=greek)
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
  
  #DTRRR<-(deltaEffect+gammaEffect)/thetaEffect
  #DTRRR<- exp(deltaEffect+gammaEffect)*exp(thetaEffect)
  thetaEffect<-as.numeric(thetaEffect!=0)*thetaEffect+as.numeric(thetaEffect==0)*0.001
  DTRRR<- (deltaEffect+gammaEffect+(0.5*thetaEffect))/abs(thetaEffect)
  DTRRR
}

getVTRRR<-function(position,ividx,dviv,multi=PosMultip,hdd=holdDays){
  thetaEffect<-getThetaEffect(pos=position$Position,greek=position$Theta)
  #thetaEffect<-as.numeric(thetaEffect>0)*thetaEffect+as.numeric(thetaEffect<0)*(0.001)+as.numeric(thetaEffect==0)*0.001
  
  vegaEffect<-getVegaEffect(pos=position$Position,greek=position$Vega,
                            ividx=ividx,dviv=dviv)
  #VTRRR<-vegaEffect/thetaEffect
  #VTRRR<- exp(vegaEffect)*exp(-1*thetaEffect)
  
  thetaEffect<-as.numeric(thetaEffect!=0)*thetaEffect+as.numeric(thetaEffect==0)*0.001
  VTRRR<- (vegaEffect+(0.5*thetaEffect))/abs(thetaEffect)
  VTRRR
}

#Option Chain and Position Data. Here we use UDL_Positions_Pre ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",")
#filtering. deleting unnecessary column
opchain %>% dplyr::select(-(starts_with('dummy',ignore.case=TRUE)),
                      -(contains('Frac',ignore.case=TRUE)),
                      -(IV)) %>% as.data.frame() -> opchain
#only OOM targeted
opchain %>% dplyr::filter(HowfarOOM>=0) -> opchain
#assiging initial Position?
#get position where opchain$Position!=0
opchain %>% dplyr::filter(Position!=0) -> position

##Historical Implied Volatility Data ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",nrows=1999);rm(rf)
  #filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(position$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

#get DTRRR VTRRR (**Just for test)---------
getDTRRR(position=position)
getVTRRR(position=position,
         ividx=getIV_td(position$IVIDX),
         dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)

#Rsk/Rtn Scenario functions -----------------------------
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

#Inner functions for gruped or rowwise operations. -------------
#evalPosRskRtnXXX is just wrappers for Rsk/Rtn greek related functions.
evalPosRskRtnDTRRR<- function(pos_eval){
  pos_DTRRR<-pos_eval$pos
  dtrrr<-getDTRRR(position=pos_DTRRR)
  dtrrr
}

evalPosRskRtnVTRRR<- function(pos_eval){
  pos_DTRRR<-pos_eval$pos
#  print(pos_DTRRR)
  #dviv should be pre-calculated when optimize
  vtrrr<-getVTRRR(position=pos_DTRRR,ividx=getIV_td(pos_DTRRR$IVIDX),dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)
  vtrrr
}

evalPosRskRtnThetaEffect<- function(pos_eval){
  pos<-pos_eval$pos
#  print(pos)
  thetaEffect<-getThetaEffect(pos=pos$Position,greek=pos$Theta)
  thetaEffect
}

evalPosRskRtnDeltaEffect<- function(pos_eval){
  pos<-pos_eval$pos
#  print(pos)
  deltaEffect<-getDeltaEffect(pos=pos$Position,greek=pos$Delta,
                              UDLY=pos$UDLY,ividx_td=getIV_td(pos$IVIDX))
  deltaEffect
}

evalPosRskRtnGammaEffect<- function(pos_eval){
  pos<-pos_eval$pos
#  print(pos)
  gammaEffect<-getGammaEffect(pos=pos$Position,greek=pos$Gamma,
                              UDLY=pos$UDLY,ividx_td=getIV_td(pos$IVIDX))
  gammaEffect
}

evalPosRskRtnVegaEffect<- function(pos_eval){
  pos<-pos_eval$pos
#  print(pos)
  #dviv should be pre-calculated when optimize
  vegaEffect<-getVegaEffect(pos=pos$Position,greek=pos$Vega,
                            ividx=getIV_td(pos$IVIDX),dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)
  vegaEffect
}

# operate to each position data frame based on scenaro changes
reflectPosChg<- function(process_df,days=holdDays){
  pos<-as.data.frame(process_df$pos[1])
  chg<-as.numeric(process_df$udlChgPct[1])
 # print(chg)
  
  # get (IVIDX_pre/IVIDX_pos)/(UDLY_pre/UDLY_pos)
  regression<-get.Volatility.Level.Regression()
  ividx_chg_pct<-get.predicted.IVIDXChange(model=regression$model,xmin=chg,xmax=100,x_by=0)$IVIDXC
  pos$IVIDX<-pos$IVIDX*(1+ividx_chg_pct)
  
  # ATM IV change
  pos$ATMIV<-pos$ATMIV*(1+ividx_chg_pct)*get.Volatility.Change.Regression.Result(pos,ividx_chg_pct)
  
  #Volatility Cone の影響。時間変化した分の影響を受ける。その比の分だけ比率変化
  #ATMIV_pos <- ATMIV_pos*(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pre/(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pos
  bdays_per_month<-252/12
  TimeToExpDate_pos<-(pos$TimeToExpDate*bdays_per_month-days)/bdays_per_month
  pos$ATMIV<-pos$ATMIV *
    get.Volatility.Cone.Regression.Result(pos$TYPE,TimeToExpDate_pos)/
    get.Volatility.Cone.Regression.Result(pos$TYPE,pos$TimeToExpDate)

  #set new TimeToExpDate
  pos$TimeToExpDate<-TimeToExpDate_pos

  #Date advance
  pos$Date <- format(advance("UnitedStates/NYSE",dates=as.Date(pos$Date,format="%Y/%m/%d"),
                                  days,0),"%Y/%m/%d")
  
  #set new value to UDLY
  pos$UDLY <- pos$UDLY+get.UDLY.Changed.Price(udly=pos$UDLY,chg_pct=chg)
  
  #set new value to HowfarOOM, Moneyness.Nm
  pos$Moneyness.Frac<-pos$Strike/pos$UDLY
  pos$HowfarOOM<-(1-pos$Moneyness.Frac)*pos$TYPE
  
  #if TimeToExpDate < TimeToExp_Limit_Closeness_G(0.3 etc), TimeToExpDate should be TimeToExp_Limit_Closeness_G.
  #Otherwise use the TimeToExpDate values themselves.
  eval_timeToExpDate<-as.numeric(pos$TimeToExpDate<TimeToExp_Limit_Closeness_G)*TimeToExp_Limit_Closeness_G+
    as.numeric(pos$TimeToExpDate>=TimeToExp_Limit_Closeness_G)*pos$TimeToExpDate
  pos$Moneyness.Nm<-log(pos$Moneyness.Frac)/pos$ATMIV/sqrt(eval_timeToExpDate)
  pos$Moneyness.Frac<-NULL
  
  #calculate IV_pos(OrigIV) using SkewModel based on model definition formula.
  get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
  pos$ATMIV*get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
  pos$OrigIV<-pos$ATMIV*get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
  
  #calculate pption price and thier greeks
  vgreeks<-set.EuropeanOptionValueGreeks(pos)
  pos$Price<-vgreeks$Price
  pos$Delta<-vgreeks$Delta
  pos$Gamma<-vgreeks$Gamma
  pos$Vega<-vgreeks$Vega
  pos$Theta<-vgreeks$Theta
  pos$Rho<-vgreeks$Rho

  pos
}

#START evaluation ------------
udlStepNum<-3;udlStepPct<-0.03
udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
posEvalTbl<-data.frame(udlChgPct=udlChgPct) ;rm(udlStepNum,udlStepPct)
#Set data frames as a row value of another data frame.
posEvalTbl %>% group_by(udlChgPct) %>% do(pos=position) -> posEvalTbl
#Modify pos based on scenario
posEvalTbl %>% group_by(udlChgPct) %>% do(pos=reflectPosChg(.)) -> posEvalTbl
#DTRRR
posEvalTbl %>% rowwise() %>% do(DTRRR=evalPosRskRtnDTRRR(.)) -> tmp
unlist(tmp$DTRRR)->tmp ; posEvalTbl$DTRRR <- tmp ;rm(tmp)
#VTRRR
posEvalTbl %>% rowwise() %>% do(VTRRR=evalPosRskRtnVTRRR(.)) -> tmp
unlist(tmp$VTRRR)->tmp ; posEvalTbl$VTRRR <- tmp ;rm(tmp)

#debugging(Just for Info ) purpose. when optimized, not necessary.
##
#  Greek Effects

#  ThetaEffect
posEvalTbl %>% rowwise() %>% do(ThetaEffect=getThetaEffect(pos=.$pos$Position,greek=.$pos$Theta)) -> tmp
unlist(tmp$ThetaEffect)->tmp ; posEvalTbl$ThetaEffect <- tmp ;rm(tmp)
#  DeltaEffect
posEvalTbl %>% rowwise() %>% do(DeltaEffect=getDeltaEffect(pos=.$pos$Position,greek=.$pos$Delta,
                                                           UDLY=.$pos$UDLY,
                                                           ividx_td=getIV_td(.$pos$IVIDX))) -> tmp
unlist(tmp$DeltaEffect)->tmp ; posEvalTbl$DeltaEffect <- tmp ;rm(tmp)
#  GammaEffect
posEvalTbl %>% rowwise() %>% do(GammaEffect=getGammaEffect(pos=.$pos$Position,greek=.$pos$Gamma,
                                                           UDLY=.$pos$UDLY,
                                                           ividx_td=getIV_td(.$pos$IVIDX))) -> tmp
unlist(tmp$GammaEffect)->tmp ; posEvalTbl$GammaEffect <- tmp ;rm(tmp) 
#  VegaEffect
posEvalTbl %>% rowwise() %>% do(VegaEffect=getVegaEffect(pos=.$pos$Position,greek=.$pos$Vega,
                                                         ividx=getIV_td(.$pos$IVIDX)
                                                         #dviv should be precalulated when optimized
                                                         ,dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)) -> tmp
unlist(tmp$VegaEffect)->tmp ; posEvalTbl$VegaEffect <- tmp ;rm(tmp)

##
#  Greeks
#
#  UDLY
posEvalTbl %>% rowwise() %>% do(UDLY=mean(.$pos$UDLY)) ->tmp
unlist(tmp$UDLY)->tmp ; posEvalTbl$UDLY <- tmp ;rm(tmp)
#  Price
posEvalTbl %>% rowwise() %>% do(Price=getPosGreeks(pos=.$pos$Position,greek=.$pos$Price)) ->tmp
unlist(tmp$Price)->tmp ; posEvalTbl$Price <- tmp ;rm(tmp)
#  Delta
posEvalTbl %>% rowwise() %>% do(Delta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Delta))->tmp
unlist(tmp$Delta)->tmp ; posEvalTbl$Delta <- tmp ;rm(tmp)
#  Gamma
posEvalTbl %>% rowwise() %>% do(Gamma=getPosGreeks(pos=.$pos$Position,greek=.$pos$Gamma))->tmp
unlist(tmp$Gamma)->tmp ; posEvalTbl$Gamma <- tmp ;rm(tmp)
#  Vega
posEvalTbl %>% rowwise() %>% do(Vega=getPosGreeks(pos=.$pos$Position,greek=.$pos$Vega))->tmp
unlist(tmp$Vega)->tmp ; posEvalTbl$Vega <- tmp ;rm(tmp)
#  Theta
posEvalTbl %>% rowwise() %>% do(Theta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Theta)) ->tmp
unlist(tmp$Theta)->tmp ; posEvalTbl$Theta <- tmp ;rm(tmp)

rm(position,posEvalTbl,udlChgPct,udlStepPct)

## Optimization Test -------------------

hollowNonZeroPosition<-function(pos){
  #opchain is global parameter. to avoid unnessary copying
  opchain$Position<-pos
  opchain %>% dplyr::filter(Position!=0) -> position
  position
}

createPositinEvalTable<-function(position,udlStepNum=3,udlStepPct=0.03,days=holdDays){
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-data.frame(udlChgPct=udlChgPct) ;rm(udlStepNum,udlStepPct)
  #Set data frames as a row value of another data frame.
  posEvalTbl %>% group_by(udlChgPct) %>% do(pos=position) -> posEvalTbl
  #Modify pos based on scenario
  posEvalTbl %>% group_by(udlChgPct) %>% do(pos=reflectPosChg(.,days)) -> posEvalTbl
  #DTRRR
  posEvalTbl %>% rowwise() %>% do(DTRRR=evalPosRskRtnDTRRR(.)) -> tmp
  unlist(tmp$DTRRR)->tmp ; posEvalTbl$DTRRR <- tmp ;rm(tmp)
  #VTRRR
  posEvalTbl %>% rowwise() %>% do(VTRRR=evalPosRskRtnVTRRR(.)) -> tmp
  unlist(tmp$VTRRR)->tmp ; posEvalTbl$VTRRR <- tmp ;rm(tmp)
  
  #debugging(Just for Info ) purpose. when optimized, not necessary.
  ##
  #  Greek Effects
  
  #  ThetaEffect
  posEvalTbl %>% rowwise() %>% do(ThetaEffect=getThetaEffect(pos=.$pos$Position,greek=.$pos$Theta)) -> tmp
  unlist(tmp$ThetaEffect)->tmp ; posEvalTbl$ThetaEffect <- tmp ;rm(tmp)
  #  DeltaEffect
  posEvalTbl %>% rowwise() %>% do(DeltaEffect=getDeltaEffect(pos=.$pos$Position,greek=.$pos$Delta,
                                                             UDLY=.$pos$UDLY,
                                                             ividx_td=getIV_td(.$pos$IVIDX))) -> tmp
  unlist(tmp$DeltaEffect)->tmp ; posEvalTbl$DeltaEffect <- tmp ;rm(tmp)
  #  GammaEffect
  posEvalTbl %>% rowwise() %>% do(GammaEffect=getGammaEffect(pos=.$pos$Position,greek=.$pos$Gamma,
                                                             UDLY=.$pos$UDLY,
                                                             ividx_td=getIV_td(.$pos$IVIDX))) -> tmp
  unlist(tmp$GammaEffect)->tmp ; posEvalTbl$GammaEffect <- tmp ;rm(tmp) 
  #  VegaEffect
  posEvalTbl %>% rowwise() %>% do(VegaEffect=getVegaEffect(pos=.$pos$Position,greek=.$pos$Vega,
                                                           ividx=getIV_td(.$pos$IVIDX)
                                                           #dviv should be precalulated when optimized
                                                           ,dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)) -> tmp
  unlist(tmp$VegaEffect)->tmp ; posEvalTbl$VegaEffect <- tmp ;rm(tmp)
  
  ##
  #  Greeks
  #
  #  UDLY
  posEvalTbl %>% rowwise() %>% do(UDLY=mean(.$pos$UDLY)) ->tmp
  unlist(tmp$UDLY)->tmp ; posEvalTbl$UDLY <- tmp ;rm(tmp)
  #  Price
  posEvalTbl %>% rowwise() %>% do(Price=getPosGreeks(pos=.$pos$Position,greek=.$pos$Price)) ->tmp
  unlist(tmp$Price)->tmp ; posEvalTbl$Price <- tmp ;rm(tmp)
  #  Delta
  posEvalTbl %>% rowwise() %>% do(Delta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Delta))->tmp
  unlist(tmp$Delta)->tmp ; posEvalTbl$Delta <- tmp ;rm(tmp)
  #  Gamma
  posEvalTbl %>% rowwise() %>% do(Gamma=getPosGreeks(pos=.$pos$Position,greek=.$pos$Gamma))->tmp
  unlist(tmp$Gamma)->tmp ; posEvalTbl$Gamma <- tmp ;rm(tmp)
  #  Vega
  posEvalTbl %>% rowwise() %>% do(Vega=getPosGreeks(pos=.$pos$Position,greek=.$pos$Vega))->tmp
  unlist(tmp$Vega)->tmp ; posEvalTbl$Vega <- tmp ;rm(tmp)
  #  Theta
  posEvalTbl %>% rowwise() %>% do(Theta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Theta)) ->tmp
  unlist(tmp$Theta)->tmp ; posEvalTbl$Theta <- tmp ;rm(tmp)
  
  posEvalTbl
  
}

#function optimized

obj_Income <- function(x){
  x<-round(x)
#   x<-floor(x)

  print(x)
  position<-hollowNonZeroPosition(pos=x)

  udlStepNum<-3; udlStepPct<-0.03
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  sd_multp<-25;anlzd_sd<-0.2
  
  #weight is normalized
  weight<-dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct / 
    sum(dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct)

  #print(posEvalTbl$pos)
  # print(posEvalTbl)
    
  #print(weight)
 
  #return(posEvalTbl)
  pos_change<-sum(as.numeric((x-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  print(pos_change)
  print(penalty1) 
  grkeval<--sum(posEvalTbl$DTRRR*weight+posEvalTbl$VTRRR*weight)
  print(grkeval)
 
  val<-grkeval*penalty1
  print(val)
  return(val)
}

obj_Income <- function(x){
  x<-round(x)
  if(sum(as.numeric(round(x)!=0))==0){
    x<-runif(length(x),-5,5)
    x<-round(x)
  }
  #   x<-floor(x)
  
  print(x)
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-3; udlStepPct<-0.03
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  sd_multp<-25;anlzd_sd<-0.2
  
  #weight is normalized
  weight<-dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct / 
    sum(dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct)
  
  #print(posEvalTbl$pos)
  # print(posEvalTbl)
  
  #print(weight)
  
  #return(posEvalTbl)
  pos_change<-sum(as.numeric((x-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  print(pos_change)
  print(penalty1) 
  grkeval<--sum(posEvalTbl$DTRRR*weight+posEvalTbl$VTRRR*weight)
  print(grkeval)
  
  val<-grkeval*penalty1
  print(val)
  return(val)
}

obj_Income_genoud_int <- function(x){
  #x<-round(x)
  if(sum(as.numeric(round(x)!=0))==0){
    x<-runif(length(x),-5,5)
    x<-round(x)
  }
  print(x)
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-3; udlStepPct<-0.03
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  sd_multp<-25;anlzd_sd<-0.2
  
  #weight is normalized
  weight<-dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct / 
    sum(dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct)
  
  #print(posEvalTbl$pos)
  # print(posEvalTbl)
  
  #print(weight)
  
  #return(posEvalTbl)
  pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  print(pos_change)
  print(penalty1) 
  grkeval<--sum(posEvalTbl$DTRRR*weight+posEvalTbl$VTRRR*weight)
  print(grkeval)
  
  val<-grkeval*penalty1
  print(val)
  return(val)
}

obj_Income_genoud_lex_int <- function(x){
  #x<-round(x)
  if(sum(as.numeric(round(x)!=0))==0){
    x<-runif(length(x),-5,5)
    x<-round(x)
  }
  print(x)
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-3; udlStepPct<-0.03
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  sd_multp<-25;anlzd_sd<-0.2
  
  #weight is normalized
  weight<-dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct / 
    sum(dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct)
  
  #print(posEvalTbl$pos)
  # print(posEvalTbl)
  
  #print(weight)
  
  #return(posEvalTbl)
  pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  print(pos_change)
  print(penalty1) 
  grkeval<--sum(posEvalTbl$DTRRR*weight+posEvalTbl$VTRRR*weight)
  print(grkeval)
  
  #val<-grkeval*penalty1
  val<-c(penalty1,grkeval)
  
  return(val)
}

obj_Income_mcga <- function(x){
  #x<-as.numeric(x<(-5))*(-5)+as.numeric(x>(5))*(5)+as.numeric(x>=(-5)&x<=5)*x
  x<-as.numeric(x<(-5))*runif(1,-5,0)+as.numeric(x>(5))*runif(1,0,5)+as.numeric(x>=(-5)&x<=5)*x
  x<-round(x)
  if(sum(as.numeric(round(x)!=0))==0){
    x<-runif(length(x),-5,5)
    x<-round(x)
  }

  print(x)
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-3; udlStepPct<-0.03
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  #print(posEvalTbl$pos)
  #print(posEvalTbl)

  sd_multp<-25;anlzd_sd<-0.2
  #weight is normalized
  weight<-dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct / 
    sum(dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct)
  #print(weight)
  
  pos_change<-sum(as.numeric((x-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  print(pos_change)
  print(penalty1) 

  grkeval<--sum(posEvalTbl$DTRRR*weight+posEvalTbl$VTRRR*weight)
  print(grkeval)
  
  val<-grkeval*penalty1
  print(val)
  return(val)
}

obj_Income_mcga_f1 <- function(x){
  x<-as.numeric(x<(-5))*(-5)+as.numeric(x>(5))*5+as.numeric(x>=(-5)&x<=5)*x
  if(sum(as.numeric(round(x)!=0))==0){
    x<-rep(1:length(x))
    x<-round(x)
  }
  pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  print(pos_change)
  print(penalty1) 
  return(penalty1)
}

obj_Income_mcga_f2 <- function(x){
  x<-as.numeric(x<(-5))*(-5)+as.numeric(x>(5))*5+as.numeric(x>=(-5)&x<=5)*x
  x<-round(x)
  if(sum(as.numeric(round(x)!=0))==0){
    x<-rep(1:length(x))
    x<-round(x)
  }
  print(x)
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-3; udlStepPct<-0.03
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  #return(posEvalTbl)
  
  sd_multp<-25;anlzd_sd<-0.2
  #weight is normalized
  weight<-dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct / 
    sum(dnorm(udlChgPct,mean=0,sd=(anlzd_sd/sqrt(252/sd_multp)))*udlStepPct)
 
  grkeval<--sum(posEvalTbl$DTRRR*weight+posEvalTbl$VTRRR*weight)
  print(grkeval)
  
  val<-grkeval
  return(val)
}

obj_Income_mcga_mf<-function(x){
  return ( c(obj_Income_mcga_f1(x),obj_Income_mcga_f2(x)) )
}

#initially evaluate using continuous value, after some point evaluated by
#INT round values. In both cases, pos_change should be evaluated by INT.

##
#Initial and evaluation vector
iniPos<-opchain$Position
iniPos<-rep(0,length(iniPos))
evaPos<-opchain$Position
evaPos<-rnorm(n=length(iniPos),mean=0,sd=1)

obj_Income(x=evaPos)
obj_Income_solnp(x=evaPos)

##
# Optimize Engine

#EDoptimR
edoprCon=function(x){
  x<-round(x)
  pos_change<-(sum(as.numeric((x-iniPos)!=0))-10)
  c(pos_change)
}
outjdopr<-JDEoptim(lower=rep(-5.2,length(iniPos)),upper=rep(5.2,length(iniPos)), fn=obj_Income,
                   tol=1e-10,NP=15*length(iniPos),maxiter=100*length(iniPos),
                   constr=edoprCon, meq = 0)
rm(edoprCon)
#JDEoptim(.., NP = 10*d, tol = 1e-15, maxiter = 200*d, trace = FALSE, triter = 1, details = FALSE, ...)

#MCGA
#mcga_chsize<-length(iniPos)
outm <- mcga( popsize=200,chsize=as.numeric(length(iniPos)),minval=-5,maxval=5,maxiter=2500,
              crossprob=1.0,mutateprob=0.01,evalFunc=obj_Income_mcga)
rm(mcga_chsize)

#genoud
domain<-matrix(c(rep(-5,length(evaPos)),rep(5,length(iniPos))), nrow=length(iniPos), ncol=2)
outgen <- genoud(fn=obj_Income_genoud_lex_int,nvars=length(iniPos),
                 pop.size=3000,max.generations=100,
                 data.type.int=TRUE,
                 lexical=TRUE,
                 wait.generations=30,gradient.check=FALSE,MemoryMatrix=TRUE,
                 starting.values=rnorm(n=length(iniPos),mean=0,sd=2),Domains=domain)
rm(domain)

#GenSA Intermediate not sufficient
GenSA(par=evaPos,fn=obj_Income,lower=rep(-5.2,length(iniPos)),upper=rep(5.2,length(iniPos)))

#hydroPSO Intermediate not sufficient
hydroPSO(par=evaPos,fn=obj_Income,
         lower=rep(-5.2,length(iniPos)), upper=rep(5.2,length(iniPos)))

#DEOptim Intermediate not sufficient
outdeop <- DEoptim(fn=obj_Income,lower = rep(-5.2,length(iniPos)),upper = rep(5.2,length(iniPos)))

#dfoptim. Intermediate not suffucient
outhjkb<-hjkb(par=evaPos, fn=obj_Income_mcga, lower = rep(-5.2,length(iniPos)), upper =rep(5.2,length(iniPos)))
outnmkb<-nmkb(par=evaPos, fn=obj_Income_mcga, lower = rep(-5.2,length(iniPos)), upper =rep(5.2,length(iniPos)))

#powell Intermediate not suffucient.
powell(par=evaPos,fn=obj_Income_mcga)

#solnp XXXX not suffucient
solnpIneqfn = function(x)
{
  x<-round(x)
  pos_change<-sum(as.numeric((x-iniPos)!=0))
  pos_change
}
solnpIneqLB = rep(0,1)
solnpIneqUB = rep(10,1)
solnpLB = rep(-5.2,length(iniPos))
solnpUB = rep(5.2,length(iniPos))

outsolnp<-solnp(pars=evaPos,fun=obj_Income_solnp,
                #distr=rnorm(n=length(iniPos),mean=0,sd=1),
                ineqfun=solnpIneqfn,ineqLB=solnpIneqLB,ineqUB=solnpIneqUB,
                LB=solnpLB,UB=solnpUB)

rm(solnpIneqfn,solnpIneqLB,solnpIneqUB,solnpLB,solnpUB)
rm(amlzd_sd)

#nleqslv doesn't work
#nleqslv(x=evaPos,fn=obj_Income_mcga)

#optim XXX not sufficient
optim(par=evaPos, f=obj_Income,
      #method ="SANN",
      lower = rep(-5.2,length(iniPos)), upper =rep(5.2,length(iniPos)))
     # control = list(), hessian = FALSE, ...)
