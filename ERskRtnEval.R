library(dplyr)
library(RQuantLib)
library(DEoptim)
library(rgenoud)
library(Rsolnp)
library(DEoptimR)
library(mcga)
library(GenSA)
library(powell)
library(hydroPSO)
library(nleqslv)
library(dfoptim)
library(GA)
library(Rmalschains)
library(soma)
library(nloptr)
library(NMOF)

#Variables -----------
#Holding Period
#holdDays<-3*252/365 #Trading Days. This should be correct.
holdDays<-3
#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays<-20
#Multipler of Position
PosMultip<-100

#Option Chain and Position Data. Here we use UDL_Positions_Pre ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",")
#filtering. deleting unnecessary column
opchain %>% dplyr::select(-(contains('Frac',ignore.case=TRUE)),
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

#Functions for gruped or rowwise operations. -------------
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
  regression<-get.Volatility.Level.Regression(Days=days)
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
  #posEvalTbl %>% rowwise() %>% do(DTRRR=evalPosRskRtnDTRRR(.)) -> tmp
  #unlist(tmp$DTRRR)->tmp ; posEvalTbl$DTRRR <- tmp ;rm(tmp)
  #VTRRR
  #posEvalTbl %>% rowwise() %>% do(VTRRR=evalPosRskRtnVTRRR(.)) -> tmp
  #unlist(tmp$VTRRR)->tmp ; posEvalTbl$VTRRR <- tmp ;rm(tmp)
  
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

#posgrks can be obtained by hollowNonZeroPosition(evaPos)
#return vector. If you like to get aggrigate payoff, use sum()
getIntrisicValue<-function(udly_price,position,multip=PosMultip){
  as.numeric(((udly_price-position$Strike)*(-position$TYPE)>0))*
    (udly_price-position$Strike)*(-position$TYPE)*multip*position$Position
}

## Optimization Test -------------------

# initial polulation functions --------
test_initial_create<-function(){
  for(k in 1:100){
    val<-0
    for(i in 1:100){
     val<-val+sum(as.numeric(round(rnorm(n=length(iniPos),mean=0,sd=0.01*k)))!=0)
   }
   cat(" sd:",0.01*k);cat(" val:",val/100)
  }
}

test_initial_polulation<-function(fname){
  for(k in 1:10000){
    x<-round(rnorm(n=length(iniPos),mean=0,sd=0.44))
    x<-x*2
    val<-obj_Income(x,isDebug=FALSE)
    if(val<1000){
      #wite.csv(x,init_cand_file,quote=FALSE,row.names=FALSE,append=T)
      #write(x,init_cand_file, ncolumns=length(iniPos))
      cat(x,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
      cat(val,file=fname,"\n",append=TRUE)
    }
  }
}

create_initial_polulation<-function(popnum,thresh=1000,sd=0.44){
  added_num<-0
  while(TRUE){
    x<-round(rnorm(n=length(iniPos),mean=0,sd=sd))
    x<-x*2
    val<-obj_Income(x,isDebug=TRUE)
    if(val<thresh){
     if(added_num==0){
       ret_val<-x
     }else{
       ret_val<-c(ret_val,x)
     }
     added_num<-added_num+1
    }
    cat(" added num:",added_num,"\n")
    if(added_num==popnum)
      break
  }
  ret_val
}

#Initial and evaluation vector ----------
iniPos<-opchain$Position
iniPos<-rep(0,length(iniPos))
evaPos<-opchain$Position

#create initial populations 
init_cand_file<-paste(".\\ResultData\\initcand",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".csv",sep="")
test_initial_polulation(fname=init_cand_file)

rm(init_cand_file)

#test sample value -----------
evaPos<-rnorm(n=length(iniPos),mean=0,sd=1)
obj_Income(x=evaPos,isDebug=TRUE)
obj_Income_Cont(x=evaPos,isDebug=TRUE)
obj_Income_genoud_lex_int(x=evaPos,isDebug=TRUE)

#functions optimized  -------------

obj_Income <- function(x,isDebug=TRUE,isMCGA=FALSE,isGenoud=FALSE){
  if(isMCGA){
    x<-as.numeric(x<(-6.5))*(-6)+as.numeric(x>(6.5))*6+as.numeric(x>=(-6.5)&x<=6.5)*x
  }
  if(!isGenoud){
    x<-round(x)
  }
  if(sum(as.numeric(round(x)!=0))==0){
    #x<-rep(1,length=length(x))
    x<-rnorm(n=length(iniPos),mean=0,sd=1)
    x<-round(x)
  }
  exp_c<-0
  
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-4; udlStepPct<-0.02
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip)
  #if(isDebug){print(posEvalTbl$pos)}
  #if(isDebug){print(posEvalTbl)}
  
  ##
  # penalty1: position total num
  pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  if(isDebug){cat(x," ");cat("pos num",pos_change)}
  if(isDebug){cat(" :p1",penalty1)}
  
  ##
  # penalty4. ThetaEffect. This should be soft constraint
  
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*2
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  theta_ttl<-thePositionGrk$ThetaEffect+sum(posEvalTbl$ThetaEffect*weight)
  
  exp_c<-as.numeric((pos_change-10)>0)*0+as.numeric((pos_change-10)<=0)
  penalty4<-(1+as.numeric(theta_ttl<0)*(abs(theta_ttl)))^exp_c
  ##
  #cost4 <- -1*theta_ttl;cost4<-0
  if(isDebug){cat(" :thta_ttl",theta_ttl);cat(" :p4",penalty4)}
  #cat(" :thta_ini",thePositionGrk$ThetaEffect);cat(" :thta_hld",sum(posEvalTbl$ThetaEffect*weight))
  
  ##
  # penalty2 tail-risk
  tail_rate<-0.4
  tailPrice<-min(sum(getIntrisicValue(position$UDLY[1]*(1-tail_rate),position)),
                 sum(getIntrisicValue(position$UDLY[1]*(1+tail_rate),position)))
  #lossLimitPrice <- -1*position$UDLY[1]*PosMultip*(tail_rate+0.1)
  lossLimitPrice <- -20000
  
  #exp_c<-as.numeric(exp_c>0)*(as.numeric((penalty4-2)>0)*0+as.numeric((penalty4-2)<0)*2)
  #penalty2<-(1+as.numeric((tailPrice-lossLimitPrice)<0)*(abs(tailPrice)))^exp_c
  penalty2<-(1+as.numeric((tailPrice-lossLimitPrice)<0)*(10))^exp_c
  
  ##
  #cost2<-(1+as.numeric(penalty2>1)*10);cost2<-0
  if(isDebug){cat(" :tlpr",tailPrice);cat(" :lslmt",lossLimitPrice);cat(" :p2",penalty2)}
  
  ##
  # penalty3, cost1 profit must be positive. also must be a cost term.
  #if(isDebug){cat(" :prc_hd",posEvalTbl$Price);cat(" :prc_ini:",getPositionGreeks(position,multi=PosMultip)$Price)}
  #if(isDebug){cat(" :prft",posEvalTbl$Price-getPositionGreeks(position,multi=PosMultip)$Price)}
  
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht",weight)}
  
  #exp_c<-as.numeric(exp_c>0)*(as.numeric((penalty3-2)>0)*0+as.numeric((penalty3-2)<0)*2)
  profit_hdays<-sum((posEvalTbl$Price-thePositionGrk$Price)*weight)
  if(isDebug){cat(" :prft_wt",profit_hdays)}
  
  penalty3<-(1+as.numeric(profit_hdays<0)*(abs(profit_hdays)))^exp_c
  penalty3<-1
  if(isDebug){cat(" :p3",penalty3)}
  ##
  # cost 3
  cost3<- -1*profit_hdays
  
  ##
  # cost5 Each Effects.
  #weight is normalized
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*3
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht2",weight)}
  cost5<- -sum((posEvalTbl$DeltaEffect+posEvalTbl$GammaEffect+
                  posEvalTbl$VegaEffect+posEvalTbl$ThetaEffect)*weight)
  if(isDebug){cat(" c5",cost5)}
  
  ##
  # total cost is weighted sum of each cost.
  cost<-(0.05*cost3+0.01*cost5+500)
  if(isDebug){cat(" :cost",cost," ")}
  
  ##
  # total cost and penalty
  val<-cost*penalty1*penalty2*penalty3*penalty4
  
  if(isDebug){cat(" val:",val,"\n")}
  
  if(isDebug){
    if(val<best_result || val<495){
      write(x,result_file,append=T)
      write(val,result_file,append=T)
      #ou use cat which gives further details on the format used
      if(val<best_result){
        best_result<<-val
      }
      #assign(best_result,val,env=.GlobalEnv)
    }
  }
  return(val)
}

obj_Income_Cont <- function(x,isDebug=TRUE,isGenoud=TRUE){
  x<-as.numeric(x<(-6.5))*(-6)+as.numeric(x>(6.5))*6+as.numeric(x>=(-6.5)&x<=6.5)*x
  
  if(sum(as.numeric(x!=0))==0){
    x<-rnorm(n=length(iniPos),mean=0,sd=1)
  }
  
  cat(x," ")
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-4; udlStepPct<-0.02
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip)
  #if(isDebug){print(posEvalTbl$pos)}
  #if(isDebug){print(posEvalTbl)}
  
  ##
  # penalty1 cost1: position total num
#   pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
#   penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))
#   cost1<-(penalty1-1)*3
#   
#   if(isDebug){cat(" :pos num",pos_change);cat(" :cost1",cost1)}

  ##
  # penalty4. ThetaEffect. This should be soft constraint
  
  #   sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*2
  #   weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #   theta_ttl<-thePositionGrk$ThetaEffect+sum(posEvalTbl$ThetaEffect*weight)
  #   
  #   exp_c<-as.numeric((pos_change-10)>0)*0+as.numeric((pos_change-10)<=0)
  #   penalty4<-(1+as.numeric(theta_ttl<0)*(abs(theta_ttl)))^exp_c
  #   
  #   if(isDebug){cat(" :thta_ttl",theta_ttl);cat(" :p4",penalty4)}

  ##
  # penalty2 tail-risk
  tail_rate<-0.4
  tailPrice<-min(sum(getIntrisicValue(position$UDLY[1]*(1-tail_rate),position)),
                 sum(getIntrisicValue(position$UDLY[1]*(1+tail_rate),position)))
  #lossLimitPrice <- -1*position$UDLY[1]*PosMultip*(tail_rate+0.1)
  lossLimitPrice <- -20000
  exp_c<-1
  penalty2<-(1+as.numeric((tailPrice-lossLimitPrice)<0)*(10))^exp_c

  # cost 2
  cost2<- -1*as.numeric(penalty2>1)*(tailPrice-lossLimitPrice)
  if(isDebug){cat(" :tlpr",tailPrice);cat(" :lslmt",lossLimitPrice);cat(" p2:",penalty2);cat(" :c2",cost2)}
  
  ##
  # penalty3, cost1 profit must be positive. also must be a cost term.
 
  #if(isDebug){cat(" :prc_hd",posEvalTbl$Price);cat(" :prc_ini:",getPositionGreeks(position,multi=PosMultip)$Price)}
  #if(isDebug){cat(" :prft",posEvalTbl$Price-getPositionGreeks(position,multi=PosMultip)$Price)}
  
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht",weight)}
  
  #exp_c<-as.numeric(exp_c>0)*(as.numeric((penalty3-2)>0)*0+as.numeric((penalty3-2)<0)*2)
  profit_hdays<-sum((posEvalTbl$Price-thePositionGrk$Price)*weight)
  
  if(isDebug){cat(" :prft_wt",profit_hdays)}  
  #   penalty3<-(1+as.numeric(profit_hdays<0)*(abs(profit_hdays)))^exp_c
  #   penalty3<-1
  #  if(isDebug){cat(" :p3",penalty3)}
  ##

  # cost 3
  cost3<- -1*profit_hdays
  if(isDebug){cat(" :c3",cost3)}

  ##
  # cost5 Each Effects.
  #weight is normalized
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*3
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht2",weight)}
 
  theta_addwht<-1.5
  cost5<- -sum((posEvalTbl$DeltaEffect+posEvalTbl$GammaEffect+
                  posEvalTbl$VegaEffect+theta_addwht*posEvalTbl$ThetaEffect)*weight)
  if(isDebug){cat(" tef:",sum(posEvalTbl$ThetaEffect));cat(" c5",cost5)}
  
  ##
  # total cost is weighted sum of each cost.
  cost<-(0.05*cost3+0.01*cost5+cost2+500)
  if(isDebug){cat(" :cost",cost," ")}
  
  ##
  # total cost and penalty
  #val<-cost*penalty1*penalty2*penalty3*penalty4
  val<-cost

  if(isDebug){cat(" val:",val,"\n")}
  
  if(isDebug){
    if(val<best_result){
      write(x,result_file,append=T)
      write(val,result_file,append=T)
      #ou use cat which gives further details on the format used
      best_result<<-val
      #assign(best_result,val,env=.GlobalEnv)
    }
  }
  return(val)
}

obj_Income_genoud_lex_int <- function(x,isDebug=TRUE){
  #x<-round(x)
  if(sum(as.numeric(round(x)!=0))==0){
    x<-rep(1,length=length(x))
    x<-round(x)
  }
  cat(x,"\n")
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-3; udlStepPct<-0.03
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  
  #if(isDebug){print(posEvalTbl$pos)}
  #if(isDebug){print(posEvalTbl)}
  
  #penalty1: position total num
  pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  cat("pos num",pos_change)
  if(isDebug){cat(" :p1",penalty1)}
  
  #penalty2 tail-risk
  tail_rate<-0.5
  tailPrice<-min(sum(getIntrisicValue(position$UDLY[1]*(1-tail_rate),position)),
      sum(getIntrisicValue(position$UDLY[1]*(1+tail_rate),position)))
  lossLimitPrice <- -1*position$UDLY[1]*PosMultip*(tail_rate+0.1)
  penalty2<-(1+as.numeric((tailPrice-lossLimitPrice)<0)*(abs(tailPrice)))
  if(isDebug){cat(" :tlpr",tailPrice);cat(" :lslmt",lossLimitPrice);cat(" :p2",penalty2)}

  #penalty3, cost1 profit must be positive. also must be a cost term.
  #if(isDebug){cat(" :prc_hd",posEvalTbl$Price);cat(" :prc_ini:",getPositionGreeks(position,multi=PosMultip)$Price)}
  #if(isDebug){cat(" :prft",posEvalTbl$Price-getPositionGreeks(position,multi=PosMultip)$Price)}
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip)
 
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht",weight)}

  profit_hdays<-sum((posEvalTbl$Price-thePositionGrk$Price)*weight)
  if(isDebug){cat(" :prft_wt",profit_hdays)}
  penalty3<-(1+as.numeric(profit_hdays<0)*(abs(profit_hdays)))^2
  if(isDebug){cat(" :p3",penalty3)}
  cost1<- -1*profit_hdays
  
  #cost2 Each Effects.  
  #weight is normalized
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*3
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht2",weight)}  
  cost2<--sum(posEvalTbl$DTRRR*weight+posEvalTbl$VTRRR*weight)
  if(isDebug){cat(" c2",cost2)}
  
  #penalty4. ThetaEffect. This should be soft constraint
  #print(thePositionGrk)
  theta_ttl<-thePositionGrk$ThetaEffect+sum(posEvalTbl$ThetaEffect*weight)
  penalty4<-(1+as.numeric(theta_ttl<0)*(abs(theta_ttl)))^2
  if(isDebug){cat(" :thta_ttl",theta_ttl);cat(" :p4",penalty4)}
  #cat(" :thta_ini",thePositionGrk$ThetaEffect);cat(" :thta_hld",sum(posEvalTbl$ThetaEffect*weight))
  
  #total cost is weighted sum of each cost.
  cost<-(0.03*cost1+cost2+15)*penalty4
  if(isDebug){cat(" :cost",cost," ")}
  
  #non lex cost function should be like this.
  #val<-grkeval*penalty1*penalty2*penalty3
  val<-c(penalty1,penalty2,penalty3,cost)
  
  if(isDebug){cat(" val:",val,"\n")}
  return(val)
}


#initially evaluate using continuous value, after some point evaluated by
#INT round values. In both cases, pos_change should be evaluated by INT.

##

# Optimize Engine  
#EDoptimR  =======
deoptR_inipop_vec<-create_initial_polulation(popnum=length(evaPos)*5,thresh=1000)
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isGenoud=FALSE ; isMCGA=FALSE
edoprCon=function(x){
  x<-round(x)
  pos_change<-(sum(as.numeric((x-iniPos)!=0))-10)
  c(pos_change)
}
outjdopr<-JDEoptim(lower=rep(-6,length(iniPos)),upper=rep(6,length(iniPos)), fn=obj_Income,
                   tol=1e-10,NP=0,#10*length(iniPos),
                   maxiter=1*length(iniPos),#50*length(iniPos),
                   add_to_init_pop=matrix(deoptR_inipop_vec,#rep(evaPos,times=10*length(evaPos)),
                                          nrow=length(iniPos),ncol=5*length(iniPos)),
                   constr=edoprCon, meq = 0)
rm(deoptR_inipop_vec,edoprCon)
#JDEoptim(.., NP = 10*d, tol = 1e-15, maxiter = 200*d, trace = FALSE, triter = 1, details = FALSE, ...)

#genoud ========
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isGenoud=TRUE ; isMCGA=FALSE
domain<-matrix(c(rep(-6,length(evaPos)),rep(6,length(iniPos))), nrow=length(iniPos), ncol=2)
outgen <- genoud(#fn=obj_Income_genoud_lex_int,lexical=TRUE,
                 fn=obj_Income,
                 nvars=length(iniPos),pop.size=1000,max.generations=100,
                 data.type.int=TRUE,#FALSE
                 wait.generations=20,gradient.check=FALSE,MemoryMatrix=TRUE,
                 boundary.enforcement=2,
                 starting.values=evaPos,#rnorm(n=length(iniPos),mean=0,sd=2),
                 Domains=domain)

result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-500
domain<-matrix(c(rep(-6.49,length(evaPos)),rep(6.49,length(iniPos))), nrow=length(iniPos), ncol=2)
#isGenoud=TRUE ; isMCGA=FALSE
outgen <- genoud(fn=obj_Income_Cont,
                 nvars=length(iniPos),pop.size=1000,max.generations=150,
                 wait.generations=20,gradient.check=FALSE,MemoryMatrix=TRUE,
                 boundary.enforcement=2,
                 starting.values=rnorm(n=length(iniPos),mean=0,sd=2),
                 Domains=domain)
rm(domain)

#hydroPSO ======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-600
outhdpso<-hydroPSO(par=rnorm(n=length(iniPos),mean=0,sd=2),#as.numeric(evaPos),
                   fn=obj_Income_Cont,#obj_Income,
                   lower=rep(-6,length(evaPos)), upper=rep(6,length(evaPos)))

#dfoptim  =======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
outhjkb<-hjkb(par=evaPos, #rnorm(n=length(iniPos),mean=0,sd=2),
              fn=obj_Income_Cont, lower = rep(-6,length(iniPos)), upper =rep(6,length(iniPos)))
outnmkb<-nmkb(par=rnorm(n=length(iniPos),mean=0,sd=1),#norm(n=length(iniPos),mean=0,sd=1)
              fn=obj_Income_Cont, lower = rep(-6,length(iniPos)), upper =rep(6,length(iniPos)))

#nloptr sbplx ======= 
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
outnloptr <-sbplx(rnorm(n=length(iniPos),mean=0,sd=2),#as.numeric(evaPos),
                  obj_Income_Cont, lower = rep(-6,length(iniPos)), upper = rep(6,length(iniPos)))

#nloptr direct/directL ======= 
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
nl.opts(list(xtol_rel = 1e-8, maxeval = 3000,check_derivatives = FALSE)) 
outnloptr <-directL(obj_Income_Cont,
                    #scaled=FALSE, #direct
                    randomized=TRUE, #directL
                    lower = rep(-6,length(iniPos)), upper = rep(6,length(iniPos)))
#nloptr newuoa,auglag======= 
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
outnloptr <-newuoa(as.numeric(evaPos),#rnorm(n=length(iniPos),mean=0,sd=2),#
                  obj_Income_Cont, lower = rep(-6,length(iniPos)), upper = rep(6,length(iniPos)))
#newuoa(,oGrad Fast:need good x0) ,auglag(,oGrad Fast),crs2lm(m,x) mlsl(x,) stogo(,x) nl.grad()
#nl.jacobian() isres(x,x) lbfgs(x,x) mma(x,) neldermead(x,),nloptr(,x) slsqp(,x) tnewton(,x) varmetric(,mGrad) 
#malsch  ==============
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isMCGA=TRUE ;isGenoud=FALSE
outmalsch<-malschains(fn=obj_Income, lower=rep(-6,length(iniPos)), upper=rep(6,length(iniPos)))
#malschains(fn, lower, upper, dim,maxEvals = 10 * control$istep, verbosity = 2,
#           initialpop = NULL, control = malschains.control(),seed = NULL, env)

#MCGA ========
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isMCGA=TRUE ;isGenoud=FALSE
outm <- mcga( popsize=200,chsize=as.numeric(length(iniPos)),minval=-6,maxval=6,maxiter=300,
              crossprob=1.0,mutateprob=0.01,evalFunc=obj_Income)
#GenSA Intermediate not sufficient =======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-500
#isGenoud=FALSE ; isMCGA=FALSE
outgensa<-GenSA(par=as.numeric(evaPos),fn=obj_Income,lower=rep(-6.1,length(iniPos)),upper=rep(6.1,length(iniPos)))
#DEOptim Intermediate not sufficient  =======
deopt_inipop_vec<-create_initial_polulation(popnum=10*length(evaPos),thresh=10000)
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-550
outdeop <- DEoptim(fn=obj_Income,lower = rep(-6,length(iniPos)),upper = rep(6,length(iniPos)),
                   control=DEoptim.control(itermax = 200,strategy=2,
                                           initialpop=matrix(deopt_inipop_vec,#rep(evaPos,times=10*length(evaPos)),
                                                             nrow=10*length(evaPos),ncol=length(evaPos),byrow=T)))
rm(deopt_inipop_vec)

#powell Intermediate not suffucient.  ======
powell(par=evaPos,fn=obj_Income_mcga)
#soma(m,x)  ==============
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isMCGA=TRUE ;isGenoud=FALSE
outsoma <- soma(obj_Income_Cont, list(min=rep(-6,length(iniPos)),max=rep(6,length(iniPos))))
#NMOF(DeOpt) =========
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isMCGA=FALSE ;isGenoud=TRUE
DEopt(OF=obj_Income,algo=list(nP=500L,nG=150L,F=0.6,CR=0.9,min=rep(-6,length(iniPos)),max=rep(6,length(iniPos))))
#GA  ==============
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isMCGA=TRUE ;isGenoud=FALSE
#Maximization of a fitness function using genetic algorithms
GAobj_Income<-function(x){ val<-(-1)*obj_Income(x) ; cat(" :Gaobj",val," ") ; return(val) }
outGA <- ga(type="real-valued",fitness=GAobj_Income,min=rep(-6,length(iniPos)),max=rep(6,length(iniPos)))
#solnp XXXX not suffucient =======
solnpIneqfn = function(x)
{
  x<-round(x)
  pos_change<-sum(as.numeric((x-iniPos)!=0))
  pos_change
}
solnpIneqLB = rep(0,1)
solnpIneqUB = rep(10,1)
solnpLB = rep(-6,length(iniPos))
solnpUB = rep(6,length(iniPos))

outsolnp<-solnp(pars=evaPos,fun=obj_Income,
                #distr=rnorm(n=length(iniPos),mean=0,sd=1),
                ineqfun=solnpIneqfn,ineqLB=solnpIneqLB,ineqUB=solnpIneqUB,
                LB=solnpLB,UB=solnpUB)

rm(solnpIneqfn,solnpIneqLB,solnpIneqUB,solnpLB,solnpUB)
rm(amlzd_sd)
#nleqslv doesn't work =======
#nleqslv(x=evaPos,fn=obj_Income_mcga)
#optim XXX not sufficient  ========
optim(par=evaPos, f=obj_Income,
      #method ="SANN",
      lower = rep(-6,length(iniPos)), upper =rep(6,length(iniPos)))
     # control = list(), hessian = FALSE, ...)
