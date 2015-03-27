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
  for(k in 1:200){
    val<-0
    for(i in 1:300){
     val<-val+sum(as.numeric(round(rnorm(n=length(iniPos),mean=0,sd=0.005*k)))!=0)
   }
   cat(" sd:",0.005*k);cat(" val:",val/300)
  }
}


test_initial_PutCall_create<-function(type,putPos=6,callPos=2){
  #Put
  put_length<-sum(as.numeric(type==OpType_Put_G))
  cat(" Put***** l:",put_length)
  for(k in 1:200){
    val<-0
    for(i in 1:300){
      val<-val+sum(as.numeric(round(rnorm(n=put_length,mean=0,sd=0.005*k)))!=0)
    }
    if(round(val/300)==putPos){ cat(" sd:",0.005*k);cat(" val:",val/300) }
  }
  cat("\n")
  #Call
  call_length<-sum(as.numeric(type==OpType_Call_G))
  cat(" Call***** l:",call_length)
  for(k in 1:200){
    val<-0
    for(i in 1:300){
      val<-val+sum(as.numeric(round(rnorm(n=call_length,mean=0,sd=0.005*k)))!=0)
    }
    if(round(val/300)==callPos) { cat(" sd:",0.005*k);cat(" val:",val/300) }
  }
}


test_initial_population<-function(fname,thresh=1000,sd=0.44){
  for(k in 1:10000){
    x<-round(rnorm(n=length(iniPos),mean=0,sd=sd))
    x<-x*2
    val<-obj_Income(x,isDebug=FALSE)
    if(val<thresh){
      cat(x,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
      cat(val,file=fname,"\n",append=TRUE)
    }
  }
}

create_initial_polulation<-function(popnum,thresh=1000,sd=0.44,ml=2,fname,isFileout=FALSE) {
  added_num<-0
  total_count<-0
  while(TRUE){
    x<-round(rnorm(n=length(iniPos),mean=0,sd=sd))
    x<-x*ml
    if(sum(as.numeric((round(x)-iniPos)!=0))>10){
      total_count<-total_count+1
      next
    }
    if(sum(x)!=0){
      total_count<-total_count+1
      next
    }
    val<-obj_Income(x,isDebug=TRUE)
    if(val<thresh){
     if(added_num==0){
       ret_val<-x
     }else{
       ret_val<-c(ret_val,x)
     }
     added_num<-added_num+1
     if(isFileout){
       cat(x,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
       cat(val,file=fname,"\n",append=TRUE)
     }
    }
    total_count<-total_count+1
    cat(" added num:",added_num,"total count:",total_count,"\n")
    if(added_num==popnum)
      break
  }
  ret_val
}

create_initial_PutCall_polulation<-function(popnum,type,thresh=1000,putsd=0.425,putn=6,callsd=0.405,calln=6,ml=2,fname,isFileout=FALSE){
  added_num<-0
  total_count<-0
  while(TRUE){
    x<-rep(0,times=length(iniPos))
    #Put
    y<-as.numeric(type==OpType_Put_G)*round(rnorm(n=length(iniPos),mean=0,sd=putsd))
    if(sum(as.numeric((y-iniPos)!=0))>putn){
      total_count<-total_count+1
      next
    }
    if(sum(y)!=0){
      total_count<-total_count+1
      next
    }
    cat(" (:y",y,")")
    #Call
    while(TRUE) {
      z<-as.numeric(type==OpType_Call_G)*round(rnorm(n=length(iniPos),mean=0,sd=callsd))
      if(sum(as.numeric((z-iniPos)!=0))>calln){
        total_count<-total_count+1
        next
      }
      if(sum(z)!=0){
        total_count<-total_count+1
        next
      }
      break
    }
    cat(" (:z",z,") :x(y+z) ")
    x<-y+z
    x<-x*ml
    #cat(" :x",x)
    val<-obj_Income(x,isDebug=TRUE)
    if(val<thresh){
      if(added_num==0){
        ret_val<-x
      }else{
        ret_val<-c(ret_val,x)
      }
      added_num<-added_num+1
      if(isFileout){
        cat(x,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
        cat(val,file=fname,"\n",append=TRUE)
      }
    }
    total_count<-total_count+1
    cat(" added num:",added_num,"total count:",total_count,"\n")
    if(added_num==popnum)
      break
  }
  ret_val
}

create_initial_exact_PutCall_polulation<-function(popnum,type,thresh=1000,putn=6,calln=6,ml=2,fname,isFileout=FALSE,isDebug=FALSE){
  added_num<-0
  total_count<-0
  while(TRUE){
    #Put   
    idxy<-as.numeric(type==OpType_Put_G)*rep(1:length(iniPos),length=length(iniPos))
    if(isDebug){ cat(" put pos:",idxy) }
    idxy<-idxy[idxy!=0]
    if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=putn,replace=FALSE,prob=NULL)
    if(isDebug){ cat(" put idxy:",idxy) }
    y<-rep(0,times=length(iniPos))
    y[idxy[1:(putn/2)]]<-1
    y[idxy[(putn/2+1):putn]]<-(-1)
    #Call
    idxy<-as.numeric(type==OpType_Call_G)*rep(1:length(iniPos),length=length(iniPos))
    idxy<-idxy[idxy!=0]
    idxy<-sample(idxy,size=calln,replace=FALSE,prob=NULL)
    if(isDebug){ cat(" call idxy:",idxy) }
    z<-rep(0,times=length(iniPos))
    z[idxy[1:(calln/2)]]<-1
    z[idxy[(calln/2+1):putn]]<-(-1)
    if(isDebug){ cat(" (:y",y,")") }
    if(isDebug){ cat(" (:z",z,") :x(y+z) ") }
    x<-y+z
    x<-x*ml
    val<-obj_Income(x,isDebug=TRUE)
    if(val<thresh){
      if(added_num==0){
        ret_val<-x
      }else{
        ret_val<-c(ret_val,x)
      }
      added_num<-added_num+1
      if(isFileout){
        cat(x,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
        cat(val,file=fname,"\n",append=TRUE)
      }
    }
    total_count<-total_count+1
    if(isDebug){cat(" added num:",added_num,"total count:",total_count,"\n")}
    if(added_num==popnum)
      break
  }
  cat(" added num:",added_num,"total count:",total_count,"\n")
  ret_val
}

#create one candidate pools
createCombineCandidatePool<-function(fname,pnum=1000,nrows=-1,skip=0,method=1){
  pool<-read.csv(fname, header=FALSE,nrows=nrows,skip=skip)
  pool %>% dplyr::arrange(pool[,length(pool)]) %>% dplyr::distinct() -> pool
  
  #select specific nums. some optional methods
  #1.top n
  if(method==1){
    pool<-pool[1:pnum,]
    return(pool)
  }
  #2. random sample
  else if(method==2){
    idx<-rep(1:nrow(pool),length=nrow(pool))
    idx<-sort(sample(idx,size=pnum,replace=FALSE,prob=NULL))
    pool<-pool[idx,]
    rownames(pool) <- c(1:nrow(pool))
    return(pool)
  }
  #3. bottom n
  else if(method==3){
    pool<-pool[(nrow(pool)-pnum+1):nrow(pool),]
    rownames(pool) <- c(1:nrow(pool))
    return(pool)
  }  
  return(pool)
}

#create initial population ---------

for(tmp in 1:10){
  create_initial_polulation(popnum=300,thresh=1000,sd=0.48,ml=2,
                            fname=paste(".\\ResultData\\inipop-0.48-1000-",format(Sys.time(),"%Y-%b-%d-%H"),".txt",sep=""),
                            isFileout=TRUE)
    create_initial_polulation(popnum=300,thresh=1000,sd=0.44,ml=2,
                            fname=paste(".\\ResultData\\inipop-0.44-1000-",format(Sys.time(),"%Y-%b-%d-%H"),".txt",sep=""),
                            isFileout=TRUE)
    create_initial_polulation(popnum=300,thresh=800,sd=0.37,ml=2,
                            fname=paste(".\\ResultData\\inipop-0.37-1000-",format(Sys.time(),"%Y-%b-%d-%H"),".txt",sep=""),
                            isFileout=TRUE)
    create_initial_polulation(popnum=300,thresh=700,sd=0.33,ml=2,
                            fname=paste(".\\ResultData\\inipop-0.34-1000-",format(Sys.time(),"%Y-%b-%d-%H"),".txt",sep=""),
                            isFileout=TRUE)
    create_initial_polulation(popnum=300,thresh=400,sd=0.27,ml=2,
                            fname=paste(".\\ResultData\\inipop-0.27-1000-",format(Sys.time(),"%Y-%b-%d"),".txt",sep=""),  
                            # fname=paste(".\\ResultData\\inipop-0.27-1000-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep=""),
                            isFileout=TRUE)
};rm(tmp)

#Put Call Separate Test.

for(tmp in 1:15){
  #Put 8 Call 2
  create_initial_PutCall_polulation(popnum=300,type=opchain$TYPE,thresh=1000,putsd=0.505,putn=8,callsd=0.405,calln=2,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P8C2-1000.txt",sep=""),
                                    isFileout=TRUE)
  #Put 6 Call 4
  create_initial_PutCall_polulation(popnum=300,type=opchain$TYPE,thresh=1000,putsd=0.425,putn=6,callsd=0.64,calln=4,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P6C4-1000.txt",sep=""),
                                    isFileout=TRUE)
  #Put 8 Call 0
  create_initial_PutCall_polulation(popnum=300,type=opchain$TYPE,thresh=1000,putsd=0.505,putn=8,callsd=0.005,calln=0,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P8C0-1000.txt",sep=""),
                                    isFileout=TRUE)
  #Put 6 Call 2
  create_initial_PutCall_polulation(popnum=300,type=opchain$TYPE,thresh=1000,putsd=0.425,putn=6,callsd=0.405,calln=2,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P6C2-1000.txt",sep=""),
                                    isFileout=TRUE)
  #Put 4 Call 4
  create_initial_PutCall_polulation(popnum=300,type=opchain$TYPE,thresh=1000,putsd=0.355,putn=4,callsd=0.64,calln=4,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P4C4-1000.txt",sep=""),
                                    isFileout=TRUE)
  #Put 6 Call 0
  create_initial_PutCall_polulation(popnum=300,type=opchain$TYPE,thresh=1000,putsd=0.425,putn=6,callsd=0.005,calln=0,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P6C0-1000.txt",sep=""),
                                    isFileout=TRUE)
  #Put 4 Call 2
  create_initial_PutCall_polulation(popnum=300,type=opchain$TYPE,thresh=1000,putsd=0.355,putn=4,callsd=0.405,calln=2,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P4C2-1000.txt",sep=""),
                                    isFileout=TRUE)
  #Put 4 Call 0
  create_initial_PutCall_polulation(popnum=200,type=opchain$TYPE,thresh=1000,putsd=0.355,putn=4,callsd=0.005,calln=0,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P4C0-1000.txt",sep=""),
                                    isFileout=TRUE)
  #Put 2 Call 2
  create_initial_PutCall_polulation(popnum=200,type=opchain$TYPE,thresh=1000,putsd=0.29,putn=2,callsd=0.405,calln=2,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P2C2-1000.txt",sep=""),
                                    isFileout=TRUE)
  #Put 2 Call 0
  create_initial_PutCall_polulation(popnum=100,type=opchain$TYPE,thresh=1000,putsd=0.29,putn=2,callsd=0.005,calln=0,ml=2,
                                    fname=paste(".\\ResultData\\inipop-",format(Sys.time(),"%Y-%b-%d-%Hh"),"-P2C0-1000.txt",sep=""),
                                    isFileout=TRUE)
};rm(tmp)
#Put Call Exact Separate Test

for(tmp in 1:15){
  create_initial_exact_PutCall_polulation(popnum=300,opchain$TYPE,thresh=1000,putn=8,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-Exc-1000-08P8C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE)
  create_initial_exact_PutCall_polulation(popnum=300,opchain$TYPE,thresh=1000,putn=6,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-Exc-1000-08P6C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE)
  create_initial_exact_PutCall_polulation(popnum=200,opchain$TYPE,thresh=1000,putn=6,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-Exc-1000-06P6C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE)
  create_initial_exact_PutCall_polulation(popnum=200,opchain$TYPE,thresh=1000,putn=4,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-Exc-1000-06P4C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE)
  create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,thresh=1000,putn=4,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-Exc-1000-04P4C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE)
  create_initial_exact_PutCall_polulation(popnum=80,opchain$TYPE,thresh=1000,putn=2,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-Exc-1000-04P2C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE)
  create_initial_exact_PutCall_polulation(popnum=50,opchain$TYPE,thresh=1000,putn=2,calln=0,ml=2,
                                          fname=paste(".\\ResultData\\inipop-Exc-1000-02P2C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE)
  create_initial_exact_PutCall_polulation(popnum=300,opchain$TYPE,thresh=1000,putn=8,calln=2,ml=2,
                                          fname=paste(".\\ResultData\\inipop-Exc-1000-10P8C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE)
  create_initial_exact_PutCall_polulation(popnum=300,opchain$TYPE,thresh=1000,putn=6,calln=4,ml=2,
                                          fname=paste(".\\ResultData\\inipop-Exc-1000-10P6C4-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          isFileout=TRUE,isDebug=FALSE)
};rm(tmp)

#create combined candidate populations

#create initial pools for combined search ----------------------
tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-1000-10P8C2-2015-3-24.csv",sep=""),
                                pnum=700,nrows=-1,skip=0,method=1)
pools<-list(list(c(10,8,2),tmp)) #No.[[1]]
poolidx<-2
tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-1000-10P6C4-2015-3-24.csv",sep=""),
                                pnum=700,nrows=-1,skip=0,method=1)
pools[poolidx]<-list(list(c(10,6,4),tmp)) ; poolidx<-poolidx+1 #No.[[2]]

tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-1000-08P6C2-2015-3-24.csv",sep=""),
                                pnum=700,nrows=-1,skip=0,method=1)
pools[poolidx]<-list(list(c(8,6,2),tmp)) ; poolidx<-poolidx+1 #No.[[3]]

tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-1000-06P4C2-2015-3-24.csv",sep=""),
                                pnum=700,nrows=-1,skip=0,method=1)
pools[poolidx]<-list(list(c(6,4,2),tmp)) ; poolidx<-poolidx+1 #No.[[4]]

tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-1000-04P2C2-2015-3-24.csv",sep=""),
                                 pnum=350,nrows=-1,skip=0,method=1)
pools[poolidx]<-list(list(c(4,2,2),tmp)) ; poolidx<-poolidx+1 #No.[[5]]

rm(poolidx,tmp)
#pools<-list(list(c(10,8,2),tmp),list(c(6,4,2),tmp2),list(c(4,2,2),tmp3)) ; rm(tmp,tmp2,tmp3)
#rm(pools)

# functions for combined population serach ------------
# two sample examples. one from pools[[2]], the other from pools[[3]]
#ceiling(runif(1, min=1e-320, max=nrow(pools[[2]][[2]])))
create_combined_population<-function(popnum,thresh=1000,plelem=c(4,5),fname,isFileout=FALSE,isDebug=FALSE,maxposn=10){
  added_num<-0
  total_count<-0
  while(TRUE) {
    s1<-pools[[ plelem[1] ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[ plelem[1] ]][[2]]))), ]
    s1_pos<-unlist(s1[-length(s1)]);s1_score<-as.numeric(s1[length(s1)])
    if(isDebug){cat("s1 :",s1_pos," sc:",s1_score)   }
    
    s2<-pools[[ plelem[2] ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[  plelem[2] ]][[2]]))), ]
    s2_pos<-unlist(s2[-length(s2)]);s2_score<-as.numeric(s2[length(s2)])
    if(isDebug){cat(" s2 :",s2_pos," sc:",s2_score)   }
    
    x_new<-rep(0,times=length(iniPos))
    x_new<-s1_pos+s2_pos
    x_new<-as.numeric((sum(x_new%%7)!=0))*x_new+as.numeric((sum(x_new%%7)==0))*x_new/7
    x_new<-as.numeric((sum(x_new%%5)!=0))*x_new+as.numeric((sum(x_new%%5)==0))*x_new/5
    x_new<-as.numeric((sum(x_new%%4)!=0))*x_new+as.numeric((sum(x_new%%4)==0))*x_new/4
    x_new<-as.numeric((sum(x_new%%3)!=0))*x_new+as.numeric((sum(x_new%%3)==0))*x_new/3
    x_new<-as.numeric((sum(x_new%%2)!=0))*x_new+as.numeric((sum(x_new%%2)==0))*x_new/2
    x_new<-as.numeric((max(abs(x_new))==1))*x_new*2+as.numeric((max(abs(x_new))!=1))*x_new
    if(isDebug){ cat(" x_new :",x_new,"\n") }
    total_count<-total_count+1
    
    if(sum(as.numeric((x_new-iniPos)!=0))>maxposn){
      next
    }
    
    #evaluate
    val<-obj_Income(x_new,isDebug=isDebug)
    if(val<thresh){
      if(added_num==0){
        ret_val<-x_new
      }else{
        ret_val<-c(ret_val,x_new)
      }
      added_num<-added_num+1
      if(isFileout){  
        cat(x_new,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
        cat(val,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);
        cat(pools[[ plelem[1] ]][[1]][2:3],file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);cat(s1_score,file=fname,append=TRUE);cat(",",file=fname,append=TRUE);
        cat(pools[[ plelem[2] ]][[1]][2:3],file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);cat(s2_score,file=fname,"\n",append=TRUE)
      }
    }
    if(((added_num%%30)==0)){cat(" added num:",added_num,"total count:",total_count,"\n")}
    if(added_num==popnum)
      break
  }
}

# combined population serach --------------
#6P4C2+4P2C2
create_combined_population(popnum=300,thresh=1000,plelem=c(4,5),fname=paste(".\\ResultData\\combile-Result-",format(Sys.time(),"%Y-%b-%d"),"-42+22.csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=10)
#8P6C2+4P2C2
create_combined_population(popnum=300,thresh=1000,plelem=c(3,5),fname=paste(".\\ResultData\\combile-Result-",format(Sys.time(),"%Y-%b-%d"),"-62+22.csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=10)
#8P6C2+6P4C2
create_combined_population(popnum=300,thresh=1000,plelem=c(3,4),fname=paste(".\\ResultData\\combile-Result-",format(Sys.time(),"%Y-%b-%d"),"-62+42.csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=10)
#10P6C4+4P2C2
create_combined_population(popnum=300,thresh=1000,plelem=c(2,5),fname=paste(".\\ResultData\\combile-Result-",format(Sys.time(),"%Y-%b-%d"),"-64+22.csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=10)
#10P6C4+6P4C2
create_combined_population(popnum=300,thresh=1000,plelem=c(2,4),fname=paste(".\\ResultData\\combile-Result-",format(Sys.time(),"%Y-%b-%d"),"-64+42.csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=10)
#10P6C4+8P6C2
create_combined_population(popnum=300,thresh=1000,plelem=c(2,3),fname=paste(".\\ResultData\\combile-Result-",format(Sys.time(),"%Y-%b-%d"),"-64+62.csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=10)
#10P8C2+4P2C2
create_combined_population(popnum=300,thresh=1000,plelem=c(1,5),fname=paste(".\\ResultData\\combile-Result-",format(Sys.time(),"%Y-%b-%d"),"-82+22.csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=10)
#10P8C2+6P4C2
create_combined_population(popnum=300,thresh=1000,plelem=c(1,4),fname=paste(".\\ResultData\\combile-Result-",format(Sys.time(),"%Y-%b-%d"),"-82+42.csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=10)
#10P8C2+8P6C2
create_combined_population(popnum=300,thresh=1000,plelem=c(1,3),fname=paste(".\\ResultData\\combile-Result-",format(Sys.time(),"%Y-%b-%d"),"-82+62.csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=10)
rm(pools)

#Initial and evaluation vector ----------
iniPos<-opchain$Position
iniPos<-rep(0,length(iniPos))
evaPos<-opchain$Position

#test sample value -----------
obj_Income(x=evaPos,isDebug=TRUE)
#obj_Income_Cont(x=evaPos,isDebug=TRUE)
#obj_Income_genoud_lex_int(x=evaPos,isDebug=TRUE)

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
  if(penalty1>2){
    if(isDebug){cat("pos num",pos_change,"\n")}
    return( (500+(pos_change-10))*penalty1 )
  }
  if(isDebug){cat(x," ");cat(" :p1",penalty1)}
  
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
    if(val<best_result){
      cat(x,file=result_file,sep=",",append=TRUE);cat(",",file=result_file,append=TRUE)
      cat(val,file=result_file,"\n",append=TRUE)
      #write(x,result_file,append=T)
      #write(val,result_file,append=T)
      #ou use cat which gives further details on the format used
      best_result<<-val
      #assign(best_result,val,env=.GlobalEnv)
    }
  }
  return(val)
}
obj_Income_orig <- function(x,isDebug=TRUE,isMCGA=FALSE,isGenoud=FALSE){
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
    if(val<best_result){
      cat(x,file=result_file,sep=",",append=TRUE);cat(",",file=result_file,append=TRUE)
      cat(val,file=result_file,"\n",append=TRUE)
      #write(x,result_file,append=T)
      #write(val,result_file,append=T)
      #ou use cat which gives further details on the format used
      best_result<<-val
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
#deoptR_inipop_vec<-create_initial_polulation(popnum=length(evaPos),thresh=1000)
#deoptR_inipop_vec<- read already created population
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isGenoud=FALSE ; isMCGA=FALSE
edoprCon=function(x){
  x<-round(x)
  pos_change<-(sum(as.numeric((x-iniPos)!=0))-10)
  c(pos_change)
}
outjdopr<-JDEoptim(lower=rep(-6,length(iniPos)),upper=rep(6,length(iniPos)), fn=obj_Income,
                   tol=1e-10,NP=10*length(iniPos),
                   maxiter=15*length(iniPos),#50*length(iniPos),
                   #add_to_init_pop=matrix(deoptR_inipop_vec,
                   #                       nrow=length(iniPos),ncol=length(iniPos)),
                   constr=edoprCon, meq = 0,trace=TRUE,detail=TRUE)
rm(deoptR_inipop_vec,edoprCon)
#JDEoptim(.., NP = 10*d, tol = 1e-15, maxiter = 200*d, trace = FALSE, triter = 1, details = FALSE, ...)
#genoud ========
genoud_inipop_vec<-deopt_inipop_vec
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isGenoud=TRUE ; isMCGA=FALSE
domain<-matrix(c(rep(-6,length(evaPos)),rep(6,length(iniPos))), nrow=length(iniPos), ncol=2)
outgen <- genoud(#fn=obj_Income_genoud_lex_int,lexical=TRUE,
                 fn=obj_Income,
                 nvars=length(iniPos),pop.size=1000,max.generations=100,
                 data.type.int=TRUE,#FALSE
                 wait.generations=100,gradient.check=FALSE,MemoryMatrix=TRUE,
                 boundary.enforcement=2,
                 starting.values=matrix(genoud_inipop_vec,
                                        nrow=10*length(evaPos),ncol=length(evaPos),byrow=T),
                 Domains=domain)

# result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
# best_result<-500
# domain<-matrix(c(rep(-6.49,length(evaPos)),rep(6.49,length(iniPos))), nrow=length(iniPos), ncol=2)
# #isGenoud=TRUE ; isMCGA=FALSE
# outgen <- genoud(fn=obj_Income_Cont,
#                  nvars=length(iniPos),pop.size=1000,max.generations=150,
#                  wait.generations=20,gradient.check=FALSE,MemoryMatrix=TRUE,
#                  boundary.enforcement=2,
#                  starting.values=rnorm(n=length(iniPos),mean=0,sd=2),
#                  Domains=domain)
rm(domain)
#hydroPSO ======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-500
outhdpso<-hydroPSO(par=as.numeric(evaPos),#rnorm(n=length(iniPos),mean=0,sd=2)
                   fn=obj_Income,#obj_Income_Cont,
                   lower=rep(-6,length(evaPos)), upper=rep(6,length(evaPos)))
#dfoptim  =======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-500
outhjkb<-hjkb(par=evaPos, #rnorm(n=length(iniPos),mean=0,sd=2),
              fn=obj_Income, lower = rep(-6,length(iniPos)), upper =rep(6,length(iniPos)))
outnmkb<-nmkb(par=evaPos,#norm(n=length(iniPos),mean=0,sd=1)
              fn=obj_Income, lower = rep(-6,length(iniPos)), upper =rep(6,length(iniPos)))
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
              crossprob=1.0,mutateprob=0.01,evalFunc=obj_Income_Cont)
#GenSA Intermediate not sufficient =======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-500
#isGenoud=FALSE ; isMCGA=FALSE
outgensa<-GenSA(par=as.numeric(evaPos),fn=obj_Income,lower=rep(-6.1,length(iniPos)),upper=rep(6.1,length(iniPos)))
#DEOptim Intermediate not sufficient  =======
deopt_inipop_vec<-create_initial_polulation(popnum=10*length(evaPos),thresh=1000)
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-500
outdeop <- DEoptim(fn=obj_Income,lower = rep(-6,length(iniPos)),upper = rep(6,length(iniPos)),
                   control=DEoptim.control(itermax = 200,strategy=1,
                                           initialpop=matrix(deopt_inipop_vec,
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
