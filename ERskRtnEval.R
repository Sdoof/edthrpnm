##
# functions optimized

obj_Income_sgmd <- function(x,Setting,isDebug=FALSE,isDetail=FALSE,
                            udlStepNum,udlStepPct,maxposnum,PosMultip,
                            tail_rate,lossLimitPrice,
                            Delta_Direct_Prf,Vega_Direct_Prf,
                            Delta_Neutral_Offset,Vega_Neutral_Offset){
  #returned when the spread is not appropriate
  unacceptableVal=10
  
  if(isDebug){
    cat("\n###################### eval func start\n")
    cat(":(Delta_Direct_Prf)",Delta_Direct_Prf)
    cat(" :(Vega_Direct_Prf)",Vega_Direct_Prf)
    cat(" :(Delta_Neutral_Offset)",Delta_Neutral_Offset," :Vega_Neutral_Offset",Vega_Neutral_Offset)
    cat(" :(holdDays)",Setting$holdDays,"\n")
  }
  
  #position where pos$Position != 0
  position<-hollowNonZeroPosition(pos=x)
  if(isDetail){print(position)}
  
  #position evaluated after holdDays later
  udlStepNum<-udlStepNum
  udlStepPct<-udlStepPct
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  
  #dATMIV/dIVIDX 1 day regression result
  posStepDays<-data.frame(days=c(1,Setting$holdDays))
  posStepDays %>% group_by(days) %>%
    do(scene=createPositionEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct,
                                    multi=PosMultip,hdd=1,HV_IV_Adjust_Ratio=Setting$HV_IV_Adjust_Ratio)) -> posStepDays
  posStepDays %>% group_by(days) %>% rowwise() %>%
    do(days=.$days,scene2=adjustPosChg(.$scene,.$days-1,base_vol_chg=0,multi=PosMultip,hdd=Setting$holdDays,HV_IV_Adjust_Ratio=Setting$HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$days) -> posStepDays$days ; tmp$scene2 -> posStepDays$scene ;rm(tmp)
  
  #last day
  posEvalTbl<-posStepDays$scene[[length(posStepDays)]]
  
  if(isDetail){print(posEvalTbl)}
  if(isDetail){print(posEvalTbl$pos)}
  
  #At day 0 position price and Greeks.
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip,hdd=Setting$holdDays,HV_IV_Adjust_Ratio=Setting$HV_IV_Adjust_Ratio)
  if(isDetail){cat("initail position\n");print(thePositionGrk)}
    
  #weighting calculate
  sd_multp<-Setting$holdDays
  anlzd_sd<-histIV$IVIDX[1]*Setting$HV_IV_Adjust_Ratio
  sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  #f.y.i sd_hd<-exp(anlzd_sd*sqrt(sd_multp/252))-1 #exponential expression
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  if(isDetail){cat(":(weight)",weight)}
  ##
  # Constraint 2. Tail Risk
  #   tailPrice<-min(sum(getIntrisicValue(position$UDLY[1]*(1-tail_rate),position)),
  #                  sum(getIntrisicValue(position$UDLY[1]*(1+tail_rate),position)))
  #   lossLimitPrice <- (-1)*lossLimitPrice
  #   if(isDebug){cat(" :tlpr",tailPrice);cat(" :lslmt",lossLimitPrice);cat(" :p2",penalty2)}
  #   if(tailPrice<lossLimitPrice)
  #     return(unacceptableVal)
  
  ##
  # Constraint 4. ThetaEffect. This should be soft constraint
  if(Setting$ThetaEffectPositive){
    theta_ttl<-sum(posEvalTbl$Theta*weight)
    if(isDetail){cat(" :(thta_ttl)",theta_ttl)}
    if(isDetail){cat(" :(thta_ini)",thePositionGrk$ThetaEffect);cat(" :(thta_wt)",sum(posEvalTbl$ThetaEffect*weight))}
    if(theta_ttl<0)
      return(unacceptableVal)
  }
  
  ##
  # Profit
  profit_hdays<-sum((posEvalTbl$Price-thePositionGrk$Price)*weight)
  profit_vector<-(posEvalTbl$Price-thePositionGrk$Price)
  maxLoss<-min(profit_vector)

  c3<-profit_hdays
  
  ## profit sd
  weight_times = round(weight*100)
  #if(isDetail){cat(" :(weight_times)",weight_times)}
  pdist = rep(profit_vector,times=weight_times)
  #if(isDetail){cat(" :(profit_dist)",pdist)}
  profit_sd<-sd(pdist)
  #if(isDetail){ cat("profit_sd",profit_sd)}
  if(isDetail){
    cat(" :(prft_vec)",profit_vector); cat(" :(prft_wght)",profit_hdays);cat(" :(profit_sd)",profit_sd);cat(" :(max_loss)",maxLoss)
  }
  
  ##
  # Advantageous Effects.
  c5<- sum((posEvalTbl$GammaEffect+posEvalTbl$ThetaEffect)*weight)
  if(isDebug){cat(" :c5(AdvEffect_wght)",c5)}
  
  ##
  # Directional Effects.
  ##
  #   Delta
  ##Delta_Neutral_Offset
  #expPriceChange<-posEvalTbl$UDLY*(exp(posEvalTbl$IVIDX*Setting$HV_IV_Adjust_Ratio*sqrt(Setting$holdDays/252))-1)
  expPriceChange <- getExpectedValueChange(base=posEvalTbl$UDLY,sd=posEvalTbl$IVIDX*Setting$HV_IV_Adjust_Ratio, dtime=Setting$holdDays/252)
  
  
  Delta_revised_offset<-posEvalTbl$Delta-Delta_Neutral_Offset
  Delta_Effect_revised_offset<- (-abs(Delta_revised_offset))*expPriceChange
  if(isDebug){
    cat(" :(expPriceChange)",expPriceChange," :(Delta Offset)",Delta_revised_offset," :(DeltaE offset)",Delta_Effect_revised_offset)
    
  }
  Delta_revised_offset<-sum(Delta_revised_offset*weight)
  Delta_Effect_revised_offset<-sum(Delta_Effect_revised_offset*weight)
  if(isDebug){
    cat(" :(DeltaE_wght)",Delta_Effect_revised_offset," :(Delta_wght)",Delta_revised_offset)
  }
  ###Delta_Thresh_Minus,Delta_Thresh_Plus
  DeltaEffect_Comp<-(Delta_revised_offset<0)*(Delta_revised_offset<Setting$Delta_Thresh_Minus[length(position$TYPE)])*Delta_Effect_revised_offset+
    (Delta_revised_offset>0)*(Delta_revised_offset>Setting$Delta_Thresh_Plus[length(position$TYPE)])*Delta_Effect_revised_offset
  
  if(isDebug){
    cat(" :betwn (Delta_Thresh_Minus)",Setting$Delta_Thresh_Minus[length(position$TYPE)],
        " and (Delta_Thresh_Plus)",Setting$Delta_Thresh_Plus[length(position$TYPE)])
    cat(" :(new DeltaE_wght)",DeltaEffect_Comp)
  }
  
  ##
  #    Vega
  ##Vega_Neutral_Offset
  expIVChange<-getExpectedValueChange(base=posEvalTbl$IVIDX,sd=annuual.daily.volatility(histIV$IVIDX)$daily,dtime=Setting$holdDays)*100
  Vega_revised_offset<-posEvalTbl$Vega-Vega_Neutral_Offset
  Vega_Effect_revised_offset<- (-abs(Vega_revised_offset))*expIVChange
  if(isDebug){
    cat(" :(expIVChange)",expIVChange," :(Vega Offset)",Vega_revised_offset," :(VegaE offset)",Vega_Effect_revised_offset)
    
  }
  Vega_revised_offset<-sum(Vega_revised_offset*weight)
  Vega_Effect_revised_offset<-sum(Vega_Effect_revised_offset*weight)
  if(isDebug){
    cat(" :(VegaE_Wght)",Vega_Effect_revised_offset," :(Vega_Wght)",Vega_revised_offset)
  }
  ##Vega_Thresh_Minus,Vega_Thresh_Plus
  VegaEffect_Comp<-(Vega_revised_offset<0)*(Vega_revised_offset<Setting$Vega_Thresh_Minus[length(position$TYPE)])*Vega_Effect_revised_offset+
    (Vega_revised_offset>0)*(Vega_revised_offset>Setting$Vega_Thresh_Plus[length(position$TYPE)])*Vega_Effect_revised_offset
   if(isDebug){
    cat(" :betwn (Vega_Thresh_Minus)",Setting$Vega_Thresh_Minus[length(position$TYPE)],
        " and (Vega_Thresh_Plus)",Setting$Vega_Thresh_Plus[length(position$TYPE)])
    cat(" :(new VegaE_wght)",VegaEffect_Comp)
   }
  
  ##
  # Vega_Direct_Prf,Delta_Direct_Prf reflected as coef
  dlta_pref_coef<-(Delta_Direct_Prf==0)*(-1)+
    (Delta_Direct_Prf>0)*(Delta_revised_offset>=0)+(Delta_Direct_Prf>0)*(Delta_revised_offset<0)*(-1)+
    (Delta_Direct_Prf<0)*(Delta_revised_offset>=0)*(-1)+(Delta_Direct_Prf<0)*(Delta_revised_offset<0)
  
  vega_pref_coef<-(Vega_Direct_Prf==0)*(-1)+
    (Vega_Direct_Prf>0)*(Vega_revised_offset>=0)+(Vega_Direct_Prf>0)*(Vega_revised_offset<0)*(-1)+
    (Vega_Direct_Prf<0)*(Vega_revised_offset>=0)*(-1)+(Vega_Direct_Prf<0)*(Vega_revised_offset<0)
  
  ##
  # Directional Effect
  c6<- vega_pref_coef*VegaEffect_Comp+dlta_pref_coef*DeltaEffect_Comp
  
  if(isDebug){
    cat(" (:vega_pref_coef",vega_pref_coef," x :VegaE_new",VegaEffect_Comp,
        "+ :dlta_pref_coef",dlta_pref_coef," x :DeltaE_new",DeltaEffect_Comp," = :(DrctlEffect)c6 ",c6,")")
  }
  
  ##
  # max Loss
  c8<- profit_sd
  if(isDebug){cat(" :c8(profit_sd)",c8)}
  
  ##
  # cost7 All Effects.
  #weight is normalized
  c7<- c5+c6
  #cost7<-sigmoid(c5,a=Setting$SigmoidA_AllEffect,b=0)
  if(isDebug){cat(" :c7(AllEffect)",c7)}
  
  ##
  # total cost is weighted sum of each cost.
  
  A<- Setting$DrctlEffect_Coef*c6 + Setting$AllEffect_Coef*c7 + Setting$MaxLoss_Coef*c8
  B<-Setting$AdvEffect_Coef*c5+Setting$Profit_Coef*c3
  
  if(isDebug){cat(" :Coef_Drct",Setting$DrctlEffect_Coef,"x",c6,"+:Coef_AllE",Setting$AllEffect_Coef,"x",c7,"+ :Coef_MaxLoss(SD)",Setting$MaxLoss_Coef,"x",c8,"= Numr",A)}
  if(isDebug){cat(" :Coef_Adv",Setting$AdvEffect_Coef,"x",c5,"+:Coef_Prft",Setting$Profit_Coef,"x",c3,"= Denom",B)}
  
  sigA<-sigmoid(A,a=Setting$SigmoidA_Numerator,b=0)
  sigB<-sigmoid(B,a=Setting$SigmoidA_Denominator,b=0)
  #cost<-(sigA/(1-sigB))
  cost<-sigA/sigB
  if(isDebug){cat(" :sigA",sigA,":sigB",sigB,":cost(sigA/sigB)",cost)}
  #if(isDebug){cat(" :cost",cost," ")}
  
  ##
  # total cost and penalty
  val<-cost 
  
  if(isDebug){cat(" :val",val,"\n")}
  return(val)
}

#evaluation of the Date and just its greekeffect (not weighted)
obj_fixedpt_sgmd <- function(x,Setting,isDebug=FALSE,isDetail=FALSE,
                             udlStepNum,udlStepPct,maxposnum,PosMultip,
                             tail_rate,lossLimitPrice,
                             Delta_Direct_Prf,Vega_Direct_Prf,
                             Delta_Neutral_Offset,Vega_Neutral_Offset){
  #returned when the spread is not appropriate
  unacceptableVal=10
  
  if(isDebug){
    cat("\n :Delta_Direct_Prf",Delta_Direct_Prf)
    cat(" :Vega_Direct_Prf",Vega_Direct_Prf)
    cat(" :Delta_Neutral_Offset",Delta_Neutral_Offset," :Vega_Neutral_Offset",Vega_Neutral_Offset)
    cat(" :holdDays",Setting$holdDays,"\n")
  }
  
  #position where pos$Position != 0
  position<-hollowNonZeroPosition(pos=x)
  if(isDetail){print(position)}
  
  #At day 0 position price and Greeks.
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip,hdd=Setting$holdDays,HV_IV_Adjust_Ratio=Setting$HV_IV_Adjust_Ratio)
  if(isDetail){print(thePositionGrk)}  
  
  ##
  # AdvEffect
  c5<- thePositionGrk$GammaEffect+thePositionGrk$ThetaEffect
  if(isDebug){cat(" :c5(AdvEffect)",c5)}
  
  ##
  # Directional Effects.
  ## Delta
  theDelta<-thePositionGrk$Delta
  theDeltaEfct<-thePositionGrk$DeltaEffect
  
  ##Delta_Neutral_Offset
   expPriceChange <- getExpectedValueChange(base=posEvalTbl$UDLY,sd=position$IVIDX[1]*Setting$HV_IV_Adjust_Ratio, dtime=Setting$holdDays/252)
   Delta_revised_offset<-theDelta-Delta_Neutral_Offset
   Delta_Effect_revised_offset<- (-abs(Delta_revised_offset))*expPriceChange
   
   if(isDebug){
     cat(" :(expctPrcChg)",expPriceChange,#" :double check(expctPrcChg)",theDeltaEfct/(-abs(theDelta)),
         " :(DeltaE offset)",Delta_Effect_revised_offset," :(Delta offset)",Delta_revised_offset)
   }
   
   ###Delta_Thresh_Minus,Delta_Thresh_Plus
   DeltaEffect_Comp<-(Delta_revised_offset<0)*(Delta_revised_offset<Setting$Delta_Thresh_Minus[length(position$TYPE)])*Delta_Effect_revised_offset+
     (Delta_revised_offset>0)*(Delta_revised_offset>Setting$Delta_Thresh_Plus[length(position$TYPE)])*Delta_Effect_revised_offset
   
   if(isDebug){
     cat(" :btwn (Delta_Thresh_Minus)",Setting$Delta_Thresh_Minus[length(position$TYPE)],
         " :and (Delta_Thresh_Plus)",Setting$Delta_Thresh_Plus[length(position$TYPE)])
     cat(" :(new DeltaE)",DeltaEffect_Comp)
   }
  
  ## Vega
  theVega<-thePositionGrk$Vega
  theVegaEfct<-thePositionGrk$VegaEffect
  
  ##Vega_Neutral_Offset
  expIVChange<-getExpectedValueChange(base=position$IVIDX[1],sd=annuual.daily.volatility(histIV$IVIDX)$daily,dtime=Setting$holdDays)
  Vega_revised_offset<-theVega-Vega_Neutral_Offset
  Vega_Effect_revised_offset<- (-abs(Vega_revised_offset))*expIVChange
  
  if(isDebug){
    cat(" :(expIVChange)",expIVChange,#" :double check(expIVChange)",theVegaEfct/(-abs(theVega)),
        " :(VegaE offset)",Vega_Effect_revised_offset," :(Vega offset)",Vega_revised_offset)
  }
  
  ###Delta_Thresh_Minus,Delta_Thresh_Plus
  VegaEffect_Comp<-(Vega_revised_offset<0)*(Vega_revised_offset<Setting$Vega_Thresh_Minus[length(position$TYPE)])*Vega_Effect_revised_offset+
    (Vega_revised_offset>0)*(Vega_revised_offset>Setting$Vega_Thresh_Plus[length(position$TYPE)])*Vega_Effect_revised_offset
  
  if(isDebug){
    cat(" :btw (Vega_Thresh_Minus)",Setting$Vega_Thresh_Minus[length(position$TYPE)],
        " :and (Vega_Thresh_Plus)",Setting$Vega_Thresh_Plus[length(position$TYPE)])
    cat(" :(new VegaE)",VegaEffect_Comp)
  }
  
  ##
  # Vega_Direct_Prf,Delta_Direct_Prf reflected as coef
  
  dlta_pref_coef<-(Delta_Direct_Prf==0)*(-1)+
    (Delta_Direct_Prf>0)*(Delta_revised_offset>=0)+(Delta_Direct_Prf>0)*(Delta_revised_offset<0)*(-1)+
    (Delta_Direct_Prf<0)*(Delta_revised_offset>=0)*(-1)+(Delta_Direct_Prf<0)*(Delta_revised_offset<0)
  
  vega_pref_coef<-(Vega_Direct_Prf==0)*(-1)+
    (Vega_Direct_Prf>0)*(Vega_revised_offset>=0)+(Vega_Direct_Prf>0)*(Vega_revised_offset<0)*(-1)+
    (Vega_Direct_Prf<0)*(Vega_revised_offset>=0)*(-1)+(Vega_Direct_Prf<0)*(Vega_revised_offset<0)
#   
#   dlta_pref_coef<-(Delta_Direct_Prf==0)*(-1)+
#     (Delta_Direct_Prf>0)*(thePositionGrk$Delta>=0)+(Delta_Direct_Prf>0)*(thePositionGrk$Delta<0)*(-1)+
#     (Delta_Direct_Prf<0)*(thePositionGrk$Delta>=0)*(-1)+(Delta_Direct_Prf<0)*(thePositionGrk$Delta<0)
#   
#   vega_pref_coef<-(Vega_Direct_Prf==0)*(-1)+
#     (Vega_Direct_Prf>0)*(thePositionGrk$Vega>=0)+(Vega_Direct_Prf>0)*(thePositionGrk$Vega<0)*(-1)+
#     (Vega_Direct_Prf<0)*(thePositionGrk$Vega>=0)*(-1)+(Vega_Direct_Prf<0)*(thePositionGrk$Vega<0)
  
  c6<- vega_pref_coef*VegaEffect_Comp+dlta_pref_coef*DeltaEffect_Comp
  
  if(isDebug){
    cat(" (:vega_pref_coef",vega_pref_coef," x :VegaE_new",VegaEffect_Comp,
        "+ :dlta_pref_coef",dlta_pref_coef," x :DeltaE_new",DeltaEffect_Comp," = :(DrctlEffect)c6 ",c6,")")
    }
  
  ##
  # All Effects
  c7<- c5+c6
  if(isDebug){cat(" :c7(AllEffect)",c7)}
  
  ##
  # total cost is weighted sum of each cost.
  c3<-0 #Profit is not valid here.
  A<-Setting$DrctlEffect_Coef*c6+Setting$AllEffect_Coef*c7
  B<-(Setting$AdvEffect_Coef+Setting$Profit_Coef)*c5+Setting$Profit_Coef*c3
  
  if(isDebug){cat(" (:Coef_Drct",Setting$DrctlEffect_Coef,"x",c6,"+:Coef_AllE",Setting$AllEffect_Coef,"x",c7,"= Numr ",A,")")}
  if(isDebug){cat(" (:Coef_Adv",Setting$AdvEffect_Coef+Setting$Profit_Coef,"x",c5,"+:Coef_Prft 0 x",c3,"= Denom",B,")")}
  
  sigA<-sigmoid(A,a=Setting$SigmoidA_Numerator,b=0)
  sigB<-sigmoid(B,a=Setting$SigmoidA_Denominator,b=0)
  cost<-sigA/sigB
  
  if(isDebug){cat(" :a",Setting$SigmoidA_Numerator," :sigA",sigA," :a",Setting$SigmoidA_Denominator," :sigB",sigB," :cost(sigA/sigB)",cost)}
  
  ##
  # total cost and penalty
  val<-cost
  
  if(isDebug){cat(" :val",val,"\n")}
  return(val)
  
}

#Rsk/Rtn greek related functions
#get the position's total greek
getPosGreeks<-function(pos,greek,multi){
  pos_greek<-sum(pos*multi*greek)
  pos_greek
}

#getGreekEffects

getThetaEffect<-function(pos,greek,multi,hdd){
  theta<-getPosGreeks(pos=pos,greek=greek,multi=multi)
  thetaEfct<-holdDays*theta
  thetaEfct
}

getDeltaEffect<-function(pos,greek,UDLY,rlzdvol_td,multi,hdd){
  #deviation_sd2expct_convratio <- integrate(f <- function(x) abs(x)*dnorm(x,0,1),-100,100)
  deviation_sd2expct_convratio=0.7978846
  rlzdvol_td <- rlzdvol_td*deviation_sd2expct_convratio
  
  expPriceChange<-mean(UDLY*(exp(rlzdvol_td*sqrt(hdd/252))-1))
  delta<-getPosGreeks(pos=pos,greek=greek,multi=multi)
  deltaEfct<-(-abs(delta))*expPriceChange
  deltaEfct
}

getExpectedValueChange <- function(base,sd,dtime){
  #deviation_sd2expct_convratio <- integrate(f <- function(x) abs(x)*dnorm(x,0,1),-100,100)
  deviation_sd2expct_convratio=0.7978846
  sd <- sd*deviation_sd2expct_convratio
  
  valueChange<- base*(exp(sd*sqrt(dtime))-1)
  
  valueChange
}

getGammaEffect<-function(pos,greek,UDLY,rlzdvol_td,multi,hdd){
  #deviation_sd2expct_convratio <- integrate(f <- function(x) abs(x)*dnorm(x,0,1),-100,100)
  deviation_sd2expct_convratio=0.7978846
  rlzdvol_td <- rlzdvol_td*deviation_sd2expct_convratio
  
  expPriceChange<-mean(UDLY*(exp(rlzdvol_td*sqrt(hdd/252))-1))
  gamma<-getPosGreeks(pos=pos,greek=greek,multi=multi)
  gammaEfct<-gamma*(expPriceChange^2)/2
  gammaEfct
}

getVegaEffect<-function(pos,greek,ividx,dviv,multi,hdd){
  #deviation_sd2expct_convratio <- integrate(f <- function(x) abs(x)*dnorm(x,0,1),-100,100)
  deviation_sd2expct_convratio=0.7978846
  dviv <- dviv*deviation_sd2expct_convratio
  
  expIVChange<-mean(ividx*(exp(dviv*sqrt(holdDays))-1))
  vega<-getPosGreeks(pos=pos,greek=greek,multi=multi)
  vegaEffect<-(-abs(vega))*(expIVChange*100)
  vegaEffect
}

getVommaEffect<-function(pos,greek,ividx,dviv,multi,hdd){
  #deviation_sd2expct_convratio <- integrate(f <- function(x) abs(x)*dnorm(x,0,1),-100,100)
  deviation_sd2expct_convratio=0.7978846
  dviv <- dviv*deviation_sd2expct_convratio
  
  expIVChange<-mean(ividx*(exp(dviv*sqrt(holdDays))-1))
  vomma<-getPosGreeks(pos=pos,greek=greek,multi=multi)
  expIVChange<-expIVChange*100
  vommaEffect<-vomma*(expIVChange^2)/2
  vommaEffect
}


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

# operate to each position data frame based on scenaro changes
# process_df is a data frame like this.
#       udlChgPct    pos
#   1     -0.12   <S3:data.frame>
#   2      0.00   <S3:data.frame>
#   3      0.12   <S3:data.frame>
# <S3:data.frame> is original data frame which only UDLY are modified.
# This function reflects Date,IV,etc after udlChg% change for the UDLYs in "days" days.
reflectPosChg<- function(process_df,days,IV_DEVIATION=0,MIN_IVIDX_CHG=(-0.5)){
  pos<-as.data.frame(process_df$pos[1])
  chg<-as.numeric(process_df$udlChgPct[1])
  # print(chg)
  
  # get (IVIDX_pre/IVIDX_pos)/(UDLY_pre/UDLY_pos)
  regression<-get.Volatility.Level.Regression(Days=days)
  ividx_chg_pct<-get.predicted.IVIDXChange(model=regression$model,xmin=chg,xmax=100,x_by=0)$IVIDXC
  #if ividx_chg_pct < MIN_IVIDX_CHG, ividx_chg_pct=MIN_IVIDX_CHG
  ividx_chg_pct<-(ividx_chg_pct<MIN_IVIDX_CHG)*MIN_IVIDX_CHG+(ividx_chg_pct>=MIN_IVIDX_CHG)*ividx_chg_pct
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
  pos$Date <- format(advance(CALENDAR_G,dates=as.Date(pos$Date,format="%Y/%m/%d"),
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
  spskew<-(pos$TYPE==OpType_Put_G)*get.predicted.spline.skew(SkewModel_Put,pos$Moneyness.Nm)+
    (pos$TYPE==OpType_Call_G)*get.predicted.spline.skew(SkewModel_Call,pos$Moneyness.Nm)
  pos$ATMIV*spskew
  pos$OrigIV<-pos$ATMIV*spskew
  
  IV_deviation<-rnorm(n=length(pos$OrigIV),mean=0,sd=IV_DEVIATION)
  pos$OrigIV<-pos$OrigIV*(1+IV_deviation)
  
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

createPositionEvalTable<-function(position,udlStepNum,udlStepPct,multi,hdd,HV_IV_Adjust_Ratio){
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-data.frame(udlChgPct=udlChgPct) ;rm(udlStepNum,udlStepPct)
  #Set data frames as a row value of another data frame.
  posEvalTbl %>% group_by(udlChgPct) %>% do(pos=position) -> posEvalTbl
  #Modify pos based on scenario
  posEvalTbl %>% group_by(udlChgPct) %>% do(pos=reflectPosChg(.,hdd)) -> posEvalTbl
 
  #cat("HV_IV_Adjust_Ratio (createPositionEvalTable):",HV_IV_Adjust_Ratio)
  
  ##
  #  Greek Effects
  
  #  ThetaEffect
  posEvalTbl %>% rowwise() %>% do(ThetaEffect=getThetaEffect(pos=.$pos$Position,greek=.$pos$Theta,multi=multi,hdd=hdd)) -> tmp
  unlist(tmp$ThetaEffect)->tmp ; posEvalTbl$ThetaEffect <- tmp ;rm(tmp)
  #  DeltaEffect
  posEvalTbl %>% rowwise() %>% do(DeltaEffect=getDeltaEffect(pos=.$pos$Position,greek=.$pos$Delta,
                                                             UDLY=.$pos$UDLY,multi=multi,hdd=hdd,
                                                             rlzdvol_td=.$pos$IVIDX*HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$DeltaEffect)->tmp ; posEvalTbl$DeltaEffect <- tmp ;rm(tmp)
  #  GammaEffect
  posEvalTbl %>% rowwise() %>% do(GammaEffect=getGammaEffect(pos=.$pos$Position,greek=.$pos$Gamma,
                                                             UDLY=.$pos$UDLY,multi=multi,hdd=hdd,
                                                             rlzdvol_td=.$pos$IVIDX*HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$GammaEffect)->tmp ; posEvalTbl$GammaEffect <- tmp ;rm(tmp) 
  #  VegaEffect
  posEvalTbl %>% rowwise() %>% do(VegaEffect=getVegaEffect(pos=.$pos$Position,greek=.$pos$Vega,
                                                           ividx=.$pos$IVIDX,multi=multi,hdd=hdd,
                                                           #dviv should be precalulated when optimized
                                                           dviv=annuual.daily.volatility(histIV$IVIDX)$daily)) -> tmp
  unlist(tmp$VegaEffect)->tmp ; posEvalTbl$VegaEffect <- tmp ;rm(tmp)
  
  ##
  #  Greeks
  #
  #  UDLY
  posEvalTbl %>% rowwise() %>% do(UDLY=mean(.$pos$UDLY)) ->tmp
  unlist(tmp$UDLY)->tmp ; posEvalTbl$UDLY <- tmp ;rm(tmp)
  #  Price
  posEvalTbl %>% rowwise() %>% do(Price=getPosGreeks(pos=.$pos$Position,greek=.$pos$Price,multi=multi)) ->tmp
  unlist(tmp$Price)->tmp ; posEvalTbl$Price <- tmp ;rm(tmp)
  #  Delta
  posEvalTbl %>% rowwise() %>% do(Delta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Delta,multi=multi))->tmp
  unlist(tmp$Delta)->tmp ; posEvalTbl$Delta <- tmp ;rm(tmp)
  #  Gamma
  posEvalTbl %>% rowwise() %>% do(Gamma=getPosGreeks(pos=.$pos$Position,greek=.$pos$Gamma,multi=multi))->tmp
  unlist(tmp$Gamma)->tmp ; posEvalTbl$Gamma <- tmp ;rm(tmp)
  #  Vega
  posEvalTbl %>% rowwise() %>% do(Vega=getPosGreeks(pos=.$pos$Position,greek=.$pos$Vega,multi=multi))->tmp
  unlist(tmp$Vega)->tmp ; posEvalTbl$Vega <- tmp ;rm(tmp)
  #  Theta
  posEvalTbl %>% rowwise() %>% do(Theta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Theta,multi=multi)) ->tmp
  unlist(tmp$Theta)->tmp ; posEvalTbl$Theta <- tmp ;rm(tmp)
  # IVIDX
  posEvalTbl %>% rowwise() %>% do(IVIDX=mean(.$pos$IVIDX)) ->tmp
  unlist(tmp$IVIDX)->tmp ; posEvalTbl$IVIDX <- tmp ;rm(tmp)
  
  posEvalTbl
  
}

#posgrks can be obtained by hollowNonZeroPosition(evaPos)
#return vector. If you like to get aggrigate payoff, use sum()
getIntrisicValue<-function(udly_price,position,multip){
  as.numeric(((udly_price-position$Strike)*(-position$TYPE)>0))*
    (udly_price-position$Strike)*(-position$TYPE)*multip*position$Position
}

##
# Creating initial candidate population of spread positions whose componets of each position are exactly spcicified by the arguments.
# if the number (putn or calln) is even number, half of the spread`s positions are assigned +1(long), the other half -1(short).
# if odd number, the first 3 positions are assigned as -1 +2 -1 to form a butterfly, the rest position are assigned in the same way
# as the case of even number (half +1 long, the other half -1 short).
# When the each position of returned compound spread is 1 or -1, the spread position are multiplied by ml. 

create_initial_exact_PutCall_polulation<-function(popnum,type,EvalFuncSetting,thresh,putn,calln,ml,fname,PosMultip,isFileout=FALSE,isDebug=FALSE,isDetail=FALSE){
  added_num<-0
  total_count<-0
  start_t<-proc.time()
  while(TRUE){
    #Put   
    idxy<-as.numeric(type==OpType_Put_G)*rep(1:length(iniPos),length=length(iniPos))
    if(isDebug){ cat(" put pos:",idxy) }
    y<-rep(0,times=length(iniPos))
    if(sum(idxy)>0){
      idxy<-idxy[idxy!=0]
      #if(isDebug){ cat(" put pos cand :",idxy) }
      idxy<-sample(idxy,size=putn,replace=FALSE,prob=NULL)
      if(isDebug){ cat(" put idxy:",idxy) }
      #y<-rep(0,times=length(iniPos))
      y_shift<-0
      putn_shift<-putn
      if((putn%%2)!=0){
        y[idxy[1:2]]<-(-1)
        y[idxy[3]]<-2
        putn_shift<-putn-3
        y_shift<-3
      }
      if(putn_shift>=2){
        y[idxy[(1+y_shift):(y_shift+(putn_shift/2))]]<-1
        y[idxy[(putn_shift/2+1+y_shift):putn]]<-(-1)
      }
    }
    #Call
    idxy<-as.numeric(type==OpType_Call_G)*rep(1:length(iniPos),length=length(iniPos))
    if(isDebug){ cat(" call pos:",idxy) }
    z<-rep(0,times=length(iniPos))
    if(sum(idxy)>0){
      idxy<-idxy[idxy!=0]
      idxy<-sample(idxy,size=calln,replace=FALSE,prob=NULL)
      if(isDebug){ cat(" call idxy:",idxy) }
      #z<-rep(0,times=length(iniPos))
      z_shift<-0
      calln_shift<-calln
      if((calln%%2)!=0){
        z[idxy[1:2]]<-(-1)
        z[idxy[3]]<-2
        calln_shift<-calln-3
        z_shift<-3
      }
      if(calln_shift>=2){    
        z[idxy[1+z_shift:(z_shift+(calln_shift/2))]]<-1
        z[idxy[(calln_shift/2+1+z_shift):calln]]<-(-1)
      }
    }
    if(isDebug){ cat(" (:y",y,")") }
    if(isDebug){ cat(" (:z",z,") :x(y+z) ") }
    x<-y+z
    x<-as.numeric(((putn%%2)==0)*((calln%%2)==0))*ml*x+as.numeric(!((putn%%2)==0)*((calln%%2)==0))*ml*x
    if(isDebug){ cat(" (:x",x,")") }
    
    ##
    # x==0 means no position to search that match the putn and calln
    if(sum((x!=0))==0)
      break
    
    posnum<-putn +calln
    tryCatch(
      val<-obj_Income_sgmd(x,EvalFuncSetting,isDebug=isDebug,isDetail=isDetail,
      #val<-obj_fixedpt_sgmd(x,EvalFuncSetting,isDebug=isDebug,isDetail=isDetail,
                           udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                           maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                           tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                           Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                           Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum]),
      error=function(e){
        message(e)
        val<-(thresh+1.0)
      })
    
    if(val<thresh){
      added_num<-added_num+1
      if(isFileout){
        cat(x,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
        cat(val,file=fname,"\n",append=TRUE)
      }
    }
    total_count<-total_count+1
    if((added_num%%50)==0){
      cat(" added num:",added_num,"total count:",total_count,"putn:",putn,"calln:",calln,"time:",(proc.time()-start_t)[3],"\n")
      start_t<-proc.time()
    }
    if(added_num==popnum)
      break
  }
  cat(" added num:",added_num,"total count:",total_count,"putn:",putn,"calln:",calln,"time:",(proc.time()-start_t)[3],"\n")
}

#function for creating one candidate pool
createCombineCandidatePool<-function(fname,pnum=1000,nrows=-1,skip=0,method=1){
  pool<-read.csv(fname, header=FALSE,nrows=nrows,skip=skip)
  pnum<-as.numeric((pnum==0))*nrow(pool)+as.numeric((pnum!=0))*pnum
  pool %>% dplyr::arrange(pool[,(length(iniPos)+1)]) %>% dplyr::distinct() -> pool
  
  
  #select specific nums. some optional methods
  #1.top n
  if(method==1){
    pool<-pool[1:pnum,]
  }
  #2. random sample
  else if(method==2){
    idx<-rep(1:nrow(pool),length=nrow(pool))
    idx<-sort(sample(idx,size=pnum,replace=FALSE,prob=NULL))
    pool<-pool[idx,]
    rownames(pool) <- c(1:nrow(pool))
  }
  #3. bottom n
  else if(method==3){
    pool<-pool[(nrow(pool)-pnum+1):nrow(pool),]
    rownames(pool) <- c(1:nrow(pool))
  }
  pool[complete.cases(pool),] -> pool
  return(pool)
}

#function for seraching candidate by combination 
# two sample examples. one from pools[[2]], the other from pools[[3]]
#ceiling(runif(1, min=1e-320, max=nrow(pools[[2]][[2]])))
create_combined_population<-function(popnum,EvalFuncSetting,thresh,plelem,fname,isFileout=FALSE,isDebug=FALSE,maxposn,PosMultip){
  added_num<-0
  total_count<-0
  start_t<-proc.time()
  while(TRUE) {
    s1<-pools[[ plelem[1] ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[ plelem[1] ]][[2]]))), ]
    s1_pos<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
    if(isDebug){cat("s1 :",s1_pos," sc:",s1_score)   }
    
    s2<-pools[[ plelem[2] ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[  plelem[2] ]][[2]]))), ]
    s2_pos<-unlist(s2[1:length(iniPos)]);s2_score<-as.numeric(s2[length(s2)])
    if(isDebug){cat(" s2 :",s2_pos," sc:",s2_score)   }
    
    s3_pos<-rep(0,times=length(iniPos))
    if(length(plelem)==3){
      s3<-pools[[ plelem[3] ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[  plelem[3] ]][[2]]))), ]
      s3_pos<-unlist(s3[1:length(iniPos)]);s3_score<-as.numeric(s3[length(s3)])
      if(isDebug){cat(" s3 :",s3_pos," sc:",s3_score)   }
    }  
    x_new<-rep(0,times=length(iniPos))
    x_new<-s1_pos+s2_pos+s3_pos
    x_new<-as.numeric((sum(x_new%%7)!=0))*x_new+as.numeric((sum(x_new%%7)==0))*x_new/7
    x_new<-as.numeric((sum(x_new%%5)!=0))*x_new+as.numeric((sum(x_new%%5)==0))*x_new/5
    x_new<-as.numeric((sum(x_new%%4)!=0))*x_new+as.numeric((sum(x_new%%4)==0))*x_new/4
    x_new<-as.numeric((sum(x_new%%3)!=0))*x_new+as.numeric((sum(x_new%%3)==0))*x_new/3
    x_new<-as.numeric((sum(x_new%%2)!=0))*x_new+as.numeric((sum(x_new%%2)==0))*x_new/2
    x_new<-as.numeric((max(abs(x_new))==1))*x_new*2+as.numeric((max(abs(x_new))!=1))*x_new
    if(isDebug){ cat(" x_new :",x_new) }
    total_count<-total_count+1
    
    posnum<-sum(as.numeric((x_new-iniPos)!=0))
    if(posnum>maxposn){
      if(isDebug){ cat("posnum ",posnum,"over maxposn \n") }
      next
    }
    #evaluate    
    tryCatch(
      val<-obj_Income_sgmd(x_new,EvalFuncSetting,isDebug=isDebug,isDetail=isDebug,
      #val<-obj_fixedpt_sgmd(x_new,EvalFuncSetting,isDebug=isDebug,isDetail=isDebug,
                            udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                            maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                            tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                            Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                            Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum]),
      error=function(e){
        message(e)
        val<-(thresh+1.0)
      })
    
    if(val<thresh){
      added_num<-added_num+1
      if(isFileout){  
        cat(x_new,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
        cat(val,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);
        cat(pools[[ plelem[1] ]][[1]][2:3],file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);cat(s1_score,file=fname,append=TRUE);cat(",",file=fname,append=TRUE)
        cat(pools[[ plelem[2] ]][[1]][2:3],file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);cat(s2_score,file=fname,append=TRUE)
        if(length(plelem)==3){
          cat(",",file=fname,append=TRUE)
          cat(pools[[ plelem[3] ]][[1]][2:3],file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);cat(s3_score,file=fname,append=TRUE)
        }
        cat("\n",file=fname,append=TRUE)
      }
    }
    if(((added_num%%50)==0)){
      cat(" added num:",added_num," total_count",total_count," time:",(proc.time()-start_t)[3],"\n")
      start_t<-proc.time()
    }
    if(added_num==popnum)
      break
  }
}

##
# Functions to be loaded from EPosAanalysis.R

#create Aggregated Price Table for Drawing
createAgrregatedGreekTbl<-function(posStepDays,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,multi=PosMultip,iniCredit=iniCredit){
  
  #Delta
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Delta)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,Delta=greek) -> greek_tbl
  agr_tbl <- greek_tbl
  
  #Gamma
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Gamma)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,Gamma=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #Vega
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Vega)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,Vega=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #Theta
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Theta)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,Theta=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #ThetaEffect
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$ThetaEffect)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,ThetaEffect=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #GammaEffect
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$GammaEffect)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,GammaEffect=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #DeltaEffect
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$DeltaEffect)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,DeltaEffect=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #VegaEffect
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$VegaEffect)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,VegaEffect=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #profit
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createPriceTbl(.$days,.$scene,iniCredit)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  agr_tbl
}

createPriceTbl<-function(days,pos_smry,credit){
  pos_smry$UDLY
  pos_smry$Price+credit
  
  pricetbl<-data.frame(day=days,UDLY=pos_smry$UDLY, profit=(pos_smry$Price+credit))
  pricetbl
}


createGreekTbl<-function(days,pos_smry_x,pos_smry_greek){ 
  greektbl<-data.frame(day=days,x=pos_smry_x, greek=pos_smry_greek)
  greektbl
}


#innfer functions : position operation related.

#After each posTable is created by createPositionEvalTable(), 
#we must adjust actual Date and related conditions. Date (and TimeToExpDate), 
#Moneyness.nm,, IV(OrigIV) and time decayed Greeks.

#assuming on each day, IVIDX doesn't change at udlChgPcg==0.
#instance price change had only occured from stepdays before.

#nested. adjustPosChg() calls adjustPosChgInner()
adjustPosChgInner<-function(process_df,time_advcd, base_vol_chg=0){
  pos<-as.data.frame(process_df$pos[1])
  if(sum(as.numeric(time_advcd==0))!=0) return(pos)
  #print(pos)
  
  #base volatility change relrected to IVIDX
  ividx_chg_pct<-as.numeric(base_vol_chg)
  pos$IVIDX<-pos$IVIDX*(1+ividx_chg_pct)
  
  # ATM IV change
  pos$ATMIV<-pos$ATMIV*(1+ividx_chg_pct)*get.Volatility.Change.Regression.Result(pos,ividx_chg_pct)
  
  #Volatility Cone の影響。時間変化した分の影響を受ける。その比の分だけ比率変化
  #ATMIV_pos <- ATMIV_pos*(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pre/(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pos
  bdays_per_month<-252/12
  TimeToExpDate_pos<-(pos$TimeToExpDate*bdays_per_month-time_advcd)/bdays_per_month
  pos$ATMIV<-pos$ATMIV *
    get.Volatility.Cone.Regression.Result(pos$TYPE,TimeToExpDate_pos)/
    get.Volatility.Cone.Regression.Result(pos$TYPE,pos$TimeToExpDate)
  
  #set new TimeToExpDate
  pos$TimeToExpDate<-TimeToExpDate_pos
  
  #Date advance
  pos$Date <- format(advance(CALENDAR_G,dates=as.Date(pos$Date,format="%Y/%m/%d"),
                             time_advcd,0),"%Y/%m/%d")
  
  #Moneyness.nm which reflects the value of TimetoExpDate
  pos$Moneyness.Frac<-pos$Strike/pos$UDLY
  eval_timeToExpDate<-as.numeric(pos$TimeToExpDate<TimeToExp_Limit_Closeness_G)*TimeToExp_Limit_Closeness_G+
    as.numeric(pos$TimeToExpDate>=TimeToExp_Limit_Closeness_G)*pos$TimeToExpDate
  pos$Moneyness.Nm<-log(pos$Moneyness.Frac)/pos$ATMIV/sqrt(eval_timeToExpDate)
  pos$Moneyness.Frac<-NULL
  
  #calculate IV_pos(OrigIV) using SkewModel based on model definition formula.
  spskew<-(pos$TYPE==OpType_Put_G)*get.predicted.spline.skew(SkewModel_Put,pos$Moneyness.Nm)+
    (pos$TYPE==OpType_Call_G)*get.predicted.spline.skew(SkewModel_Call,pos$Moneyness.Nm)
  pos$ATMIV*spskew
  pos$OrigIV<-pos$ATMIV*spskew
  
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

adjustPosChg<-function(process_df,time_advcd,base_vol_chg=0,multi,hdd,HV_IV_Adjust_Ratio,isDebug=FALSE){
 
   if(isDebug){
    print(process_df)
    print(time_advcd)
  }
  
  process_df %>% group_by(udlChgPct) %>% do(pos=adjustPosChgInner(.,time_advcd,base_vol_chg=base_vol_chg)) -> process_df
  
  #cat("HV_IV_Adjust_Ratio (adjustPosChg):",HV_IV_Adjust_Ratio)
  
  ##
  #  Greeks
  #
  #  UDLY
  process_df %>% rowwise() %>% do(UDLY=mean(.$pos$UDLY)) ->tmp
  unlist(tmp$UDLY)->tmp ; process_df$UDLY <- tmp ;rm(tmp)
  #  Price
  process_df %>% rowwise() %>% do(Price=getPosGreeks(pos=.$pos$Position,greek=.$pos$Price,multi=multi)) ->tmp
  unlist(tmp$Price)->tmp ; process_df$Price <- tmp ;rm(tmp)
  #  Delta
  process_df %>% rowwise() %>% do(Delta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Delta,multi=multi))->tmp
  unlist(tmp$Delta)->tmp ; process_df$Delta <- tmp ;rm(tmp)
  #  Gamma
  process_df %>% rowwise() %>% do(Gamma=getPosGreeks(pos=.$pos$Position,greek=.$pos$Gamma,multi=multi))->tmp
  unlist(tmp$Gamma)->tmp ; process_df$Gamma <- tmp ;rm(tmp)
  #  Vega
  process_df %>% rowwise() %>% do(Vega=getPosGreeks(pos=.$pos$Position,greek=.$pos$Vega,multi=multi))->tmp
  unlist(tmp$Vega)->tmp ; process_df$Vega <- tmp ;rm(tmp)
  #  Theta
  process_df %>% rowwise() %>% do(Theta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Theta,multi=multi)) ->tmp
  unlist(tmp$Theta)->tmp ; process_df$Theta <- tmp ;rm(tmp)
  #  IVIDX
  process_df %>% rowwise() %>% do(IVIDX=mean(.$pos$IVIDX)) ->tmp
  unlist(tmp$IVIDX)->tmp ; process_df$IVIDX <- tmp ;rm(tmp)
  
  ##
  # Greek Effects
  
  # ThetaEffect
  process_df %>% rowwise() %>% do(ThetaEffect=getThetaEffect(pos=.$pos$Position,greek=.$pos$Theta,multi=multi,hdd=hdd)) -> tmp
  unlist(tmp$ThetaEffect)->tmp ; process_df$ThetaEffect <- tmp ;rm(tmp)
  # DeltaEffect
  process_df %>% rowwise() %>% do(DeltaEffect=getDeltaEffect(pos=.$pos$Position,greek=.$pos$Delta,
                                                             UDLY=.$pos$UDLY,multi=multi,hdd=hdd,
                                                             rlzdvol_td=.$pos$IVIDX*HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$DeltaEffect)->tmp ; process_df$DeltaEffect <- tmp ;rm(tmp)
  # GammaEffect
  process_df %>% rowwise() %>% do(GammaEffect=getGammaEffect(pos=.$pos$Position,greek=.$pos$Gamma,
                                                             UDLY=.$pos$UDLY,multi=multi,hdd=hdd,
                                                             rlzdvol_td=.$pos$IVIDX*HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$GammaEffect)->tmp ; process_df$GammaEffect <- tmp ;rm(tmp)
  # VegaEffect
  process_df %>% rowwise() %>% do(VegaEffect=getVegaEffect(pos=.$pos$Position,greek=.$pos$Vega,
                                                           ividx=.$pos$IVIDX,multi=multi,hdd=hdd,
                                                           #dviv should be precalulated when optimized
                                                           dviv=annuual.daily.volatility(histIV$IVIDX)$daily)) -> tmp
  unlist(tmp$VegaEffect)->tmp ; process_df$VegaEffect <- tmp ;rm(tmp)
  
  return(process_df)
}

#One position's greeks are retuned as a data frame which has only one row.
getPositionGreeks<-function(position,multi,hdd,HV_IV_Adjust_Ratio){
  price<-getPosGreeks(pos=position$Position,greek=position$Price,multi=multi)
  delta<-getPosGreeks(pos=position$Position,greek=position$Delta,multi=multi)
  gamma<-getPosGreeks(pos=position$Position,greek=position$Gamma,multi=multi)
  theta<-getPosGreeks(pos=position$Position,greek=position$Theta,multi=multi)
  vega<-getPosGreeks(pos=position$Position,greek=position$Vega,multi=multi)
  udly<-mean(position$UDLY)
  
  #cat("HV_IV_Adjust_Ratio (getPositionGreeks):",HV_IV_Adjust_Ratio)
  
  thetaEffect<-getThetaEffect(pos=position$Position,greek=position$Theta,multi=multi,hdd=hdd)
  vegaEffect<-getVegaEffect(pos=position$Position,greek=position$Vega,multi=multi,hdd=hdd,
                            ividx=position$IVIDX,dviv=annuual.daily.volatility(histIV$IVIDX)$daily)
  deltaEffect<-getDeltaEffect(pos=position$Position,greek=position$Delta,multi=multi,hdd=hdd,
                              UDLY=position$UDLY,rlzdvol_td=position$IVIDX*HV_IV_Adjust_Ratio)
  
  gammaEffect<-getGammaEffect(pos=position$Position,greek=position$Gamma,multi=multi,hdd=hdd,
                              UDLY=position$UDLY,rlzdvol_td=position$IVIDX*HV_IV_Adjust_Ratio)
  
  data.frame(Price=price,Delta=delta,Gamma=gamma,Theta=theta,Vega=vega,UDLY=udly,
             ThetaEffect=thetaEffect,GammaEffect=gammaEffect,DeltaEffect=deltaEffect,VegaEffect=vegaEffect)

}

