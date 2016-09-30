##
# functions optimized

obj_Income_sgmd <- function(x,Setting,isDebug=FALSE,isDetail=FALSE,
                            udlStepNum,udlStepPct,maxposnum,PosMultip,
                            tail_rate,lossLimitPrice,
                            Delta_Direct_Prf,Vega_Direct_Prf,
                            Delta_Neutral_Offset,Vega_Neutral_Offset){
  #returned when the spread is not appropriate
  unacceptableVal=UNACCEPTABLEVAL
  
  if(isDebug){
    cat("\n###################### eval func start\n")
    cat(":(Delta_Direct_Prf)",Delta_Direct_Prf)
    cat(" :(Vega_Direct_Prf)",Vega_Direct_Prf)
    cat(" :(Delta_Neutral_Offset)",Delta_Neutral_Offset," :(Vega_Neutral_Offset)",Vega_Neutral_Offset)
    if(isDetail==F){cat(" :(holdDays)",Setting$holdDays,"\n")}
  }
  if(isDetail){cat(" :(holdDays)",Setting$holdDays,"\n")}
  
  #position where pos$Position != 0
  position<-hollowNonZeroPosition(pos=x)
  if(isDebug){print(position)}
  
  #At day 0 position price and Greeks.
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip,hdd=Setting$holdDays,HV_IV_Adjust_Ratio=Setting$HV_IV_Adjust_Ratio)
  if(isDebug){cat("initail position\n");print(thePositionGrk)}
  
  #posEvalTble after 1 day and holdDay
  udlStepNum<-Setting$UdlStepNum
  udlStepPct<-Setting$UdlStepPct
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  
  #dATMIV/dIVIDX 1 day regression result
  posStepDays<-data.frame(days=c(1,Setting$holdDays))
  posStepDays %>% dplyr::group_by(days) %>%
    dplyr::do(scene=createPositionEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct,
                                            multi=PosMultip,hdd=.$days,HV_IV_Adjust_Ratio=Setting$HV_IV_Adjust_Ratio)) -> posStepDays
  
  #Use for checking volatility sensitivity later
  posStepDays_vc<-posStepDays
  
  if(isDebug){cat("\n:(1st day evalTble)\n");print(posStepDays$scene[[1]])}
  if(isDebug){print(posStepDays$scene[[1]]$pos)}
  if(isDebug){cat(":(On holdDay evalTble)\n");print(posStepDays$scene[[length(posStepDays)]])}
  if(isDebug){print(posStepDays$scene[[length(posStepDays)]]$pos)}
  
  ##
  #  weighting calculate
  if(sum(EvalFuncSetting$Weight_Explicit)<0.2){
    sd_multp<-Setting$holdDays
    anlzd_sd<-histIV$IVIDX[1]*Setting$HV_IV_Adjust_Ratio
    sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
    sd_hd_1d<-(anlzd_sd/sqrt(252/1))
    #f.y.i sd_hd<-exp(anlzd_sd*sqrt(sd_multp/252))-1 #exponential expressionsum()
    weight<-dsn(udlChgPct,xi=sd_multp/252*Setting$Weight_Drift,alpha=Setting$Weight_Skew*sd_hd,omega=sd_hd) /
      sum(dsn(udlChgPct,xi=sd_multp/252*Setting$Weight_Drift,alpha=Setting$Weight_Skew*sd_hd,omega=sd_hd))
    weight_Effect_hd<-dsn(udlChgPct,xi=sd_multp/252*Setting$Weight_Drift_GreekEffect,alpha=Setting$Weight_Skew_GreekEffect*sd_hd,omega=sd_hd) /
      sum(dsn(udlChgPct,xi=sd_multp/252*Setting$Weight_Drift_GreekEffect,alpha=Setting$Weight_Skew_GreekEffect*sd_hd,omega=sd_hd))
    weight_Effect_1d<-dsn(udlChgPct,xi=1/252*Setting$Weight_Drift_GreekEffect,alpha=Setting$Weight_Skew_GreekEffect*sd_hd_1d,omega=sd_hd_1d) /
      sum(dsn(udlChgPct,xi=1/252*Setting$Weight_Drift_GreekEffect,alpha=Setting$Weight_Skew_GreekEffect*sd_hd_1d,omega=sd_hd_1d))
    #average weighted weight_Effect
    weight_Effect=weight_Effect_1d*Setting$GreekEfctOnHldD+weight_Effect_hd*(1-Setting$GreekEfctOnHldD)
  }else{
    weight<-Setting$Weight_Explicit
    weight_Effect<-Setting$Weight_Explicit_1D*Setting$GreekEfctOnHldD+Setting$Weight_Explicit*(1-Setting$GreekEfctOnHldD)
    #weight_Effect=weight_Effect/sum(weight_Effect)
  }
  
  if(isDetail){
    cat(" :(weight hdday)",weight)#;cat(" :(weightEffect hdday)",weight_Effect_hd);cat(" :(weightEffect 1ay)",weight_Effect_1d)
    cat(" :(weightEffect)",weight_Effect)
  }
  
  ##
  # Volatility Senstivity 
  #integrate(f <- function(x) abs(x)*dnorm(x,0,1),-100,100) == 0.7978846
  # Daily Volatility of Implied Volatility
  dviv = annuual.daily.volatility(histIV$IVIDX)$daily
  dviv = dviv*Setting$ExpIVChange_Multiple
  dviv_forExp = dviv*0.7978846
  #Calculate Expected IV change
  expIVChange<-histIV$IVIDX[1]*(exp(dviv_forExp*sqrt(Setting$holdDays))-1)
  
  ##
  #  vertical (Implied Volatility) weight
  #IVChgPct<-seq(histIV$IVIDX[1]-expIVChange,histIV$IVIDX[1]+expIVChange,length=3)
  #sd_iv=histIV$IVIDX[1]*(exp(dviv*sqrt(Setting$holdDays))-1)*sqrt(1-cor_tmp*cor_tmp)
  sd_iv=histIV$IVIDX[1]*(exp(dviv*sqrt(Setting$holdDays))-1)
  IVChgPct<-seq(histIV$IVIDX[1]-sd_iv,histIV$IVIDX[1]+sd_iv,length=3)
  #weigting
  weight_IV=dnorm(IVChgPct,mean=histIV$IVIDX[1],sd=sd_iv)/sum(dnorm(IVChgPct,mean=histIV$IVIDX[1],sd=sd_iv))
  
  if(isDetail){cat(" :(weight_IV)",weight_IV)}
  
  #Implied Volatility Change percent
  #make sure this is CONDITIONAL. given the price change(and regressed IV change), calculate additional volatility change
  #if correlation is 1, the vol_chg should be 0.
  cor_tmp=get.Volatility.Level.Regression(Days=Setting$holdDays)$cor
  vol_chg=expIVChange*sqrt(1-cor_tmp*cor_tmp)/histIV$IVIDX[1]
  #vol_chg=vol_chg*Setting$ExpIVChange_Multiple
  if(isDetail){cat(" :(IV change%)",vol_chg)}
  
  #PositionStepDays data frame of Volatility UP scenario
  posStepDays_vc_plus<-posStepDays_vc
  posStepDays_vc_plus %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% 
    dplyr::do(days=.$days,scene2=adjustPosChg(.$scene,base_vol_chg=vol_chg,
                                              multi=PosMultip,hdd=Setting$holdDays,HV_IV_Adjust_Ratio=Setting$HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$days) -> posStepDays_vc_plus$days ; tmp$scene2 -> posStepDays_vc_plus$scene ;rm(tmp)
  
  #PositionStepDays data frame of Volatility Down scenario
  posStepDays_vc_minus<-posStepDays_vc
  posStepDays_vc_minus %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% 
    dplyr::do(days=.$days,scene2=adjustPosChg(.$scene,base_vol_chg=(-1)*vol_chg,
                                              multi=PosMultip,hdd=Setting$holdDays,HV_IV_Adjust_Ratio=Setting$HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$days) -> posStepDays_vc_minus$days ; tmp$scene2 -> posStepDays_vc_minus$scene ;rm(tmp)
  
  ##
  # Profit
  
  ##
  # Estimated Profit
  posEvalTbl<-posStepDays$scene[[length(posStepDays)]]
  profit_vector<-(posEvalTbl$Price-posStepDays$scene[[1]]$Price[udlStepNum + 1])
  profit_hdays<-sum(profit_vector*weight)
  if(isDetail){cat(" :(1stD price",posStepDays$scene[[1]]$Price[udlStepNum + 1]);cat(" :(prft_vec)",profit_vector);cat(" :(prft_wght)",profit_hdays)}
  
  ##
  # Profit Scenario When IV goes Up
  posEvalTbl<-posStepDays_vc_plus$scene[[length(posStepDays)]]
  profit_vector_vc_plus<-(posEvalTbl$Price-posStepDays$scene[[1]]$Price[udlStepNum + 1])
  profit_hdays_vc_plus<-sum(profit_vector_vc_plus*weight)
  if(isDetail){cat(" :(prft_vec_vc+)",profit_vector_vc_plus);cat(" :(prft_wght_vc+)",profit_hdays_vc_plus)}
  
  ##
  # Profit Scenario When IV goes Down
  posEvalTbl<-posStepDays_vc_minus$scene[[length(posStepDays)]]
  profit_vector_vc_minus<-(posEvalTbl$Price-posStepDays$scene[[1]]$Price[udlStepNum + 1])
  profit_hdays_vc_minus<-sum(profit_vector_vc_minus*weight)
  
  #Initial Delta
  iniDelta <- posStepDays$scene[[1]]$Delta[udlStepNum + 1]
  if(isDetail){cat(" :(iniDelta)",iniDelta);cat(" :(prft_vec_vc-)",profit_vector_vc_minus);cat(" :(prft_wght_vc-)",profit_hdays_vc_minus)}
  
  ## Delta Hedge
  if(Setting$DeltaHedge){
    profit_vector<-profit_vector-as.numeric(iniDelta)*(posEvalTbl$UDLY-mean(thePositionGrk$UDLY))
    profit_hdays<-sum(profit_vector*weight)
    if(isDetail){cat(" :(iniDelta)",iniDelta);cat(" :(prft_vec_dh)",profit_vector);cat(" :(prft_wght_dh)",profit_hdays)}
    
    profit_vector_vc_plus<-profit_vector_vc_plus-as.numeric(iniDelta)*(posEvalTbl$UDLY-mean(thePositionGrk$UDLY))
    profit_hdays_vc_plus<-sum(profit_vector_vc_plus*weight)
    if(isDetail){cat(" :(prft_vec_dh_vc+)",profit_vector_vc_plus);cat(" :(prft_wght_dh_vc+)",profit_hdays_vc_plus)}
    
    profit_vector_vc_minus<-profit_vector_vc_minus-as.numeric(iniDelta)*(posEvalTbl$UDLY-mean(thePositionGrk$UDLY))
    profit_hdays_vc_minus<-sum(profit_vector_vc_minus*weight)
    if(isDetail){cat(" :(prft_vec_dh_vc-)",profit_vector_vc_minus);cat(" :(prft_wght_dh_vc-)",profit_hdays_vc_minus)}
  }
  
  ##
  #  sd and max_loss
  
  # maxLoss
  maxLoss<-min(c(profit_vector,profit_vector_vc_plus,profit_vector_vc_minus))
  
  ## MaxLoss Constraint
  if(maxLoss<Setting$LossLimitPrice){
    if(isDetail){
      cat(" :(max_loss)",maxLoss);cat(" :(loss_limit_price)",Setting$LossLimitPrice)
    }
    return(unacceptableVal)
  }
  
  # calculating sd
  pdist<-c(rep(rep(profit_vector,times=round(weight*100)),times=round(weight_IV*100)[2]),
           rep(rep(profit_vector_vc_plus,times=round(weight*100)),times=round(weight_IV*100)[3]),
           rep(rep(profit_vector_vc_minus,times=round(weight*100)),times=round(weight_IV*100)[1]))
  
  if(Setting$UseSortinoRatio){
    profit_sd=sd(pdist[pdist<0])
  }else{
    profit_sd<-sd(pdist)
  }
  
  if(isDetail){
    cat(" :(sd_SortinoR)",sd(pdist[pdist<0]));cat(" :(sd)",sd(pdist)); cat(" :(profit_sd)",profit_sd)
    cat(" :(max_loss)",maxLoss);cat(" :(loss_limit_price)",Setting$LossLimitPrice)
  }
  
  ## c3 profit
  c3<- sum(c(profit_hdays_vc_minus,profit_hdays,profit_hdays_vc_plus)*weight_IV)
  
  # ROIC
  profit_expctd=c3
  ROIC=1.0
  if(profit_expctd<0){
    ROIC=0.0
  }else if(maxLoss<0){
    ROIC=profit_expctd/(-maxLoss)
  }
  
  ## c8 metric
  c8<- profit_sd
  if(Setting$EvalConvex)
    c8<- (-1)*maxLoss
  else
    c8<- profit_sd
  if(isDetail){cat(" :c8(profit_sd or max_loss)",c8)}
  
  ##
  # Greek Effects calculations. Forward looking indicator. Use first day's posEvalTble.
  
  ##
  # True Vega and Vomma Effect
  VegaEffectWithSign= (profit_hdays_vc_plus-profit_hdays_vc_minus)/2
  VommaEffect=((profit_hdays_vc_plus + profit_hdays_vc_minus)/2)-profit_hdays
  Vega_True=VegaEffectWithSign/(expIVChange*100)
  if(isDetail){cat(" :(Vega)",Vega_True," :(VegaEffectWithSign)",VegaEffectWithSign," :(VommaEffect)",VommaEffect)}
  
  ## Greek Effect calculation Scene
  #  default first day
  posEvalTbl<-posStepDays$scene[[1]]
  #  default holding day
  #posEvalTbl<-posStepDays$scene[[length(posStepDays)]]
  #not default, averaging 1st Day and holdDay
  if(isDetail){cat(" :(GreekEfctOnHldD)",Setting$GreekEfctOnHldD)}
  if(Setting$GreekEfctOnHldD<1){
    posEvalTbl_1<-posStepDays$scene[[1]]
    posEvalTbl_hd<-posStepDays$scene[[length(posStepDays)]]
    #weighting
    posEvalTbl$Delta=posEvalTbl_1$Delta*Setting$GreekEfctOnHldD+posEvalTbl_hd$Delta*(1-Setting$GreekEfctOnHldD)
    posEvalTbl$Gamma=posEvalTbl_1$Gamma*Setting$GreekEfctOnHldD+posEvalTbl_hd$Gamma*(1-Setting$GreekEfctOnHldD)
    posEvalTbl$Vega=posEvalTbl_1$Vega*Setting$GreekEfctOnHldD+posEvalTbl_hd$Vega*(1-Setting$GreekEfctOnHldD)
    posEvalTbl$Theta=posEvalTbl_1$Theta*Setting$GreekEfctOnHldD+posEvalTbl_hd$Theta*(1-Setting$GreekEfctOnHldD)
    posEvalTbl$DeltaEffect=posEvalTbl_1$DeltaEffect*Setting$GreekEfctOnHldD+posEvalTbl_hd$DeltaEffect*(1-Setting$GreekEfctOnHldD)
    posEvalTbl$GammaEffect=posEvalTbl_1$GammaEffect*Setting$GreekEfctOnHldD+posEvalTbl_hd$GammaEffect*(1-Setting$GreekEfctOnHldD)
    posEvalTbl$VegaEffect=posEvalTbl_1$VegaEffect*Setting$GreekEfctOnHldD+posEvalTbl_hd$VegaEffect*(1-Setting$GreekEfctOnHldD)
    posEvalTbl$ThetaEffect=posEvalTbl_1$ThetaEffect*Setting$GreekEfctOnHldD+posEvalTbl_hd$ThetaEffect*(1-Setting$GreekEfctOnHldD)
    posEvalTbl$IVIDX=posEvalTbl_1$IVIDX*Setting$GreekEfctOnHldD+posEvalTbl_hd$IVIDX*(1-Setting$GreekEfctOnHldD)
  }
  
  ##
  # Constraint 4. ThetaEffect. This should be soft constraint
  if(Setting$ThetaEffectPositive){
    theta_ttl<-sum(posEvalTbl$Theta*weight_Effect)
    if(isDetail){cat(" :(thta_ttl)",theta_ttl)}
    if(isDetail){cat(" :(thta_ini)",thePositionGrk$ThetaEffect);cat(" :(thta_wt)",sum(posEvalTbl$ThetaEffect*weight_Effect))}
    if(theta_ttl<0)
      return(unacceptableVal)
  }
  
  ##
  # Advantageous Effects.
  c5<- sum((posEvalTbl$GammaEffect+posEvalTbl$ThetaEffect)*weight_Effect)+VommaEffect
  if(isDetail){cat(" :(GammaEffect)",sum(posEvalTbl$GammaEffect*weight_Effect)," :(ThetaEffect)",sum(posEvalTbl$ThetaEffect*weight_Effect),
                   " :c5(AdvEffect_wght)",c5)}
  
  ##
  # Directional Effects.
  ##
  
  ##
  #    Delta
  
  ##Delta_Neutral_Offset
  expPriceChange <- getExpectedValueChange(base=posEvalTbl$UDLY,sd=posEvalTbl$IVIDX*Setting$HV_IV_Adjust_Ratio, dtime=Setting$holdDays/252)
  Delta_revised_offset<-posEvalTbl$Delta-Delta_Neutral_Offset
  Delta_Effect_revised_offset<- (-abs(Delta_revised_offset))*expPriceChange
  if(isDetail){cat(" :(expPriceChange)",expPriceChange," :(Delta Offset)",Delta_revised_offset," :(DeltaE offset)",Delta_Effect_revised_offset)}
  
  Delta_revised_offset<-sum(Delta_revised_offset*weight_Effect)
  Delta_Effect_revised_offset<-sum(Delta_Effect_revised_offset*weight_Effect)
  if(isDetail){cat(" :(DeltaE_wght)",Delta_Effect_revised_offset," :(Delta_wght)",Delta_revised_offset)}
  
  ###Delta_Thresh_Minus,Delta_Thresh_Plus
  DeltaEffect_Comp<-(Delta_revised_offset<0)*(Delta_revised_offset<Setting$Delta_Thresh_Minus[length(position$TYPE)])*Delta_Effect_revised_offset+
    (Delta_revised_offset>0)*(Delta_revised_offset>Setting$Delta_Thresh_Plus[length(position$TYPE)])*Delta_Effect_revised_offset
  
  if(isDetail){
    cat(" :betwn (Delta_Thresh_Minus)",Setting$Delta_Thresh_Minus[length(position$TYPE)],
        " and (Delta_Thresh_Plus)",Setting$Delta_Thresh_Plus[length(position$TYPE)])
    cat(" :(new DeltaE_wght)",DeltaEffect_Comp)
  }
  
  ##
  #    Vega
  
  VegaEffect_Comp = (-1)*abs(VegaEffectWithSign)
  Vega_revised_offset=Vega_True
  
  ##
  # Delta_Direct_Prf, Vega_Direct_Prf reflected as coef
  dlta_pref_coef<-(Delta_Direct_Prf==0)*(-1)+
    (Delta_Direct_Prf>0)*(Delta_revised_offset>=0)+(Delta_Direct_Prf>0)*(Delta_revised_offset<0)*(-1)+
    (Delta_Direct_Prf<0)*(Delta_revised_offset>=0)*(-1)+(Delta_Direct_Prf<0)*(Delta_revised_offset<0)
  
  vega_pref_coef<-(Vega_Direct_Prf==0)*(-1)+
    (Vega_Direct_Prf>0)*(Vega_revised_offset>=0)+(Vega_Direct_Prf>0)*(Vega_revised_offset<0)*(-1)+
    (Vega_Direct_Prf<0)*(Vega_revised_offset>=0)*(-1)+(Vega_Direct_Prf<0)*(Vega_revised_offset<0)
  
  ##
  # Directional Effect
  c6<- vega_pref_coef*VegaEffect_Comp+dlta_pref_coef*DeltaEffect_Comp
  if(isDetail){
    cat(" :(vega_pref_coef",vega_pref_coef," x :VegaE_new",VegaEffect_Comp,
        "+ :dlta_pref_coef",dlta_pref_coef," x :DeltaE_new",DeltaEffect_Comp," = :(DrctlEffect)c6 ",c6,")")
  }
  ##
  # cost7 All Effects.
  c7<- c5+c6
  if(isDetail){cat(" :c7(AllEffect)",c7)}
  
  ##
  # total cost is weighted sum of each cost.
  A<-Setting$DrctlEffect_Coef*c6 + Setting$AllEffect_Coef*c7 + Setting$MaxLoss_Coef*c8
  B<-Setting$AdvEffect_Coef*c5+Setting$Profit_Coef*c3
  
  if(isDetail){cat(" :Coef_Drct",Setting$DrctlEffect_Coef,"x",c6,"+:Coef_AllE",Setting$AllEffect_Coef,"x",c7,"+ :Coef_MaxLoss(SD)",Setting$MaxLoss_Coef,"x",c8,"= Numr",A)}
  if(isDetail){cat(" :Coef_Adv",Setting$AdvEffect_Coef,"x",c5,"+:Coef_Prft",Setting$Profit_Coef,"x",c3,"= Denom",B)}
  
  cost=unacceptableVal
  if(Setting$EvalSigmoidFunc==TRUE){
    sigA<-sigmoid(A,a=Setting$SigmoidA_Numerator,b=0)
    sigB<-sigmoid(B,a=Setting$SigmoidA_Denominator,b=0)
    cost<-sigA/sigB
    if(isDetail){cat(" :(sigA)",sigA,":(sigB)",sigB," :cost(sigA/sigB)",cost)}
  }else{
    if(Setting$EvalEconomicValue){
      if(B>0)
        cost=(A-B)
    }else{
      if(B>0)
        cost=((A-B)/B)
    }
    if(isDetail){cat(" :cost",cost)}
  }
  
  ##
  # total cost and penalty
  val<-cost 
  
  if(isDetail){
    ROIC_anlzd=ROIC*252/Setting$holdDays
    cat(" :(val)",val,"\n");cat(" :(exp prft)",profit_expctd," :(maxloss):",maxLoss," :(ROIC)",ROIC," :(ROIC anlzd)",ROIC_anlzd,"\n")
  }
  return(val)
}

#Rsk/Rtn greek related functions
#get the position's total greek
getPosGreeks<-function(pos,greek,multi){
  pos_greek<-sum(pos*multi*greek)
  pos_greek
}

###
##  getGreekEffects

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


##
#  Factory of Volatility Level Regression Result
get.Volatility.Level.Regression<-function(Days=holdDays,ctoc=TRUE){
  daysCandidate=c(1,3,5,7,12,18)
  daysdiff=abs(c(daysCandidate)-Days)
  days_idx=min(which( abs(daysdiff) == min(daysdiff)))
  Days=daysCandidate[days_idx]
  if(Days==1){
    return(PC1dCtC_IVCF1dCtC)
  } else if(Days==3){
    return(PC3dCtC_IVCF3dCtC)
  }else if(Days==5){
    return(PC5dCtC_IVCF5dCtC)
  }else if(Days==7){
    return(PC7dCtC_IVCF7dCtC)
  }else if(Days==12){
    return(PC12dCtC_IVCF12dCtC)
  }else if(Days==18){
    return(PC18dCtC_IVCF18dCtC)
  }
}

##
# ATMIV Volatility Change Regression

#read from get.Volatility.Change.Regression.Result to get specific regression values.
get.VolChg<-function(model,month){
  chg<-predict(model,x=month)
  return(chg)
}

#get VC.f(=ATMIV.f/IVIDX.f) as a vector for each position element.
# where ATMIV.f=ATMIV_pos/ATMIV_pre, IVIDX.f= IVIDX_pos/IVIDX_pre. 
# VC.f also depends on the option's TYPE, whether IVIDX went up or down, and TimeToExpPdate.
get.Volatility.Change.Regression.Result<-function(pos,up_dn){
  atmiv_chg<-(pos$TYPE==OpType_Put_G)*(up_dn>=0)*
    (get.VolChg(model=PutIVChgUp,month=pos$TimeToExpDate))$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Call_G)*(up_dn>=0)*
    (get.VolChg(model=CallIVChgUp,month=pos$TimeToExpDate))$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Put_G)*(up_dn<0)*
    (get.VolChg(model=PutIVChgDown,month=pos$TimeToExpDate))$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Call_G)*(up_dn<0)*
    (get.VolChg(model=CallIVChgDown,month=pos$TimeToExpDate))$y
  atmiv_chg
}

##
# ATMIV Volatility Change Regression Advanced

save.ATMIV.f.IVIDX.f <- function (model,optype,up_dn,days,x_idx,y_idx) {
  reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_",
                      ifelse(optype==OpType_Call_G,"Call","Put"),
                      ifelse(up_dn>=0,"IVUp_","IVDown_"),
                      "ATMIV.f.IVIDX.f_",days,"D",sep="")
  reg_saved_names<-c("model","days","x","y")
  reg_saved <- vector("list",length(reg_saved_names))
  reg_saved[[1]]<-model
  reg_saved[[2]]<-days
  reg_saved[[3]]<-x_idx
  reg_saved[[4]]<-y_idx
  names(reg_saved)=reg_saved_names
  
  #saved file name.
  save(reg_saved,file=reg_saved_fn)  
}

load.ATMIV.f.IVIDX.f <- function (optype,up_dn,days) {
  load_fn_suffix=paste(ifelse(optype==OpType_Call_G,"Call","Put"),
                       ifelse(up_dn>=0,"IVUp_","IVDown_"),
                       "ATMIV.f.IVIDX.f_",days,"D",sep="")
  
  reg_load_fn <- paste(DataFiles_Path_G,Underying_Symbol_G,"_",
                       load_fn_suffix,
                       sep="")
  load(reg_load_fn)
  assign(load_fn_suffix, reg_saved,env=.GlobalEnv)
}

get.ATMIV.f.VolChg<-function(model,days,hdd,ividx.f,x_idx,y_idx,month){
  y<-predict(model,x=month)$y
  #cat("regressed value",y,"\n")
  #cat("ividx.f",ividx.f,"holdDay",hdd,"month",month,"\n")
  
  ATMIV.f = y*((ividx.f)^(x_idx))*(month^(y_idx))
  
  imp_diff=ividx.f-ATMIV.f
  imp_diff=imp_diff/((hdd/1)^(0.5))
  ATMIV.f.rev=ATMIV.f+imp_diff
  
  #cat("ATMIV.f(naive estm)",ATMIV.f,"ATMIV.f(hdd estm)",ATMIV.f.rev,"\n")
  #return(ATMIV.f)
  return(ATMIV.f.rev)
}

get.ATMIV.f_1D.Regression.Result<-function(pos,up_dn,days,hdd,ividx.f){
  atmiv_chg=(pos$TYPE==OpType_Put_G)*(up_dn>=0)*
    (get.ATMIV.f.VolChg(model=PutIVUp_ATMIV.f.IVIDX.f_1D$model,
                        days=1,
                        hdd=hdd,
                        ividx.f=ividx.f,
                        x_idx=PutIVUp_ATMIV.f.IVIDX.f_1D$x,
                        y_idx=PutIVUp_ATMIV.f.IVIDX.f_1D$y,
                        month=pos$TimeToExpDate))
  atmiv_chg=atmiv_chg+
    (pos$TYPE==OpType_Call_G)*(up_dn>=0)*
    (get.ATMIV.f.VolChg(model=CallIVUp_ATMIV.f.IVIDX.f_1D$model,
                        days=1,
                        hdd=hdd,
                        ividx.f=ividx.f,
                        x_idx=CallIVUp_ATMIV.f.IVIDX.f_1D$x,
                        y_idx=CallIVUp_ATMIV.f.IVIDX.f_1D$y,
                        month=pos$TimeToExpDate))
  atmiv_chg=atmiv_chg+
    (pos$TYPE==OpType_Put_G)*(up_dn<0)*
    (get.ATMIV.f.VolChg(model=PutIVDown_ATMIV.f.IVIDX.f_1D$model,
                        days=1,
                        hdd=hdd,
                        ividx.f=ividx.f,
                        x_idx=PutIVDown_ATMIV.f.IVIDX.f_1D$x,
                        y_idx=PutIVDown_ATMIV.f.IVIDX.f_1D$y,
                        month=pos$TimeToExpDate))
  atmiv_chg=atmiv_chg+
    (pos$TYPE==OpType_Call_G)*(up_dn<0)*
    (get.ATMIV.f.VolChg(model=CallIVDown_ATMIV.f.IVIDX.f_1D$model,
                        days=1,
                        hdd=hdd,
                        ividx.f=ividx.f,
                        x_idx=CallIVDown_ATMIV.f.IVIDX.f_1D$x,
                        y_idx=CallIVDown_ATMIV.f.IVIDX.f_1D$y,
                        month=pos$TimeToExpDate))
  atmiv_chg
}

##
# Volatility Cone

#read from get.Volatility.Cone.Regression.Result to get specific regression values.
get.VCone<-function(model,month){
  cone<-predict(model,x=month)
  return(cone)
}

#get the regression result as a vector for each position element.
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

# ATM IV behavior
save.ATMIDXIV.f<-function(model,optype){
  if(optype==OpType_Put_G){
    reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PUT_ATMIDXIV.f",sep="")
  }else if(optype==OpType_Call_G){
    reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CALL_ATMIDXIV.f",sep="")
  }
  save(model,file=reg_saved_fn)
}

load.ATMIDXIV.f<- function(optype) {
  if(optype==OpType_Put_G){
    reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PUT_ATMIDXIV.f",sep="")
    load(reg_load_fn)
    assign("PUT_ATMIDXIV.f",model,env=.GlobalEnv)
  }else if(optype==OpType_Call_G){
    reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CALL_ATMIDXIV.f",sep="")
    load(reg_load_fn)
    assign("CALL_ATMIDXIV.f",model,env=.GlobalEnv)
  }
}

get.Volatility.Change.Regression.Result.ATMIDXIV.f<-function(pos,pos_TimeToExpDate){
  atmiv_chg<-(pos$TYPE==OpType_Put_G)*predict(PUT_ATMIDXIV.f,x=pos_TimeToExpDate)$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Call_G)*predict(CALL_ATMIDXIV.f,x=pos_TimeToExpDate)$y
  atmiv_chg
}

##
# hollowing NonZero Position from option chain(Opchain)
hollowNonZeroPosition<-function(pos){
  #opchain is global parameter. to avoid unnessary copying
  opchain$Position<-pos
  opchain %>% dplyr::filter(Position!=0) -> position
  position
}

##
#  Creation of Position Evaluation Table

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
  #pos$ATMIV<-pos$ATMIV*(1+ividx_chg_pct)*get.Volatility.Change.Regression.Result(pos,ividx_chg_pct)
  
  #Volatility Cone の影響。時間変化した分の影響を受ける。その比の分だけ比率変化
  #ATMIV_pos <- ATMIV_pos*(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pre/(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pos
  bdays_per_month<-252/12
  TimeToExpDate_pos<-(pos$TimeToExpDate*bdays_per_month-days)/bdays_per_month
  #volatility cone logic is included in the ATM change behavior below
  #pos$ATMIV<-pos$ATMIV *
  #  get.Volatility.Cone.Regression.Result(pos$TYPE,TimeToExpDate_pos)/
  #  get.Volatility.Cone.Regression.Result(pos$TYPE,pos$TimeToExpDate)
  
  ##This is the new ATMIV behavior
  
  # cat("########## ividx.f",(1+ividx_chg_pct),"holdDay",EvalFuncSetting$holdDays,"\n")
  # cat("TimeToExpDate",pos$TimeToExpDate,"\n")
  # cat("TYPE",pos$TYPE,"\n")
  # cat("regressed",get.ATMIV.f_1D.Regression.Result(pos,up_dn=ividx_chg_pct,days=1,hdd=EvalFuncSetting$holdDays,ividx.f=(1+ividx_chg_pct)),"\n")
  # cat("ATMIV pre",pos$ATMIV,"\n")
  pos$ATMIV<-pos$ATMIV*
    get.ATMIV.f_1D.Regression.Result(pos,up_dn=ividx_chg_pct,days=1,hdd=EvalFuncSetting$holdDays,ividx.f=(1+ividx_chg_pct))*
    get.Volatility.Change.Regression.Result.ATMIDXIV.f(pos,TimeToExpDate_pos)/get.Volatility.Change.Regression.Result.ATMIDXIV.f(pos,pos$TimeToExpDate)
  # cat("ATMIV pos",pos$ATMIV,"\n")
  
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


createPositionEvalTable<-function(position,udlStepNum,udlStepPct,multi,hdd,HV_IV_Adjust_Ratio){
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-data.frame(udlChgPct=udlChgPct) ;rm(udlStepNum,udlStepPct)
  #Set data frames as a row value of another data frame.
  posEvalTbl %>% group_by(udlChgPct) %>% do(pos=position) -> posEvalTbl
  #Modify pos based on scenario
  posEvalTbl %>% group_by(udlChgPct) %>% do(pos=reflectPosChg(process_df=.,days=hdd)) -> posEvalTbl
  
  ##
  #  Greek Effects
  
  #  ThetaEffect
  posEvalTbl %>% dplyr::rowwise() %>% 
    dplyr::do(ThetaEffect=getThetaEffect(pos=.$pos$Position,greek=.$pos$Theta,multi=multi,hdd=hdd)) -> tmp
  unlist(tmp$ThetaEffect)->tmp ; posEvalTbl$ThetaEffect <- tmp ;rm(tmp)
  #  DeltaEffect
  posEvalTbl %>% rowwise() %>% dplyr::do(DeltaEffect=getDeltaEffect(pos=.$pos$Position,greek=.$pos$Delta,
                                                                    UDLY=.$pos$UDLY,multi=multi,hdd=hdd,
                                                                    rlzdvol_td=.$pos$IVIDX*HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$DeltaEffect)->tmp ; posEvalTbl$DeltaEffect <- tmp ;rm(tmp)
  #  GammaEffect
  posEvalTbl %>% rowwise() %>% dplyr::do(GammaEffect=getGammaEffect(pos=.$pos$Position,greek=.$pos$Gamma,
                                                                    UDLY=.$pos$UDLY,multi=multi,hdd=hdd,
                                                                    rlzdvol_td=.$pos$IVIDX*HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$GammaEffect)->tmp ; posEvalTbl$GammaEffect <- tmp ;rm(tmp) 
  #  VegaEffect
  posEvalTbl %>% rowwise() %>% dplyr::do(VegaEffect=getVegaEffect(pos=.$pos$Position,greek=.$pos$Vega,
                                                                  ividx=.$pos$IVIDX,multi=multi,hdd=hdd,
                                                                  #dviv should be precalulated when optimized
                                                                  dviv=annuual.daily.volatility(histIV$IVIDX)$daily)) -> tmp
  unlist(tmp$VegaEffect)->tmp ; posEvalTbl$VegaEffect <- tmp ;rm(tmp)
  
  ##
  #  Greeks
  #
  #  UDLY
  posEvalTbl %>% dplyr::rowwise() %>% dplyr::do(UDLY=mean(.$pos$UDLY)) ->tmp
  unlist(tmp$UDLY)->tmp ; posEvalTbl$UDLY <- tmp ;rm(tmp)
  #  Price
  posEvalTbl %>% dplyr::rowwise() %>% dplyr::do(Price=getPosGreeks(pos=.$pos$Position,greek=.$pos$Price,multi=multi)) ->tmp
  unlist(tmp$Price)->tmp ; posEvalTbl$Price <- tmp ;rm(tmp)
  #  Delta
  posEvalTbl %>% dplyr::rowwise() %>% dplyr::do(Delta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Delta,multi=multi))->tmp
  unlist(tmp$Delta)->tmp ; posEvalTbl$Delta <- tmp ;rm(tmp)
  #  Gamma
  posEvalTbl %>% dplyr::rowwise() %>% dplyr::do(Gamma=getPosGreeks(pos=.$pos$Position,greek=.$pos$Gamma,multi=multi))->tmp
  unlist(tmp$Gamma)->tmp ; posEvalTbl$Gamma <- tmp ;rm(tmp)
  #  Vega
  posEvalTbl %>% dplyr::rowwise() %>% dplyr::do(Vega=getPosGreeks(pos=.$pos$Position,greek=.$pos$Vega,multi=multi))->tmp
  unlist(tmp$Vega)->tmp ; posEvalTbl$Vega <- tmp ;rm(tmp)
  #  Theta
  posEvalTbl %>% dplyr::rowwise() %>% dplyr::do(Theta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Theta,multi=multi)) ->tmp
  unlist(tmp$Theta)->tmp ; posEvalTbl$Theta <- tmp ;rm(tmp)
  # IVIDX
  posEvalTbl %>% dplyr::rowwise() %>% dplyr::do(IVIDX=mean(.$pos$IVIDX)) ->tmp
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
# Sampling related functions

sampleVerticalSpread<-function(targetOpTyep,verticalType,targetExpDate,
                               isDebug=FALSE,isDetail=FALSE){
  
  idxy<-as.numeric(opchain$TYPE==targetOpTyep)*
    as.numeric(opchain$ExpDate==targetExpDate)*rep(1:length(opchain$TYPE),length=length(opchain$TYPE))
  if(isDebug){ cat(" candidate pos:",idxy) }
  y<-rep(0,times=length(opchain$TYPE))
  if(sum(idxy)>0){
    idxy<-idxy[idxy!=0]
    #if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=2,replace=FALSE,prob=NULL)
    opchain$Strike[idxy[1]]
    opchain$Strike[idxy[2]]
    (opchain$Strike[idxy[2]]>=opchain$Strike[idxy[1]])*(verticalType==BULL_VERTICAL_SPREAD_TYPE)*idxy +
      (opchain$Strike[idxy[2]]<opchain$Strike[idxy[1]])*(verticalType==BULL_VERTICAL_SPREAD_TYPE)*rev(idxy) +
      (opchain$Strike[idxy[2]]>=opchain$Strike[idxy[1]])*(verticalType==BEAR_VERTICAL_SPREAD_TYPE)*rev(idxy) +
      (opchain$Strike[idxy[2]]<opchain$Strike[idxy[1]])*(verticalType==BEAR_VERTICAL_SPREAD_TYPE)*idxy ->idxy
    if(isDebug){ cat("idxy:",idxy) }
    #y<-rep(0,times=length(iniPos))
    y_shift<-0
    n_shift<-(2)
    y[idxy[(1+y_shift):(y_shift+(n_shift/2))]]<-(1)
    y[idxy[(n_shift/2+1+y_shift):2]]<-(-1)
  }
  if(isDebug){ cat(" (:pos",y,")") }
  return(y)
}

sampleDiagonalSpread<-function(targetOpTyep,diagonalType,targetExpDate_f,targetExpDate_b,
                               isDebug=FALSE,isDetail=FALSE){
  #Front
  idxy<-as.numeric(opchain$TYPE==targetOpTyep)*
    as.numeric(opchain$ExpDate==targetExpDate_f)*rep(1:length(opchain$TYPE),length=length(opchain$TYPE))
  y<-rep(0,times=length(opchain$TYPE))
  if(sum(idxy)>0){
    idxy<-idxy[idxy!=0]
    #if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=1,replace=T,prob=NULL)
    #y<-rep(0,times=length(iniPos))
    y[idxy[1]]<-as.numeric(diagonalType==DIAGONAL_TYPE_LONG)*(-1) +
      as.numeric(diagonalType==DIAGONAL_TYPE_SHORT)*(1)
  }
  #Back
  idxy<-as.numeric(opchain$TYPE==targetOpTyep)*
    as.numeric(opchain$ExpDate==targetExpDate_b)*rep(1:length(opchain$TYPE),length=length(opchain$TYPE))
  z<-rep(0,times=length(iniPos))
  if(sum(idxy)>0){
    idxy<-idxy[idxy!=0]
    #if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=1,replace=T,prob=NULL)
    #y<-rep(0,times=length(iniPos))
    z[idxy[1]]<-as.numeric(diagonalType==DIAGONAL_TYPE_LONG)*(1) +
      as.numeric(diagonalType==DIAGONAL_TYPE_SHORT)*(-1)
  }
  x<-y+z
  
  if(isDebug){ cat(" (:pos",x,")") }
  return(x)
}

## load file info into Global HashT
loadToPositionHash<-function(fname){
  tmp<-read.table(fname,header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp=tmp[,1:(length(opchain$Position)+1)]
  colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
  tmp %>% dplyr::rowwise() %>%
    # x = unlist(.)[1:length(opchain$Position)]
    dplyr::do(key=paste(unlist(.)[1:length(opchain$Position)],collapse = ""),
              md5sum=digest(paste(unlist(.)[1:length(opchain$Position)],collapse = ""))) -> tmp2
  POSITION_OPTIM_HASH[ unlist(tmp2$md5sum) ]<<-tmp$eval
}

## sampling main routine
sampleMain<-function(sampleSpreadType,totalPopNum,targetExpDate,targetExpDate_f,targetExpDate_b,
                     spreadRatio,InitialPopThresh,outFname,isFileout=T,isDebug=F,isDetail=F,
                     POSITION_HASH=hash()){
  added_num=0
  total_count=0
  hash_hit_num=0
  s1_idx=0
  s2_idx=0
  start_t<-proc.time()
  #POSITION_HASH=hash()
  while(TRUE){
    x<-rep(0,times=length(opchain$TYPE))
    if(sampleSpreadType==IRON_CONDOR_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      x<-y+z
      x<-x*spreadRatio[1]
    }else if(sampleSpreadType==DOUBLE_DIAGONAL_SMPLING){
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      y=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-(y+z)*spreadRatio[1]
    }else if(sampleSpreadType==DOUBLE_DIAGONAL_CALL_SMPLING){
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      y=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-(y+z)*spreadRatio[1]
    }else if(sampleSpreadType==DOUBLE_DIAGONAL_PUT_SMPLING){
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      y=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-(y+z)*spreadRatio[1]
    }else if(sampleSpreadType==DOUBLE_DIAGONAL_OPTYPE_ANY_SMPLING){
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      y=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-(y+z)*spreadRatio[1]
    }else if(sampleSpreadType==DIAGONAL_SMPLING){
      y=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]
    }else if(sampleSpreadType==POOL_PLUS_SINGLE_DIAGONAL_SMPLING){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
      
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==POOL_PLUS_DOUBLE_DIAGONAL_SMPLING){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
      
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      w=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_SINGLE_DIAGONAL){
      s1_idx=0
      s2_idx=0
      
      s1_idx=ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]])))
      s1<-pools[[ 1 ]][[2]][s1_idx, ]
      y<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
      
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_VERTICAL_CREDIT_SPREAD){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
      
      if(runif(1)<=0.500000){
        z=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                               verticalType=BULL_VERTICAL_SPREAD_TYPE,
                               targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      }else {
        z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                               verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                               targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      }
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_VERTICAL_DEBT_SPREAD){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
      
      if(runif(1)<=0.500000){
        z=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                               verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                               targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      }else {
        z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                               verticalType=BULL_VERTICAL_SPREAD_TYPE,
                               targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      }
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_IRON_CONDOR){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
      
      z=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      w=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_FILE){
      s1_idx=0
      s2_idx=0
      
      s1_idx=ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]])))
      s1<-pools[[ 1 ]][[2]][s1_idx, ]
      y<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
      
      s2_idx=ceiling(runif(1, min=1e-320, max=nrow(pools[[  2 ]][[2]])))
      s2<-pools[[ 2 ]][[2]][s2_idx, ]
      z<-unlist(s2[1:length(iniPos)]);s2_score<-as.numeric(s2[length(s2)])
      
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_DOUBLE_DIAGONAL){
      s1_idx=0
      s2_idx=0
      
      s1_idx=ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]])))
      s1<-pools[[ 1 ]][[2]][s1_idx, ]
      y<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
      
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      w=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
    }else if(sampleSpreadType==IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      
      z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      
      w=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      x<-(y+z)*spreadRatio[1]+w*spreadRatio[2]
    }else if(sampleSpreadType==CALL_BEAR_SPREAD_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]
      
    }else if(sampleSpreadType==PUT_BULL_SPREAD_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]
    }else if(sampleSpreadType==IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      
      z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      w=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      v=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-(y+z)*spreadRatio[1]+w*spreadRatio[2]+v*spreadRatio[3]
    }else if(sampleSpreadType==CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING){
      
      y=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==PUT_BULL_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      w=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
    }else if(sampleSpreadType==PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      w=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
    }
    
    posnum=sum(as.numeric((x)!=0))
    if(posnum==0)
      next
    
    if(isDetail)
      print(hollowNonZeroPosition(x))
    
    #cache check and evaluate
    val<-(InitialPopThresh-1)
    md5sumOfPos=digest(paste(x,collapse = ""))
    if(has.key(md5sumOfPos, POSITION_HASH)==FALSE){
      tryCatch(
        val<-obj_Income_sgmd(x,EvalFuncSetting,isDebug=isDebug,isDetail=isDetail,
                             udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                             maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                             tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                             Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                             Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum]),
        error=function(e){
          message(e)
          val<-(InitialPopThresh+1.0)
        })
      POSITION_HASH[md5sumOfPos]<-val
    }else{
      val<-POSITION_HASH[[md5sumOfPos]]
      hash_hit_num<-hash_hit_num+1
    }
    
    #value check
    tryCatch(
      if(val<(InitialPopThresh-1)){
        added_num<-added_num+1
        if(isFileout){
          cat(x,file=outFname,sep=",",append=TRUE);cat(",",file=outFname,append=TRUE)
          cat(val,file=outFname,append=TRUE)
          if(s1_idx!=0)
            cat(",",s1_idx,file=outFname,append=TRUE)
          if(s2_idx!=0){
            cat(",",s2_idx,file=outFname,append=TRUE)
          }
          cat("\n",file=outFname,append=TRUE)
        }
      },
      error=function(e){
        message(e)
        cat("val:",val,"InitialPopThresh",InitialPopThresh,"\n")
      }
    )
    #add total_count and show count information
    total_count<-total_count+1
    if((added_num%%50)==0){
      cat(" added num:",added_num," hash hit:",hash_hit_num," hash length:",length(POSITION_HASH),
          "total:",total_count," time:",(proc.time()-start_t)[3],"\n")
      start_t<-proc.time()
    }
    if(added_num==totalPopNum)
      break
  }
  cat("   hash hit:",hash_hit_num," hash length:",length(POSITION_HASH)," total:",total_count,"\n")
  POSITION_HASH<-hash()
}


##
# Creating initial candidate population of spread positions whose componets of each position are spcicified by the arguments.
# If the number (putn or calln) is even number, half of the spread`s positions are assigned +1(long), the other half -1(short).
# otherwise (odd number), the first 3 positions are assigned as -1 +2 -1 to form a butterfly, the rest position are assigned in the same way
# as the case of even number (half +1 long, the other half -1 short).
# When the each position of returned compound spread is 1 or -1, the spread position are multiplied by ml. 

create_initial_exact_PutCall_polulation<-function(popnum,type,EvalFuncSetting,thresh,putn,calln,ml,fname,PosMultip,
                                                  isFileout=FALSE,isDebug=FALSE,isDetail=FALSE){
  added_num<-0
  total_count<-0
  cat("hash hit:",HASH_HIT_NUM,"hash length",length(POSITION_OPTIM_HASH),"\n")
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
    #set position num
    posnum<-putn +calln
    
    #cache check and evaluate
    val<-(thresh+1.0)
    md5sumOfPos=digest(paste(x,collapse = ""))
    if(has.key(md5sumOfPos, POSITION_OPTIM_HASH)==FALSE){
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
          cat("val:",val,"thresh",thresh,"\n")
        })
      POSITION_OPTIM_HASH[md5sumOfPos]<<-val
    }else{
      val<-POSITION_OPTIM_HASH[[md5sumOfPos]]
      HASH_HIT_NUM<<-HASH_HIT_NUM+1
    }
    
    #value check
    tryCatch(
      if(val<thresh){
        added_num<-added_num+1
        if(isFileout){
          cat(x,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
          cat(val,file=fname,"\n",append=TRUE)
        }
      },
      error=function(e){
        message(e)
        cat("val:",val,"thresh:",thresh,"x",x,"\n")
      })
    #add total_count and show count information
    total_count<-total_count+1
    if((added_num%%50)==0){
      cat(" added num:",added_num,"total count:",total_count,"hash hit:",HASH_HIT_NUM,"hash num:",length(POSITION_OPTIM_HASH),"putn:",putn,"calln:",calln,"time:",(proc.time()-start_t)[3],"\n")
      start_t<-proc.time()
    }
    if(added_num>=popnum)
      break
  }
}

#called to calculate the spread's total legs num
getPutCallnOfthePosition<-function(x){
  type<-opchain$TYPE
  #put
  putpos<-(type+OpType_Put_G)
  putpos<-putpos/(OpType_Put_G*2)
  putn<-sum(abs(putpos*x))
  #call
  callpos<-(type+OpType_Call_G)
  callpos<-callpos/(OpType_Call_G*2)
  calln<-sum(abs(callpos*x))
  return (c(putn,calln))
}

#function for seraching candidate by combination 
# two sample examples. one from pools[[2]], the other from pools[[3]]
#ceiling(runif(1, min=1e-320, max=nrow(pools[[2]][[2]])))
create_combined_population<-function(popnum,EvalFuncSetting,thresh,plelem,ml,fname,isFileout=FALSE,isDebug=FALSE,maxposn,PosMultip){
  added_num<-0
  total_count<-0
  cat("hash hit:",HASH_HIT_NUM,"hash length",length(POSITION_OPTIM_HASH),"\n")
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
    x_new<-as.numeric((max(abs(x_new))==1))*x_new*ml+as.numeric((max(abs(x_new))!=1))*x_new
    if(isDebug){ cat(" x_new :",x_new) }
    total_count<-total_count+1
    
    #posnum
    #posnum<-sum(as.numeric((x_new-iniPos)!=0))
    posnum=sum(getPutCallnOfthePosition(x_new))
    if(posnum>maxposn){
      if(isDebug){ cat("posnum ",posnum,"over maxposn\n") }
      next
    }
    
    #cache check and evaluate
    val=(thresh+1.0) #initial not acceptable value
    md5sumOfPos=digest(paste(x_new,collapse = ""))
    if(has.key(md5sumOfPos, POSITION_OPTIM_HASH)==FALSE){
      tryCatch(
        val<-obj_Income_sgmd(x_new,EvalFuncSetting,isDebug=isDebug,isDetail=isDebug,
                             udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                             maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                             tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                             Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                             Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum]),
        error=function(e){
          message(e)
          cat("val:",val,"thresh",thresh,"\n")
        })
      POSITION_OPTIM_HASH[md5sumOfPos]<<-val
    }else{
      val<-POSITION_OPTIM_HASH[[md5sumOfPos]]
      HASH_HIT_NUM<<-HASH_HIT_NUM+1
    }
    #value check
    tryCatch(
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
      },
      error=function(e){
        message(e)
        cat("val:",val,"thresh:",thresh,"x",x_new,"\n")
      }
    )
    #show count information
    if(((added_num%%50)==0)){
      cat(" added num:",added_num,"hash hit:",HASH_HIT_NUM,"hash num:",length(POSITION_OPTIM_HASH),"total_count",total_count," time:",(proc.time()-start_t)[3],"\n")
      start_t<-proc.time()
    }
    if(added_num>=popnum)
      break
  }
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


##
# Functions to be loaded from EPosAanalysis.R

#create Aggregated Price Table for Drawing
createAgrregatedGreekTbl<-function(posStepDays,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,multi=PosMultip,iniCredit=iniCredit){
  
  #Delta
  posStepDays %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% dplyr::do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Delta)) -> tmp
  greek_tbl<-dplyr::full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-dplyr::full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,Delta=greek) -> greek_tbl
  agr_tbl <- greek_tbl
  
  #Gamma
  posStepDays %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% dplyr::do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Gamma)) -> tmp
  greek_tbl<-dplyr::full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-dplyr::full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,Gamma=greek) -> greek_tbl
  agr_tbl  %>% dplyr::left_join(greek_tbl)  -> agr_tbl
  
  #Vega
  posStepDays %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% dplyr::do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Vega)) -> tmp
  greek_tbl<-dplyr::full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-dplyr::full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,Vega=greek) -> greek_tbl
  agr_tbl  %>% dplyr::left_join(greek_tbl)  -> agr_tbl
  
  #Theta
  posStepDays %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% dplyr::do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Theta)) -> tmp
  greek_tbl<-dplyr::full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-dplyr::full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,Theta=greek) -> greek_tbl
  agr_tbl  %>% dplyr::left_join(greek_tbl)  -> agr_tbl
  
  #ThetaEffect
  posStepDays %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% dplyr::do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$ThetaEffect)) -> tmp
  greek_tbl<-dplyr::full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-dplyr::full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,ThetaEffect=greek) -> greek_tbl
  agr_tbl  %>% dplyr::left_join(greek_tbl)  -> agr_tbl
  
  #GammaEffect
  posStepDays %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% dplyr::do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$GammaEffect)) -> tmp
  greek_tbl<-dplyr::full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-dplyr::full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,GammaEffect=greek) -> greek_tbl
  agr_tbl  %>% dplyr::left_join(greek_tbl)  -> agr_tbl
  
  #DeltaEffect
  posStepDays %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% dplyr::do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$DeltaEffect)) -> tmp
  greek_tbl<-dplyr::full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-dplyr::full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,DeltaEffect=greek) -> greek_tbl
  agr_tbl  %>% dplyr::left_join(greek_tbl)  -> agr_tbl
  
  #VegaEffect
  posStepDays %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% dplyr::do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$VegaEffect)) -> tmp
  greek_tbl<-dplyr::full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-dplyr::full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,VegaEffect=greek) -> greek_tbl
  agr_tbl  %>% dplyr::left_join(greek_tbl)  -> agr_tbl
  
  #profit
  posStepDays %>% dplyr::group_by(days) %>% dplyr::rowwise() %>% dplyr::do(ptbl=createPriceTbl(.$days,.$scene,iniCredit)) -> tmp
  greek_tbl<-dplyr::full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-dplyr::full_join(greek_tbl,tmp$ptbl[[i]])
  }
  agr_tbl  %>% dplyr::left_join(greek_tbl)  -> agr_tbl
  
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
# Now this function change only Base Volatility Level.
# so arguments other than base_vol_chg does not affect anything.
adjustPosChgInner<-function(process_df,base_vol_chg=0){
  pos<-as.data.frame(process_df$pos[1])
  #print(pos)
  
  #base volatility change relrected to IVIDX
  ividx_chg_pct<-as.numeric(base_vol_chg)
  pos$IVIDX<-pos$IVIDX*(1+ividx_chg_pct)
  
  # ATM IV change
  #same as this
  # cat("########## ividx.f",(1+ividx_chg_pct),"holdDay",EvalFuncSetting$holdDays,"\n")
  # cat("TimeToExpDate",pos$TimeToExpDate,"\n")
  # cat("TYPE",pos$TYPE,"\n")
  # cat("regressed",get.ATMIV.f_1D.Regression.Result(pos,up_dn=ividx_chg_pct,days=1,hdd=EvalFuncSetting$holdDays,ividx.f=(1+ividx_chg_pct)),"\n")
  # cat("ATMIV pre",pos$ATMIV,"\n")
  pos$ATMIV<-pos$ATMIV*
    get.ATMIV.f_1D.Regression.Result(pos,up_dn=ividx_chg_pct,days=1,hdd=EvalFuncSetting$holdDays,ividx.f=(1+ividx_chg_pct))
  #cat("ATMIV pos",pos$ATMIV,"\n")
  
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

# Now this function change only Base Volatility Level.
# so arguments other than base_vol_chg does not affect anything.
adjustPosChg<-function(process_df,base_vol_chg=0,multi,hdd,HV_IV_Adjust_Ratio,isDebug=FALSE){
  
  if(isDebug){
    print(process_df)
  }
  
  process_df %>% dplyr::group_by(udlChgPct) %>% dplyr::do(pos=adjustPosChgInner(.,base_vol_chg=base_vol_chg)) -> process_df
  
  #cat("HV_IV_Adjust_Ratio (adjustPosChg):",HV_IV_Adjust_Ratio)
  
  ##
  #  Greeks
  #
  #  UDLY
  process_df %>% dplyr::rowwise() %>% dplyr::do(UDLY=mean(.$pos$UDLY)) ->tmp
  unlist(tmp$UDLY)->tmp ; process_df$UDLY <- tmp ;rm(tmp)
  #  Price
  process_df %>% dplyr::rowwise() %>% dplyr::do(Price=getPosGreeks(pos=.$pos$Position,greek=.$pos$Price,multi=multi)) ->tmp
  unlist(tmp$Price)->tmp ; process_df$Price <- tmp ;rm(tmp)
  #  Delta
  process_df %>% dplyr::rowwise() %>% dplyr::do(Delta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Delta,multi=multi))->tmp
  unlist(tmp$Delta)->tmp ; process_df$Delta <- tmp ;rm(tmp)
  #  Gamma
  process_df %>% dplyr::rowwise() %>% dplyr::do(Gamma=getPosGreeks(pos=.$pos$Position,greek=.$pos$Gamma,multi=multi))->tmp
  unlist(tmp$Gamma)->tmp ; process_df$Gamma <- tmp ;rm(tmp)
  #  Vega
  process_df %>% dplyr::rowwise() %>% dplyr::do(Vega=getPosGreeks(pos=.$pos$Position,greek=.$pos$Vega,multi=multi))->tmp
  unlist(tmp$Vega)->tmp ; process_df$Vega <- tmp ;rm(tmp)
  #  Theta
  process_df %>% dplyr::rowwise() %>% dplyr::do(Theta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Theta,multi=multi)) ->tmp
  unlist(tmp$Theta)->tmp ; process_df$Theta <- tmp ;rm(tmp)
  #  IVIDX
  process_df %>% dplyr::rowwise() %>% dplyr::do(IVIDX=mean(.$pos$IVIDX)) ->tmp
  unlist(tmp$IVIDX)->tmp ; process_df$IVIDX <- tmp ;rm(tmp)
  
  ##
  # Greek Effects
  
  # ThetaEffect
  process_df %>% dplyr::rowwise() %>% dplyr::do(ThetaEffect=getThetaEffect(pos=.$pos$Position,greek=.$pos$Theta,multi=multi,hdd=hdd)) -> tmp
  unlist(tmp$ThetaEffect)->tmp ; process_df$ThetaEffect <- tmp ;rm(tmp)
  # DeltaEffect
  process_df %>% dplyr::rowwise() %>% dplyr::do(DeltaEffect=getDeltaEffect(pos=.$pos$Position,greek=.$pos$Delta,
                                                                           UDLY=.$pos$UDLY,multi=multi,hdd=hdd,
                                                                           rlzdvol_td=.$pos$IVIDX*HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$DeltaEffect)->tmp ; process_df$DeltaEffect <- tmp ;rm(tmp)
  # GammaEffect
  process_df %>% dplyr::rowwise() %>% dplyr::do(GammaEffect=getGammaEffect(pos=.$pos$Position,greek=.$pos$Gamma,
                                                                           UDLY=.$pos$UDLY,multi=multi,hdd=hdd,
                                                                           rlzdvol_td=.$pos$IVIDX*HV_IV_Adjust_Ratio)) -> tmp
  unlist(tmp$GammaEffect)->tmp ; process_df$GammaEffect <- tmp ;rm(tmp)
  # VegaEffect
  process_df %>% dplyr::rowwise() %>% dplyr::do(VegaEffect=getVegaEffect(pos=.$pos$Position,greek=.$pos$Vega,
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

