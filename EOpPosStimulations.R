#get business days between the days expressed as character
get.busdays.between <- function(start,end){
  bus_day<-businessDaysBetween(CALENDAR_G,
                               as.Date(start,format="%Y/%m/%d"),
                               as.Date(end,format="%Y/%m/%d"))
  bus_day
}

##
# insert a row at r line to the existing Data Frame.
# return new inserted Data Frame

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

##
# revised version of S't'imulate

Simulate <- function (position,
                      #Total Stimulation Num
                      StimultaionNum=1000,
                      #Max Stimulation day.
                      MaxStimDay,
                      #Option Multiple
                      PosMultip,
                      #holding days to compute Greek Effects
                      hdd,
                      #underlying daily return for geometic brown motion
                      mu_udly,
                      #underlying initial daily volatility.
                      sigma_udly,
                      #volatility drift mu of geometric brown motion
                      mu_iv,
                      #vov of geometric brown motion
                      sigma_iv,
                      #Assumed Realized Volatility and IV ratio
                      HV_IV_Adjust_Ratio,
                      #randomness introduced to calculated(estimated) IV
                      IV_DEVIATION) {
  
  #List of Every Result
  start_t<-proc.time()  
  StimRslts<-vector("list",StimultaionNum)
  for(ith_stim in 1:StimultaionNum){
    #Stimulation processed on this data frame
    XTStim<-position
    XTOrig<-position
    #Stimulation day
    #returned as a vector. So get the first element.
    stim_days_num<-min(get.busdays.between(XTOrig$Date,XTOrig$ExpDate))-1
    #if MaxStimDay<stim_days_num, stim_days_num<-MaxStimDay.
    stim_days_num<-as.numeric(MaxStimDay<stim_days_num)*(MaxStimDay-stim_days_num)+stim_days_num
    
    #get the underlying changed of all days.
    udly_prices <- GBM(S0=XTStim$UDLY[[1]],u=mu_udly,sigma=sigma_udly,N=stim_days_num)
                  #geombrmtn.stimulate(s0=XTStim$UDLY[[1]],mu=mu_udly,sigma=sigma_udly,length=stim_days_num)
                  #udly_prices <- udly_prices[-1]
    #cat(udly_prices,sep=",","\n")
    
    #cat(udly_prices,sep=",","\n")
    
    #get and create the IV change percent to base IV
    iv_chgPercnt <- GBM(S0=1.0,u=mu_iv,sigma=sigma_iv,N=stim_days_num)
                  #geombrmtn.stimulate(s0=1.0,mu=mu_iv,sigma=sigma_iv,length=stim_days_num)
                  #iv_chgPercnt <- iv_chgPercnt[-1]
    tmp<-replace(iv_chgPercnt,rep(2:length(iv_chgPercnt)),iv_chgPercnt[1:length(iv_chgPercnt)-1])
    tmp[1]<-1
    iv_chgPercnt<-iv_chgPercnt/tmp
    
    SimparamNames<-c("StimDays","Mu_udl","Sigma_udl","Mu_iv","Sigma_iv")
    StimulationParameters <- vector("list",length(SimparamNames))
    StimulationParameters[[1]]<-stim_days_num
    StimulationParameters[[2]]<-mu_udly
    StimulationParameters[[3]]<-sigma_udly
    StimulationParameters[[4]]<-mu_iv
    StimulationParameters[[5]]<-sigma_iv
    names(StimulationParameters)<-SimparamNames
    
    #First calculate original Position Grks
    orgPositionGrk<-getPositionGreeks(XTOrig,multi=PosMultip,hdd=hdd,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)
    #print(orgPositionGrk)
    
    #Push original histIV, later Poped
    histIVOrig<-histIV
    
    #Prepare the stored values
    positionProfit <- rep(0, times=stim_days_num)
    positionEvalScores<-vector("list",stim_days_num)
    PositionDataframe<-vector("list",stim_days_num)
    
    for(day_chg in 1:stim_days_num){
      
      #Just for comparison later.
      XTStim_b<-XTStim
      
      #print(day_chg)
      #cat("before IVIDX",XTStim$IVIDX,"\n")
      #cat("iv_chgPercnt ",iv_chgPercnt[day_chg],"\n")
      XTStim$IVIDX<-XTStim$IVIDX*iv_chgPercnt[day_chg]
      #cat("after IVIDX",XTStim$IVIDX,"\n")
      
      ## Create this kind of data frame.
      #       udlChgPct    pos
      #   1     0.03   <S3:data.frame>
      tmp<-seq( ((udly_prices[day_chg]-XTStim_b$UDLY[1])/XTStim_b$UDLY[1]),length=1)
      tmp2<-data.frame(udlChgPct=tmp)
      tmp2 %>% group_by(udlChgPct) %>% do(pos=XTStim) -> tmp2
      
      ## Reflect Position Change
      #
      #days=1 means just 1 day will have passed. 
      tmp2 %>% group_by(udlChgPct) %>% do(pos=reflectPosChg(.,days=1,IV_DEVIATION=IV_DEVIATION)) -> tmp2
      XTStim<-tmp2$pos[[1]]
      
      #XTStim is the above_data_frame$pos which reflects the Date,UDLY,IVIDX,OrigIV,All Greeks
      #cat("UDLY should become ",udly_prices[day_chg],"\n")
      #cat("Actual UDLY ",XTStim$UDLY,"\n")
      #print(XTStim)
      
      #Latest (Date,IVIDX) inserted on top of histIV 
      XTStim  %>% select(Date,IVIDX) %>% .[1,] -> tmp
      histIV<-insertRow(histIV,tmp,r=1)
      #Oldest row should be removed
      histIV %>% slice(-nrow(histIV))->histIV
      
      #Caluculate Greeks 
      newPositionGrk<-getPositionGreeks(XTStim,multi=PosMultip,hdd=hdd,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)
      #print(newPositionGrk)
      
      #Position Profit
      positionProfit[day_chg]<-newPositionGrk$Price-orgPositionGrk$Price
      
      #EvalScore
      positionEvalScores[[day_chg]] <- newPositionGrk
      
      #ValueGreek
      PositionDataframe[[day_chg]] <- XTStim
      
    }
    
    positionProfit <- positionProfit - positionProfit[1]
    
    histIV<-histIVOrig
    #the ith_stim iteration finished
    content_names<-c("Parameter","IniValueGreek","IniEvalScore","AdjustDay","Profit","EvalScore","ValueGreek")
    content_<-vector("list",length(content_names))
    content_[[1]]<-StimulationParameters
    content_[[2]]<-XTOrig
    content_[[3]]<-orgPositionGrk
    content_[[4]]<-day_chg
    content_[[5]]<-positionProfit
    content_[[6]]<-positionEvalScores
    content_[[7]]<-PositionDataframe
    names(content_)<-content_names
    #Result List
    StimRslts[[ith_stim]]<-content_
  }
  cat(" sim_result ",ith_stim, " time: ",(proc.time()-start_t)[3])
  return(StimRslts)
}

##
# creating Stimulation results data frame from StimRslts list object

getStimResultDataFrame <- function (StimRslts,StimultaionNum){
  profit_each_itr_ <- rep(0,StimultaionNum)
  udly_price_at_liquidation_  <- rep(0,StimultaionNum)
  DeltaEffect_at_liliquidation <- rep(0,StimultaionNum)
  VegaEffect_at_liliquidation <- rep(0,StimultaionNum)
  ThetaEffect_at_liliquidation <- rep(0,StimultaionNum)
  GammaEffect_at_liliquidation <- rep(0,StimultaionNum)
  Delta_at_liliquidation <- rep(0,StimultaionNum)
  Vega_at_liliquidation <- rep(0,StimultaionNum)
  liq_days_<- rep(0,StimultaionNum)
  for(j in 1:StimultaionNum){
    profit_each_itr_[j]<-StimRslts[[j]]$Profit[StimRslts[[j]]$AdjustDay]
    udly_price_at_liquidation_[j]<-mean(StimRslts[[j]]$ValueGreek[[StimRslts[[j]]$AdjustDay]]$UDLY)
    DeltaEffect_at_liliquidation[j]<-StimRslts[[j]]$EvalScore[[StimRslts[[j]]$AdjustDay]]$DeltaEffect
    VegaEffect_at_liliquidation[j]<-StimRslts[[j]]$EvalScore[[StimRslts[[j]]$AdjustDay]]$VegaEffect
    ThetaEffect_at_liliquidation[j]<-StimRslts[[j]]$EvalScore[[StimRslts[[j]]$AdjustDay]]$ThetaEffect
    GammaEffect_at_liliquidation[j]<-StimRslts[[j]]$EvalScore[[StimRslts[[j]]$AdjustDay]]$GammaEffect
    Delta_at_liliquidation[j]<-StimRslts[[j]]$EvalScore[[StimRslts[[j]]$AdjustDay]]$Delta
    Vega_at_liliquidation[j]<-StimRslts[[j]]$EvalScore[[StimRslts[[j]]$AdjustDay]]$Vega
    liq_days_[j]<-StimRslts[[j]]$AdjustDay
  }
  return(data.frame(udly=udly_price_at_liquidation_,profit=profit_each_itr_,
                    DeltaEffect=DeltaEffect_at_liliquidation,VegaEffect=VegaEffect_at_liliquidation,
                    ThetaEffect=ThetaEffect_at_liliquidation,GammaEffect=GammaEffect_at_liliquidation,
                    Delta=Delta_at_liliquidation,Vega=Vega_at_liliquidation,liqDay=liq_days_))
}

getGCDComboRatio <- function(combo_ratio){
  combo_ratio<-as.numeric((sum(combo_ratio%%7)!=0))*combo_ratio+as.numeric((sum(combo_ratio%%7)==0))*combo_ratio/7
  combo_ratio<-as.numeric((sum(combo_ratio%%5)!=0))*combo_ratio+as.numeric((sum(combo_ratio%%5)==0))*combo_ratio/5
  combo_ratio<-as.numeric((sum(combo_ratio%%4)!=0))*combo_ratio+as.numeric((sum(combo_ratio%%4)==0))*combo_ratio/4
  combo_ratio<-as.numeric((sum(combo_ratio%%3)!=0))*combo_ratio+as.numeric((sum(combo_ratio%%3)==0))*combo_ratio/3
  combo_ratio<-as.numeric((sum(combo_ratio%%2)!=0))*combo_ratio+as.numeric((sum(combo_ratio%%2)==0))*combo_ratio/2
  return(combo_ratio)
}

##
# write code snap of API to acess IB TWS

writeIbAPITicket <- function(out_text_file,thePosition,sep="$"){
  thePosition_org<-thePosition
  
  #if the legnum >=5, sprit thePisition into 2 parts. Put and Call
  if(length(thePosition$TYPE)>=5)
    thePosition %>% filter(TYPE==OpType_Put_G) -> thePosition
  m_symbol<-rep(Underying_Symbol_G,times=length(thePosition$TYPE))
  m_expiry<-format(as.Date(thePosition$ExpDate,format="%Y/%m/%d"),"%Y%m%d")
  m_strike<-thePosition$Strike
  m_right<-ifelse(thePosition$TYPE==OpType_Put_G, "P","C")
  buy_sell<-ifelse(thePosition$Position>=0, "BUY","SELL")
  combo_ratio_raw <- abs(thePosition$Position)
  combo_ratio <- getGCDComboRatio(combo_ratio_raw)
  limit_price<-(-9000)
  qty<-min(combo_ratio_raw/combo_ratio)
  
  cat("SymbolTicket = [ ",file=out_text_file,append=T);cat(sprintf("\'%s\'",paste(m_symbol)),sep=sep,file=out_text_file,append=T) ; cat(" ]; ",file=out_text_file,append=T)
  cat("ExpiryTicket = [ ",file=out_text_file,append=T);cat(sprintf("\'%s\'",paste(m_expiry)),sep=sep,file=out_text_file,append=T) ; cat(" ]; ",file=out_text_file,append=T)
  cat("StrikeTicket = [ ",file=out_text_file,append=T); cat(paste(m_strike),sep=sep,file=out_text_file,append=T) ; cat(" ] ; ",file=out_text_file,append=T)
  cat("RightTicket = [ ",file=out_text_file,append=T);cat(sprintf("\'%s\'",paste(m_right)),sep=sep,file=out_text_file,append=T) ; cat(" ]; ",file=out_text_file,append=T)
  cat("BuySell = [ ",file=out_text_file,append=T);cat(sprintf("\'%s\'",paste(buy_sell)),sep=sep,file=out_text_file,append=T) ; cat(" ]; ",file=out_text_file,append=T)
  cat("ComboRatio = [ ",file=out_text_file,append=T);cat(paste(combo_ratio),sep=sep,file=out_text_file,append=T);cat(" ] ;",file=out_text_file,append=T)
  cat("LimitPrice_G = ",limit_price," ;",file=out_text_file,append=T)
  cat("QTY_G = ",qty,"\n",file=out_text_file,append=T)
  
  #Second Call, if needed.
  thePosition<-thePosition_org
  if(length(thePosition$TYPE)>=5) {
    thePosition %>% filter(TYPE==OpType_Call_G) -> thePosition
    m_symbol<-rep(Underying_Symbol_G,times=length(thePosition$TYPE))
    m_expiry<-format(as.Date(thePosition$ExpDate,format="%Y/%m/%d"),"%Y%m%d")
    m_strike<-thePosition$Strike
    m_right<-ifelse(thePosition$TYPE==OpType_Put_G, "P","C")
    buy_sell<-ifelse(thePosition$Position>=0, "BUY","SELL")
    combo_ratio_raw <- abs(thePosition$Position)
    combo_ratio <- getGCDComboRatio(combo_ratio_raw)
    limit_price<-(-9000)
    qty<-min(combo_ratio_raw/combo_ratio)
    
    cat("SymbolTicket2 = [ ",file=out_text_file,append=T);cat(sprintf("\'%s\'",paste(m_symbol)),sep=sep,file=out_text_file,append=T) ; cat(" ]; ",file=out_text_file,append=T)
    cat("ExpiryTicket2 = [ ",file=out_text_file,append=T);cat(sprintf("\'%s\'",paste(m_expiry)),sep=sep,file=out_text_file,append=T) ; cat(" ]; ",file=out_text_file,append=T)
    cat("StrikeTicket2 = [ ",file=out_text_file,append=T); cat(paste(m_strike),sep=sep,file=out_text_file,append=T) ; cat(" ] ; ",file=out_text_file,append=T)
    cat("RightTicket2 = [ ",file=out_text_file,append=T);cat(sprintf("\'%s\'",paste(m_right)),sep=sep,file=out_text_file,append=T) ; cat(" ]; ",file=out_text_file,append=T)
    cat("BuySell2 = [ ",file=out_text_file,append=T);cat(sprintf("\'%s\'",paste(buy_sell)),sep=sep,file=out_text_file,append=T) ; cat(" ]; ",file=out_text_file,append=T)
    cat("ComboRatio2 = [ ",file=out_text_file,append=T);cat(paste(combo_ratio),sep=sep,file=out_text_file,append=T);cat(" ] ;",file=out_text_file,append=T)
    cat("LimitPrice2_G = ",limit_price," ;",file=out_text_file,append=T)
    cat("QTY2_G = ",qty,"\n",file=out_text_file,append=T)
  }
}


##set.AmericanOptionValueGreeks(xt)
# to be defined
