###
# Start Stimulation
##
####

##
# Functions to be loaded ---------

#get business days between the days expressed as character
get.busdays.between <- function(start,end){
  bus_day<-businessDaysBetween("UnitedStates/NYSE",
                               as.Date(start,format="%Y/%m/%d"),
                               as.Date(end,format="%Y/%m/%d"))
  bus_day
}

#set.ValueGreeks(xt) should be loaded before.
# Option Value and Greek Change
set.EuropeanOptionValueGreeks <- function(xt){
  tmp_<-(set.ValueGreeks(xt))
  #Price
  ret_<-list(tmp_[[1]])
  #Delta
  ret_<-c(ret_,list(tmp_[[2]]))
  #Gamma
  ret_<-c(ret_,list(tmp_[[3]]))
  #Vega
  ret_<-c(ret_,list(tmp_[[4]]/100))
  #Theta
  ret_<-c(ret_,list(tmp_[[5]]/365))
  #Rho
  ret_<-c(ret_,list(tmp_[[6]]/365))
  names(ret_)<-c("Price","Delta","Gamma","Vega","Theta","Rho")  
  ret_
}

##
# Stimulate main function. returns StimRslts

Stimulate <- function (position,
                       #Total Stimulation Num
                       StimultaionNum=1000,
                       #Max Stimulation day.
                       MaxStimDay=14,PosMultip=PosMultip,
                       #underlying daily return for geometic brown motion
                       mu_udly,
                       #underlying initial daily volatility.
                       sigma_udly,
                       #volatility drift mu of geometric brown motion
                       mu_iv,
                       #vov of geometric brown motion
                       sigma_iv) {
  
  #List of Every Result
  StimRslts<-NULL  
  for(ith_stim in 1:StimultaionNum){
    #Stimulation processed on this data frame
    XTStim<-position
    XTOrig<-position
    #Stimulation day
    #returned as a vector. So get the first element.
    stim_days_num<-min(get.busdays.between(XTOrig$Date,XTOrig$ExpDate))-1
    #if MaxStimDay<stim_days_num, stim_days_num<-MaxStimDay.
    stim_days_num<-as.numeric(MaxStimDay<stim_days_num)*(MaxStimDay-stim_days_num)+stim_days_num
    
    #Prepare the stored values
    StimulationParameters <- NULL
    positionProfit <- rep(0, times=stim_days_num)
    positionEvalScores <- NULL
    PositionDataframe <- NULL
    
    #get the underlying changed of all days.
    udly_prices <- geombrmtn.stimulate(s0=XTStim$UDLY[[1]],mu=mu_udly,sigma=sigma_udly,length=stim_days_num)
    udly_prices <- udly_prices[-1]
    
    #get and create the IV change percent to base IV
    iv_chgPercnt <- geombrmtn.stimulate(s0=1.0,mu=mu_iv,sigma=sigma_iv,length=stim_days_num)
    iv_chgPercnt <- iv_chgPercnt[-1]
    tmp<-replace(iv_chgPercnt,rep(2:length(iv_chgPercnt)),iv_chgPercnt[1:length(iv_chgPercnt)-1])
    tmp[1]<-1
    iv_chgPercnt<-iv_chgPercnt/tmp
    
    StimulationParameters<-list(stim_days_num)
    StimulationParameters<-c(StimulationParameters,list(mu_udly))
    StimulationParameters<-c(StimulationParameters,list(sigma_udly))
    StimulationParameters<-c(StimulationParameters,list(mu_iv))
    StimulationParameters<-c(StimulationParameters,list(sigma_iv))
    names(StimulationParameters)<-c("StimDays","Mu_udl","Sigma_udl","Mu_iv","Sigma_iv")
    
    #First calculate original Position Grks
    orgPositionGrk<-getPositionGreeks(XTOrig,multi=PosMultip)
    #print(orgPositionGrk)
    for(day_chg in 1:stim_days_num){
      
      #Just for comparison later.
      XTStim_b<-XTStim
      
      #Advance day_chg day
      #XTStim$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(XTOrig$Date,format="%Y/%m/%d"),day_chg,0),"%Y/%m/%d")
      #print(XTStim$Date)
      #Underlying Price change
      XTStim$UDLY <- rep(udly_prices[day_chg],times=length(XTStim$UDLY))
      
      #Base IVIDX Swings
      XTStim$IVIDX<-XTStim$IVIDX*iv_chgPercnt[day_chg]
      
      #Volatiliy Skew change 
      #case 1.
      tmp<-seq( ((XTStim$UDLY[1]-XTStim_b$UDLY[1])/XTStim_b$UDLY[1]),length=1)
      tmp2<-data.frame(udlChgPct=tmp)
      tmp2 %>% group_by(udlChgPct) %>% do(pos=XTStim) -> tmp2
      #days=1 means just 1 day will have passed. 
      tmp2 %>% group_by(udlChgPct) %>% do(pos=reflectPosChg(.,days=1)) -> tmp2
      XTStim<-tmp2$pos[[1]]
      rm(tmp,tmp2)
      #print(XTStim)
      
      # greekEffects are calculated supposing this position held for "holdDays"
      # 
      newPositionGrk<-getPositionGreeks(XTStim,multi=PosMultip)
      #print(newPositionGrk)
      
      #Case of American Option
      #TBD
      
      #Position Profit
      #positionProfit[day_chg]<-(sum(XTStim$Position*XTStim$Price) - sum(XTOrig$Position*XTOrig$Price))*posMultip
      # or
      positionProfit[day_chg]<-newPositionGrk$Price-orgPositionGrk$Price
      
      #Evaluation Function to be used for position adjustment(liqudation)
      #Suchs as DTRRR, VTRRR, RTRRR. TBD
      #positionProfit[day_chg] may be used.
      
      if(day_chg==1){
        positionEvalScores<-list(newPositionGrk)
      }else{
        positionEvalScores<-c(positionEvalScores,list(newPositionGrk))
      }
      
      #Current XTStim appended to a list
      #Data frame to be stored.
      if(day_chg==1){
        PositionDataframe<-list(XTStim)
      }else{
        PositionDataframe<-c(PositionDataframe,list(XTStim))
      }
      
      #Position adjustment(liqudation) to be defined later
      # for example. simple los-cut
      #if (positionProfit[day_chg]<=-5) { 
      #  break
      #}
      
      #otherwise advance a day. loop.
    }
    
    #the ith_stim iteration finished
    content_<-list(StimulationParameters)
    content_<-c(content_,list(XTOrig))
    content_<-c(content_,list(orgPositionGrk))
    content_<-c(content_,list(day_chg))
    content_<-c(content_,list(positionProfit))
    content_<-c(content_,list(positionEvalScores))
    content_<-c(content_,list(PositionDataframe))
    names(content_)<-c("Parameter","IniValueGreek","IniEvalScore","AdjustDay","Profit","EvalScore","ValueGreek")
    if(ith_stim==1){
      StimRslts<-list(content_)
      names(StimulationParameters)<-c("Result")
    } else{
      StimRslts<-c(StimRslts,list(content_))
    }
 # rm(day_chg,positionProfit,positionEvalScores,PositionDataframe,content_)
 # rm(XTOrig,XTStim,XTStim_b,newPositionGrk,orgPositionGrk,udly_prices,iv_chgPercnt,StimulationParameters)
  }
  return(StimRslts)
}

##
# creating Stimulation results data frame from StimRslts list object

getStimResultDataFrame <- function (StimRslts,StimultaionNum){
  profit_each_itr_ <- rep(0,StimultaionNum)
  udly_price_at_liquidation_  <- rep(0,StimultaionNum)
  liq_days_<- rep(0,StimultaionNum)
  for(j in 1:StimultaionNum){
    profit_each_itr_[j]<-StimRslts[[j]]$Profit[StimRslts[[j]]$AdjustDay]
    udly_price_at_liquidation_[j]<-mean(StimRslts[[j]]$ValueGreek[[StimRslts[[j]]$AdjustDay]]$UDLY)
    liq_days_[j]<-StimRslts[[j]]$AdjustDay
  }
  return(data.frame(udly=udly_price_at_liquidation_,profit=profit_each_itr_,liqDay=liq_days_))
}

##set.AmericanOptionValueGreeks(xt)
# to be defined
