library(RQuantLib)

###
# Start Stimulation
##
####

#Total Stimulation Num
StimultaionNum=1
#List of Every Result
StimRslts<-NULL

for(ith_stim in 1:StimultaionNum){
  #Stimulation processed on this data frame
  XTStim<-position
  XTOrig<-position
  #Stimulation day
  #returned as a vector. So get the first element.
  stim_days_num<-min(get.busdays.between(XTOrig$Date,XTOrig$ExpDate))-1
  
  #underlying daily return for geometic brown motion
  mu_udly<-(0.92)^(1/252)-1
  #underlying initial daily volatility.
  #HV should be used? or IV should be used?
  sigma_udly<-0.25/sqrt(252)
  
  #Prepare the stored values
  StimulationParameters <- NULL
  positionProfit <- rep(0, times=stim_days_num)
  positionEvalScores <- NULL
  PositionDataframe <- NULL
  
  #get the underlying changed of all days.
  s0<-XTStim$UDLY[[1]]
  udly_prices <- geombrmtn.stimulate(s0=s0,mu=mu_udly,sigma=sigma_udly,length=stim_days_num)
  udly_prices <- udly_prices[-1]
  
  StimulationParameters<-list(stim_days_num)
  StimulationParameters<-c(StimulationParameters,list(mu_udly))
  StimulationParameters<-c(StimulationParameters,list(sigma_udly))
  names(StimulationParameters)<-c("StimDays","Mu","Sigma")
  
  #First calculate original Position Grks
  orgPositionGrk<-getPositionGreeks(XTOrig,multi=PosMultip)
  
  for(day_chg in 1:stim_days_num){
    
    #Just for comparison later.
    XTStim_b<-XTStim
    
    #Advance day_chg day
    #XTStim$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(XTOrig$Date,format="%Y/%m/%d"),day_chg,0),"%Y/%m/%d")
    #print(XTStim$Date)
    #Underlying Price change
    XTStim$UDLY <- rep(udly_prices[day_chg],times=length(XTStim$UDLY))
    
    #Volatiliy Skew change 
    #case 1.
    tmp<-seq( ((XTStim$UDLY[1]-XTStim_b$UDLY[1])/XTStim_b$UDLY[1]),length=1)
    tmp2<-data.frame(udlChgPct=tmp)
    tmp2 %>% group_by(udlChgPct) %>% do(pos=XTStim) -> tmp2
    #days=1 means just 1 day will have passed. 
    tmp2 %>% group_by(udlChgPct) %>% do(pos=reflectPosChg(.,days=1)) -> tmp2
    XTStim<-tmp2$pos[[1]]
    rm(tmp,tmp2)
    print(XTStim)
    
    #print(thePositionGrk)
    
    #case 2. volatility r.v i.i.d
    # To be defined.
   
    #Set new Price and Greeks
    #Case of European Option
#     tmp_<-set.EuropeanOptionValueGreeks(XTStim)
#     XTStim$Price<-tmp_$Price
#     XTStim$Delta<-tmp_$Delta
#     XTStim$Gamma<-tmp_$Gamma
#     XTStim$Vega<-tmp_$Vega
#     XTStim$Theta<-tmp_$Theta
#     XTStim$Rho<-tmp_$Rho
#     (tmp_)

    # greekEffects are calculated supposing this position held for "holdDays"
    # 
    newPositionGrk<-getPositionGreeks(XTStim,multi=PosMultip)
    print(newPositionGrk)

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
      positionEvalScores<-list(day_chg)
    }else{
      positionEvalScores<-c(PositionDataframe,list(day_chg))
    }
    
    #Current XTStim appened to a list
    #Data frame to be stored. Greeks and Everything
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
  content_<-c(content_,list(day_chg))
  content_<-c(content_,list(positionProfit))
  content_<-c(content_,list(positionEvalScores))
  content_<-c(content_,list(PositionDataframe))
  names(content_)<-c("Parameter","AdjustDay","Profit","EvalScore","ValueGreek")
  if(ith_stim==1){
    StimRslts<-list(content_)
    names(StimulationParameters)<-c("Result")
  } else{
    StimRslts<-c(StimRslts,list(content_))
  }
}

##
# Result analysis ---------------------------
profit_each_itr_ <- rep(0,StimultaionNum)
udly_price_at_liquidation_  <- rep(0,StimultaionNum)
liq_days_<- rep(0,StimultaionNum)
for(j in 1:StimultaionNum){
  profit_each_itr_[j]<-StimRslts[[j]]$Profit[StimRslts[[j]]$AdjustDay]
  udly_price_at_liquidation_[j]<-mean(StimRslts[[j]]$ValueGreek[[StimRslts[[j]]$AdjustDay]]$UDLY)
  liq_days_[j]<-StimRslts[[j]]$AdjustDay
}
# max profit and corresponding Underlying Price
max(profit_each_itr_);udly_price_at_liquidation_[order(profit_each_itr_)[length(profit_each_itr_)]]
# min profit and corresponding Underlying Price
min(profit_each_itr_);udly_price_at_liquidation_[order(profit_each_itr_)[1]]
# mean profit
mean(profit_each_itr_)
# standard deviation
sd(profit_each_itr_)
# median profit
median(profit_each_itr_)

hist(udly_price_at_liquidation_,breaks="Scott",main="UDLY Liq Prices")
points(mean(XT[[1]]$UDLY),0, col = "red", pch = 3)

hist(profit_each_itr_,breaks="Scott",main="Profit/Loss")
points(mean(profit_each_itr_), 0, col = "red", pch = 3)
points(median(profit_each_itr_), 0, col = "orange", pch = 1)
points(mean(profit_each_itr_)+sd(profit_each_itr_), 0, col = "darkgrey", pch = 2)
points(mean(profit_each_itr_)-sd(profit_each_itr_), 0, col = "darkgrey", pch = 2)


##
##New Data Flame made to faciliate futher analying
##
res_dtf_ <- data.frame(UDLY=udly_price_at_liquidation_,
                       Profit=profit_each_itr_,
                       LIQDAY=liq_days_)

#Condition
subset(res_dtf_,Profit<=-15)

#Condition 
#Underlying > Inital Underlying
max((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit)
min((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit)
mean((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit)
sd((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit)
median((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit)
#UDLY histgram
hist((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$UDLY,
     breaks="Scott",main="UDLY Liq Prices over Initial UDL Prc")
points(mean(XT[[1]]$UDLY),0, col = "red", pch = 3)
#Profit/Loss histgram
hist((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit,
     breaks="Scott",main="Profit/Loss over initial Underlying Prices")
points(mean((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit),
       0, col = "red", pch = 3)
points(median((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit), 
       0, col = "orange", pch = 1)
points(mean((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit)
       +sd((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit),
       0,col = "darkgrey", pch = 2)
points(mean((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit)
       -sd((subset(res_dtf_,UDLY>mean(XT[[1]]$UDLY)))$Profit),
       0,col = "darkgrey", pch = 2)

###Scenario 2.
###
#volatility changes based on market condition.
#also should pertubate IV based on stiumulation condition

#Back Test Functions.
#To Be Designed Later.


#Functions to be loaded ---------

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

##set.AmericanOptionValueGreeks(xt)
# to be defined
