###
# Start Stimulation
##
####

#Total Stimulation Num
StimultaionNum=100
#List of Every Result
StimRslts<-NULL
#Max Stimulation day.
MaxStimDay<-14

for(ith_stim in 1:StimultaionNum){
  #Stimulation processed on this data frame
  XTStim<-position
  XTOrig<-position
  #Stimulation day
  #returned as a vector. So get the first element.
  stim_days_num<-min(get.busdays.between(XTOrig$Date,XTOrig$ExpDate))-1
  #if MaxStimDay<stim_days_num, stim_days_num<-MaxStimDay.
  stim_days_num<-as.numeric(MaxStimDay<stim_days_num)*(MaxStimDay-stim_days_num)+stim_days_num
  
  #underlying daily return for geometic brown motion
  mu_udly<-(0.92)^(1/252)-1
  #underlying initial daily volatility.
  sigma_udly<-0.25/sqrt(252)
  #sigma_udly<-getIV_td(histIV[1,]$IVIDX)/sqrt(252)
  
  #volatility drift 
  #mu_iv<-(1.0)^(1/252)-1
  #vov
  #sigma_iv<-0.8/sqrt(252)
  
  #Prepare the stored values
  StimulationParameters <- NULL
  positionProfit <- rep(0, times=stim_days_num)
  positionEvalScores <- NULL
  PositionDataframe <- NULL
  
  #get the underlying changed of all days.
  
  udly_prices <- geombrmtn.stimulate(s0=XTStim$UDLY[[1]],mu=mu_udly,sigma=sigma_udly,length=stim_days_num)
  udly_prices <- udly_prices[-1]
  
  StimulationParameters<-list(stim_days_num)
  StimulationParameters<-c(StimulationParameters,list(mu_udly))
  StimulationParameters<-c(StimulationParameters,list(sigma_udly))
  names(StimulationParameters)<-c("StimDays","Mu_udl","Sigma_udl")
  
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
    
    #case 2. volatility r.v i.i.d
    #         UDLYとIVの間に相関が無く、独立変数として扱う
    #XTStim$IVIDX<-... 設定後
    #                  1. XTStim$IVIDXで変更される値を先読みし、reflectPosChgで元に戻るように設定
    #                  2. reflectPosChg()の後、XTStim$IVIDX<-XTStim_b$IVIDX起点のランダムウォーク
    #                     シミュレーション開始時に計算済
    #Price and Greeks に関しては再計算（そのたのTime,Moneynessは再計算不要）
    #Case of European Option　再計算
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
  rm(day_chg,positionProfit,positionEvalScores,PositionDataframe,content_)
  rm(XTOrig,XTStim,XTStim_b,newPositionGrk,orgPositionGrk,udly_prices,StimulationParameters)
  rm(mu_udly,sigma_udly,stim_days_num)
}

#cleanings for the stimulation
rm(ith_stim)

##
# Result analysis ---------------------------
profit_each_itr_ <- rep(0,StimultaionNum)
udly_price_at_liquidation_  <- rep(0,StimultaionNum)
liq_days_<- rep(0,StimultaionNum)
for(j in 1:StimultaionNum){
  profit_each_itr_[j]<-StimRslts[[j]]$Profit[StimRslts[[j]]$AdjustDay]
  udly_price_at_liquidation_[j]<-mean(StimRslts[[j]]$ValueGreek[[StimRslts[[j]]$AdjustDay]]$UDLY)
  liq_days_[j]<-StimRslts[[j]]$AdjustDay
};rm(j)

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

#udly price histgram
gg <- ggplot(data.frame(udly=udly_price_at_liquidation_),aes(x=udly))+geom_histogram(alpha=0.9,aes(y=..density..))+geom_density(size=1.0,colour="blue")
(gg+geom_point(x=mean(position$UDLY),y=0,size=6.0,colour="red",pch=3)
 +geom_point(x=mean(udly_price_at_liquidation_),y=0,size=6.0,colour="red",pch=1)
 +geom_point(x=median(udly_price_at_liquidation_),y=0,size=6.0,colour="orange",pch=1)
 +geom_point(x=mean(udly_price_at_liquidation_)+sd(udly_price_at_liquidation_),y=0,size=6.0,colour="darkgrey",pch=2)
 +geom_point(x=mean(udly_price_at_liquidation_)-sd(udly_price_at_liquidation_),y=0,size=6.0,colour="darkgrey",pch=2)
 )
rm(gg)

#payoff function
gg <- ggplot(data.frame(udly=udly_price_at_liquidation_,profit=profit_each_itr_),aes(x=udly,y=profit))
(gg+geom_point()
 +geom_point(x=mean(position$UDLY),y=0,size=6.0,colour="red",pch=3)
 +geom_point(x=mean(udly_price_at_liquidation_),y=0,size=6.0,colour="red",pch=1)
 +geom_point(x=median(udly_price_at_liquidation_),y=0,size=6.0,colour="orange",pch=1)
 +geom_point(x=mean(udly_price_at_liquidation_)+sd(udly_price_at_liquidation_),y=0,size=6.0,colour="darkgrey",pch=2)
 +geom_point(x=mean(udly_price_at_liquidation_)-sd(udly_price_at_liquidation_),y=0,size=6.0,colour="darkgrey",pch=2)
 )
rm(gg)

#profit histgram
gg <- ggplot(data.frame(profit=profit_each_itr_),aes(x=profit))+geom_histogram(alpha=0.9,aes(y=..density..))+geom_density(size=1.0,colour="blue")
(gg+geom_point(x=mean(profit_each_itr_),y=0,size=5.0,colour="red",pch=3)
 +geom_point(x=median(profit_each_itr_),y=0,size=5.0,colour="orange",pch=1)
 +geom_point(x=mean(profit_each_itr_)+sd(profit_each_itr_),y=0,size=5.0,colour="darkgrey",pch=2)
 +geom_point(x=mean(profit_each_itr_)-sd(profit_each_itr_),y=0,size=5.0,colour="darkgrey",pch=2)
 +geom_density()
 ) 
rm(gg)

##
##New Data Flame made to faciliate futher analying
##
res_dtf_ <- data.frame(UDLY=udly_price_at_liquidation_,
                       Profit=profit_each_itr_,
                       LIQDAY=liq_days_)

#Condition 特定の条件で切り出したデータの（条件付き）分布
#subset(res_dtf_,Profit<=-15)

#Condition 
#Underlying > Inital Underlying
#max((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit)
#min((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit)
#mean((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit)
#sd((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit)
#median((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit)

#UDLY histgram
# Here UDLYが平均より大きい結果の条件の下でのUDLYの頻度分布
#hist((subset(res_dtf_,UDLY>mean(position$UDLY)))$UDLY,
#     breaks="Scott",main="UDLY Liq Prices over Initial UDL Prc")
#points(mean(position$UDLY),0, col = "red", pch = 3)

#Profit/Loss histgram
# Here UDLYが平均より大きい結果の条件の下でのpayoffの頻度分布
# hist((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit,
#      breaks="Scott",main="Profit/Loss over initial Underlying Prices")
# points(mean((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit),
#        0, col = "red", pch = 3)
# points(median((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit), 
#        0, col = "orange", pch = 1)
# points(mean((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit)
#        +sd((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit),
#        0,col = "darkgrey", pch = 2)
# points(mean((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit)
#        -sd((subset(res_dtf_,UDLY>mean(position$UDLY)))$Profit),
#        0,col = "darkgrey", pch = 2)

#all cleanings. result file should've been saved. 
rm(profit_each_itr_,udly_price_at_liquidation_,liq_days_,res_dtf_)
rm(histIV,position,StimultaionNum)
rm(StimRslts)

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
