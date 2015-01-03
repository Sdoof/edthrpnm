library(RQuantLib)

#1. Simple Payoff
# x-lay:UDLY, y-lay: Price
#When T advances like T+7, T+14,T+21,T+28、
# IF volatility remains constant(adjust horizental volatility skew only)
#  At T+X, get the position's price assuming UDLY target the price without 
#  probability consideration, only payoff.
#Namely for example, at T+7
# sum(T7$Position*T7$Price) at UDLY at original price. Likewise,
# sum(T7Mns[[i]]$Position*T7Mns[[i]]$Price) at lower than original price.
# sum(T7Pls[[i]]$Position*T7Pls[[i]]$Price) at higher than original price.

#Set Global Variable also set in another file.
file_prefix_simple_G<-"OptionVariablesT"

#XT read
#XT[[1:TimechgNum]]
# XT[[1]],XT[[2]]...
for(i in 1:(NumOfTimeChange_G+1)){
 f_name=paste("OptionVariablesT",as.character((i-1)*ChangTimeUnit_G),".csv",sep="")
 (xt_tmp<-read.table(f_name,header=T,sep=","))
 # Date>ExpDate となっている要素は削除する
 xt_tmp<-subset(xt_tmp,as.Date(Date,format="%Y/%m/%d")<=as.Date(ExpDate,format="%Y/%m/%d"))
  if(i==1){
    XT<-list(xt_tmp)
  } else {
    XT<-c(XT,list(xt_tmp))
  }
}
#XTMns,XTPlus read
#  XTMns[[1:TimechgNum]][[1:PriceChgNum]]
#   XTMns[[1]]
#     XTMns[[1]][[1]],XTMns[[1]][[2]],...
#   XTMns[[2]]
#     XTMns[[2]][[1]],XTMns[[2]][[2]],...
#  XTPls[[TimechgNum]][[PriceChgNum]] likewise

for(i in 1:(NumOfTimeChange_G+1)){
  for(j in 1:(NumOfOnesideStrkPrice_G)){
    #XTMns
    f_name=paste("OptionVariablesT",as.character((i-1)*ChangTimeUnit_G),"StrMns",as.character(j),".csv",sep="")
    (xt_tmp<-read.table(f_name,header=T,sep=","))
    # Date>ExpDate となっている要素は削除する
    xt_tmp<-subset(xt_tmp,as.Date(Date,format="%Y/%m/%d")<=as.Date(ExpDate,format="%Y/%m/%d"))

    #XTPls
    f_name=paste("OptionVariablesT",as.character((i-1)*ChangTimeUnit_G),"StrPlus",as.character(j),".csv",sep="")
    (xt_tmp_pls<-read.table(f_name,header=T,sep=","))
    # Date>ExpDate となっている要素は削除する
    xt_tmp_pls<-subset(xt_tmp_pls,as.Date(Date,format="%Y/%m/%d")<=as.Date(ExpDate,format="%Y/%m/%d"))
    if(j==1){
      xtmns_elem<-list(xt_tmp)
      xtmns_elem_pls<-list(xt_tmp_pls)
    } else {
      xtmns_elem<-c(xtmns_elem,list(xt_tmp))
      xtmns_elem_pls<-c(xtmns_elem_pls,list(xt_tmp_pls))
    }
  }
  if(i==1){
    XTMns<-list(xtmns_elem)
    XTPls<-list(xtmns_elem_pls)
  } else{
    XTMns<-c(XTMns,list(xtmns_elem))
    XTPls<-c(XTPls,list(xtmns_elem_pls))
  }
}

#-sum($Price*$Position)
#  Positionを取った時のCash credit(debtは-)
#sum($Price*$Position) 
#  Positionの反対売買した（closed out)時のcash credit(debtは-)

x_axis_<-list()
y_axis_<-list()
#compute actual NumOfTimeChange_G because of Date > ExpDate
for( time_chg in 1:3){
 #from left to center
 for(prce_chg in 1:(NumOfOnesideStrkPrice_G)){
   x_axis_<-c(x_axis_,list(XTMns[[time_chg]][[NumOfOnesideStrkPrice_G-prce_chg+1]]$UDLY[1]))
   y_axis_<-c(y_axis_,list(sum((XTMns[[time_chg]][[NumOfOnesideStrkPrice_G+1-prce_chg]]$Price - XT[[1]]$Price)*XT[[1]]$Position)))
 }
 #center
 x_axis_<-c(x_axis_,list(XT[[time_chg]]$UDLY[1]))
 y_axis_<-c(y_axis_,list(sum(XT[[time_chg]]$Position*XT[[time_chg]]$Price - XT[[1]]$Position*XT[[1]]$Price)))
 #center to right
 for(prce_chg in 1:NumOfOnesideStrkPrice_G){
   x_axis_<-c(x_axis_,list(XTPls[[time_chg]][[prce_chg]]$UDLY[1]))
   y_axis_<-c(y_axis_,list(sum((XTPls[[time_chg]][[prce_chg]]$Price - XT[[1]]$Price)*XT[[1]]$Position)))
 }
}

#plot(x_axis_,y_axis_)

#2. Expected Return Scenario Stimulation
#Advance T 1 by 1. Get Payoff Dist. Calc Expected Payoff.
#The most basic price movement is Geometric Random Walk.
#
#  If.  1. Price moves with Geometric Random Walk
#       2. volatility remains constant
#       3. There is no vertical/horizontal/price movement
#          volatility skews
#  Expected Payoff should be the Price position values at T+x day.
# 
#  But We should investigate: 
#    1. The Effect of volatility Skewness
#    2. The Effect of volatility Changes. Up trend/Down trend
#    3. The Effect of Underlying Price Trend
#    4. The Effect of Mechanical Position Adjust(Liquidation)
#       when Forward Risk/Return Ration becomes unfavorable.
#
#  We use only T0. then Monte-Carlo based on various scenario.
#

# Import various volatility skew functions included in other files.
# Import Geometric Brown Motion Stimulation function

#Functions Defined

volatility.surface.skew <- function(xTb,xTa){
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xTb$Date,format="%Y/%m/%d"),
                                  as.Date(xTb$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xTa$Date,format="%Y/%m/%d"),
                                  as.Date(xTa$ExpDate,format="%Y/%m/%d"))
  vol_ret_<-horizental.volatility.skew(vol_b=xTb$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  vol_ret_
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

#Stimulation processed on this data frame
XTStim<-XT[[1]]
XTOrig<-XT[[1]]
#Stimulation day
stim_days_num=12
#underlying daily return for geometic brown motion
mu_udly<-(1.03)^(1/252)-1
#underlying initial daily volatility.
#HV should be used? or IV should be used?
sigma_udly<-0.2/sqrt(252)

#Prepare the stored values
positionProfit <- rep(0, times=stim_days_num)
PositionDataframe <- NULL
PositionEvalScores <- NULL

####
###Scenario 1.
###
#volatility never changes throught the stimulation.

#get the underlying changed of all days.
s0<-XTStim$UDLY[[1]]
udly_prices <- geombrmtn.stimulate <- geombrmtn.stimulate(s0=s0,mu=mu_udly,sigma=sigma_udly,length=stim_days_num)
udly_prices <- udly_prices[-1]

for(day_chg in 1:stim_days_num){

  #Just for comparison later.
  XTStim_b<-XTStim

  #Advance day_chg day
  XTStim$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(XTOrig$Date,format="%Y/%m/%d"),day_chg,0),"%Y/%m/%d")
  
  #Underlying Price change
  XTStim$UDLY <- rep(udly_prices[day_chg],times=length(XTStim$UDLY))

  #Volatiliy Vertical Skew adjustment
  XTStim$OrigIV <- XTStim$OrigIV+vertical.volatility.skew(xTb=XTStim_b,xTa=XTStim)
  #Underlying Price change volatility skew
  XTStim$OrigIV <- XTStim$OrigIV+Underlying.PriceChange.volatility.skew(xTb=XTStim_b,xTa=XTStim)
  #Horizental Volatility SKew adjustment
  XTStim$OrigIV <- XTStim$OrigIV+volatility.surface.skew(xTb=XTStim_b,xTa=XTStim)

  #Set new Price and Greeks
  #Case of European Option
  tmp_<-set.EuropeanOptionValueGreeks(XTStim)
  XTStim$Price<-tmp_$Price
  XTStim$Delta<-tmp_$Delta
  XTStim$Gamma<-tmp_$Gamma
  XTStim$Vega<-tmp_$Vega
  XTStim$Theta<-tmp_$Theta
  XTStim$Rho<-tmp_$Theta

  #Case of American Option
  #TBD

  #Position Profit
  positionProfit[day_chg]<-(sum(XTStim$Position*XTStim$Price) - sum(XTOrig$Position*XTOrig$Price))

  #Evaluation Function to be used for position adjustment(liqudation)
  #Suchs as DTRRR, VTRRR, RTRRR. TBD
  #positionProfit[day_chg] may be used.
  if(day_chg==1){
    PositionEvalScores<-list(c(0))
  }else{
    PositionEvalScores<-c(PositionEvalScores,list(c(0)))
  }
  #Position adjustment(liqudation)
  #to be defined later

  #Current XTStim appened to a list
  #Data frame to be stored. Greeks and Everything
  if(day_chg==1){
    PositionDataframe<-list(XTStim)
  }else{
    PositionDataframe<-c(PositionDataframe,list(XTStim))
  }
  #advance a day. loop.
}




#Back Test Functions.
#To Be Designed Later.