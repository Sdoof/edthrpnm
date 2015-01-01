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

#Set Global Variable set in another file.

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


#  xT21StrMns<-c(xT21StrMns,list(xT21_a))
# T<-list(T,xt)

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


#Back Test Functions.
#To Be Designed Later.