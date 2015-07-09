###Start
library(RQuantLib)
##
# Functions to be loaded -------------------------

#  Greekの計算と設定

#IV (Original IV)
set.IVOrig <- function(xT){
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == 1){
      xT$OrigIV[i]=EuropeanOptionImpliedVolatility(type="put", value=xT$Price[i],underlying=xT$UDLY[i],
                                                   strike=xT$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
                                                   maturity=busdays_betwn/365,volatility=0.2)
    }else if(xT$TYPE[i] == -1){
      xT$OrigIV[i]=EuropeanOptionImpliedVolatility(type="call", value=xT$Price[i],underlying=xT$UDLY[i],
                                                   strike=xT$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
                                                   maturity=busdays_betwn/365,volatility=0.2)
    }
  }
  xT$OrigIV
}


#Set Value Greeks
set.ValueGreeks <- function(xT){
  value<-rep(0,length(xT$TYPE))
  delta<-rep(0,length(xT$TYPE))
  gamma<-rep(0,length(xT$TYPE))
  vega<-rep(0,length(xT$TYPE))
  theta<-rep(0,length(xT$TYPE))
  rho<-rep(0,length(xT$TYPE))
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == 1){
      C0_tmp<-EuropeanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT$OrigIV[i])
      
    }else if(xT$TYPE[i] == -1){
      C0_tmp<-EuropeanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT$OrigIV[i])
    }
    value[i]<-C0_tmp$value
    delta[i]<- C0_tmp$delta
    gamma[i]<- C0_tmp$gamma
    vega[i]<- C0_tmp$vega
    theta[i]<- C0_tmp$theta
    rho[i]<- C0_tmp$rho
  }
  tmp_ret<-list(value)
  tmp_ret<-c(tmp_ret,list(delta))
  tmp_ret<-c(tmp_ret,list(gamma))
  tmp_ret<-c(tmp_ret,list(vega))
  tmp_ret<-c(tmp_ret,list(theta))
  tmp_ret<-c(tmp_ret,list(rho))
  tmp_ret
}