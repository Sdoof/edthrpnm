#Set IV
set.IVOrig <- function(xT){
  xT$OrigIV<-rep(0,times=length(xT$TYPE))
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
set.EuropeanOptionValueGreeks <- function(xT){
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
  
  Grknames<-c("Price","Delta","Gamma","Vega","Theta","Rho")
  tmp_ret<-vector("list",length(Grknames))
  tmp_ret[[1]]<-value
  tmp_ret[[2]]<-delta
  tmp_ret[[3]]<-gamma
  tmp_ret[[4]]<-vega
  tmp_ret[[5]]<-theta
  tmp_ret[[6]]<-rho 
  names(tmp_ret)<-Grknames
  tmp_ret
}