#Set IV
set.IVOrig <- function(xT){
  xT$OrigIV<-rep(0,times=length(xT$TYPE))
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == OpType_Put_G){
      xT$OrigIV[i]=EuropeanOptionImpliedVolatility(type="put", value=xT$Price[i],underlying=xT$UDLY[i],
                                                   strike=xT$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
                                                   maturity=busdays_betwn/252,volatility=0.2)
    }else if(xT$TYPE[i] == OpType_Call_G){
      xT$OrigIV[i]=EuropeanOptionImpliedVolatility(type="call", value=xT$Price[i],underlying=xT$UDLY[i],
                                                   strike=xT$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
                                                   maturity=busdays_betwn/252,volatility=0.2)
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
    if(xT$TYPE[i] == OpType_Put_G){
      C0_tmp<-EuropeanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/252, volatility=xT$OrigIV[i])
      
    }else if(xT$TYPE[i] == OpType_Call_G){
      C0_tmp<-EuropeanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/252, volatility=xT$OrigIV[i])
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
  tmp_ret[[4]]<-vega/100
  tmp_ret[[5]]<-theta/252
  tmp_ret[[6]]<-rho/252
  names(tmp_ret)<-Grknames
  tmp_ret
}

##
# calculate and return of vomma of the spread position.
# xT : spread postion data frame
# return value: position vomma as a vector. 
#   you may use this like this. 
#   thePosition$vomma <- get.EuropeanOptionvomma(xT=thePosition)
get.EuropeanOptionVomma <- function(xT){
  # 1%
  h <- 10^(-2)
  #first allocate vector returned.
  vomma<-rep(0,length(xT$TYPE))
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    
    if(xT$TYPE[i] == OpType_Put_G){
      tmp<-EuropeanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/252, volatility=xT$OrigIV[i]+h)
      VegaPlus<-tmp$vega/100
      
      tmp<-EuropeanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/252, volatility=xT$OrigIV[i]-h)
      VegaMinus<-tmp$vega/100
      
    }else if(xT$TYPE[i] == OpType_Call_G){
      tmp<-EuropeanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                          dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/252, volatility=xT$OrigIV[i]+h)
      VegaPlus<-tmp$vega/100
      
      tmp<-EuropeanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                          dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/252, volatility=xT$OrigIV[i]-h)
      VegaMinus<-tmp$vega/100
    }
   vomma[i] <- (VegaPlus - VegaMinus)/2
  }
  vomma
}
