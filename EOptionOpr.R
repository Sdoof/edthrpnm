
#  Greekの計算と設定
set.AmericanIVOrig <- function(xT){
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == 1){
      xT$OrigIV[i]=AmericanOptionImpliedVolatility(type="put", value=xT$Price[i],underlying=xT$UDLY[i],
                                                   strike=xT$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
                                                   maturity=busdays_betwn/252,volatility=0.2)
    }else if(xT$TYPE[i] == -1){
      xT$OrigIV[i]=AmericanOptionImpliedVolatility(type="call", value=xT$Price[i],underlying=xT$UDLY[i],
                                                   strike=xT$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
                                                   maturity=busdays_betwn/252,volatility=0.2)
    }
  }
  xT$OrigIV
}

# Set Value Delta Gamma
set.AmericanValueDeltaGamma <- function(xT){
  value<-rep(0,length(xT$TYPE))
  delta<-rep(0,length(xT$TYPE))
  gamma<-rep(0,length(xT$TYPE))
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == 1){
      C0_tmp<-AmericanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT$OrigIV[i],
                             timeSteps=150, gridPoints=149, engine="CrankNicolson")
    }else if(xT$TYPE[i] == -1){
      C0_tmp<-AmericanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT$OrigIV[i],
                             timeSteps=150, gridPoints=149, engine="CrankNicolson")
    }
    value[i]<-C0_tmp$value
    delta[i]<- C0_tmp$delta
    gamma[i]<- C0_tmp$gamma
  }
  append(append(value,delta),gamma)
}

#1. Delta, Gamma
set.AmericanDeltaGamma <- function(xT){
  delta<-rep(0,length(xT$TYPE))
  gamma<-rep(0,length(xT$TYPE))
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == 1){
      C0_tmp<-AmericanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT$OrigIV[i],
                             timeSteps=150, gridPoints=149, engine="CrankNicolson")
    }else if(xT$TYPE[i] == -1){
      C0_tmp<-AmericanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT$OrigIV[i],
                             timeSteps=150, gridPoints=149, engine="CrankNicolson")
    }
    delta[i]<- C0_tmp$delta
    gamma[i]<- C0_tmp$gamma
  }
  append(delta,gamma)
}

#2.Vega
set.AmericanVega <- function(xT){
  h <- 10^(-2)
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == 1){
      Cplus <- (AmericanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                               dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
                               volatility=xT$OrigIV[i]+h,
                               timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
      Cminus <- (AmericanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                                dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
                                volatility=xT$OrigIV[i]-h,
                                timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
    } else if(xT$TYPE[i] == -1){
      Cplus <- (AmericanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                               dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
                               volatility=xT$OrigIV[i]+h,
                               timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
      Cminus <- (AmericanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                                dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
                                volatility=xT$OrigIV[i]-h,
                                timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
    }
    # a central difference
    xT$Vega[i] <- (Cplus - Cminus)/2
  }
  xT$Vega
}

#3. Theta
set.AmericanTheta <- function(xT){
  h <- 1
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == 1){
      Cminus<- (AmericanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                               dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=(busdays_betwn-h)/365,
                               volatility=xT$OrigIV[i],
                               timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
    } else if(xT$TYPE[i] == -1){
      Cminus<- (AmericanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                               dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=(busdays_betwn-h)/365,
                               volatility=xT$OrigIV[i],
                               timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
    }
    # a central difference
    xT$Theta[i] <- (Cminus-xT$Price[i])
  }
  xT$Theta
}


#4. Rho
set.AmericanRho <- function(xT){
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween(CALENDAR_G,
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == 1){
      xT$Rho[i]<-(EuropeanOption(type="put", underlying=xT$UDLY[i], strike=xT$Strike[i],
                                 dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
                                 volatility=xT$OrigIV[i]))$rho/100
    } else if(xT$TYPE[i] == -1){
      xT$Rho[i]<-(EuropeanOption(type="call", underlying=xT$UDLY[i], strike=xT$Strike[i],
                                 dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
                                 volatility=xT$OrigIV[i]))$rho/100
    }
  }
  xT$Rho
}