###Start
library(RQuantLib)

## Read a txt file(csv file)
(xT0<-read.table("OptionVariables.csv",header=T,sep=","))

## Tx later data stored.
#Here Simple stimulation
#Only taking Horizental Volatility Skew into accounted.
#No taking into account of GENERAL Volatility TREND.
#Change x$Date and recalucate related values.
#More elabolated stimulation should be done somewhere.
#xT7
#xT14
#xT21
#xT28
#xT35

###Global 変数及び定数.
# Possibly read from File
riskFreeRate_G=0.01
divYld_G=0.0
NumOfOnesideStrkPrice_G=4
ChangStrkPrUnit_G=10

#xT0の列のアクセス
#xT0$ContactName
#xT0[[2]]
#str_v<-xT0$Strike
#str_v<-xT0[[7]

##現在のポジションの列を取って演算用のVector
#cur_pos<-xT0$Position
#cur_pos<-xT0[[1]]

##計算に基づきその値を変化させる
# 例えば、
#   Delta合計
#     sum(cur_pos*xT0$Delta)
#   Gamma合計
#     sum(cur_pos*xT0$Gamma)
#   Vega合計
#     sum(cur_pos*xT0$Vega)
#   Theta合計
#     sum(cur_pos*xT0$Theta)
#   Rho合計
#     sum(cur_pos*xT0$Rho)
#   Price合計（意味ないけど）
#     sum(cur_pos*xT0$Price)
#   Intrisic Value
#    TYPE:1 PUT, -1 CALL, Underlying 0
#     intr_t <- (xT0$UDLY-xT0$Strike)*xT0$TYPE
#     if(intr_t<0) intr_t<-0
#   TIME Value
#      xT0$Price-intr_t

#  Greekの計算と設定

#0. IV (Original IV)

##検算用
#(xT0$Price[1])
#(xT0$UDLY[1])
#(xT0$Strike[1])
#date_today <-as.Date(xT0$Date[1],format="%Y/%m/%d")
#date_exp <-as.Date(xT0$ExpDate[1],format="%Y/%m/%d")
## Week days 簡易計算。week数*5 + 余り。
#(days_diff=as.numeric(difftime(date_exp,date_today,tz="",units="days")))
#(days_diff_week<-((days_diff%/%7)*5+(days_diff%%7)))
## Instead, Use RQuantLabの関数
#(busdays_betwn<-businessDaysBetween("UnitedStates/NYSE", date_today, date_exp))
#(busdays_betwn<-businessDaysBetween("UnitedStates", date_today, date_exp))
#xT0$OrigIV[1]=AmericanOptionImpliedVolatility(type="put", value=xT0$Price[1],underlying=xT0$UDLY[1],
#                                            strike=xT0$Strike[1],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
#                                           maturity=days_diff_week/365,volatility=0.2)
##EOF 検算
#

set.IVOrig <- function(xT){
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
                                       as.Date(xT$Date[i],format="%Y/%m/%d"),
                                       as.Date(xT$ExpDate[i],format="%Y/%m/%d"))
    if(xT$TYPE[i] == 1){
      xT$OrigIV[i]=AmericanOptionImpliedVolatility(type="put", value=xT$Price[i],underlying=xT$UDLY[i],
                                                    strike=xT$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
                                                    maturity=busdays_betwn/365,volatility=0.2)
    }else if(xT$TYPE[i] == -1){
      xT$OrigIV[i]=AmericanOptionImpliedVolatility(type="call", value=xT$Price[i],underlying=xT$UDLY[i],
                                                    strike=xT$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
                                                    maturity=busdays_betwn/365,volatility=0.2)
    }
  }
  xT$OrigIV
}

xT0$OrigIV<-set.IVOrig(xT=xT0)

#for(i in 1:length(xT0$TYPE)){
  ##  Business days
#  busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
#                                      as.Date(xT0$Date[i],format="%Y/%m/%d"),
#                                      as.Date(xT0$ExpDate[i],format="%Y/%m/%d"))
#  if(xT0$TYPE[i] == 1){
#    xT0$OrigIV[i]=AmericanOptionImpliedVolatility(type="put", value=xT0$Price[i],underlying=xT0$UDLY[i],
#                                     strike=xT0$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
#                                     maturity=busdays_betwn/365,volatility=0.2)
#  }else if(xT0$TYPE[i] == -1){
#    xT0$OrigIV[i]=AmericanOptionImpliedVolatility(type="call", value=xT0$Price[i],underlying=xT0$UDLY[i],
#                                                strike=xT0$Strike[i],dividendYield=divYld_G,riskFreeRate=riskFreeRate_G,
#                                                maturity=busdays_betwn/365,volatility=0.2)
#  }
#}

# Greeks
#RQualntLab
#  AmericanOption(type, underlying, strike,
#                 dividendYield, riskFreeRate, maturity, volatility,
#                 timeSteps=150, gridPoints=149, engine="BaroneAdesiWhaley")
#  
#   Arguments
#      type          A string with one of the values "call" or "put"
#      underlying    Current price of the underlying stock
#      strike        Strike price of the option
#      dividendYield Continuous dividend yield (as a fraction) of the stock
#      riskFreeRate  Risk-free rate
#      maturity      Time to maturity (in fractional years)
#      volatility    Volatility of the underlying stock
#      timeSteps     Time steps for the “CrankNicolson” finite differences method engine,
#                    default value is 150
#      gridPoints    Grid points for the “CrankNicolson” finite differences method,
#                    default value is 149
#      engine        String selecting pricing engine, currently supported are 
#                    “BaroneAdesiWhaley” and “CrankNicolson”
#   Value
#
#       value    Value of option
#       delta   Sensitivity of the option value for a change in the underlying
#       gamma   Sensitivity of the option delta for a change in the underlying
#       vega    Sensitivity of the option value for a change in the underlying’s volatility
#       theta   Sensitivity of the option value for a change in t, the remaining time to maturity
#       rho     Sensitivity of the option value for a change in the risk-free interest rate
#       dividendRho Sensitivity of the option value for a change in the dividend yield

##Start of 検算用。Later We apply for loop

#Delta,gamma
#時間差
#(busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
#                                   as.Date(xT0$Date[1],format="%Y/%m/%d"),
#                                   as.Date(xT0$ExpDate[1],format="%Y/%m/%d")))
#(xT0$Price[1]);(xT0$UDLY[1]);(xT0$Strike[1]);(xT0$OrigIV[1])
#Ametican Option
#(C0<-AmericanOption(type="put", underlying=xT0$UDLY[1], strike=xT0$Strike[1],
#               dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT0$OrigIV[1],
#               timeSteps=150, gridPoints=149, engine="CrankNicolson"))

##Numerical Calculation of Vega Theta Rho

#First Vega
#h <- 10^(-(0:10))  ## a vector of values
#vegaVec <- numeric(length(h))
#CplusVec <- numeric(length(h))
#CminusVec <- numeric(length(h))
#for (i in seq(along.with = h)) {
#  CplusVec[i] <- (AmericanOption(type="put", underlying=xT0$UDLY[1], strike=xT0$Strike[1],
#                           dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
#                           volatility=xT0$OrigIV[1]+h[i],
#                           timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
#  CminusVec[i] <- (AmericanOption(type="put", underlying=xT0$UDLY[1], strike=xT0$Strike[1],
#                           dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
#                           volatility=xT0$OrigIV[1]-h[i],
#                           timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
#  ## a central difference
#  vegaVec[i] <- (CplusVec[i] - CminusVec[i])/2
#}
#the analytic solution
#C0$value
#data.frame(h = h, Cplus = CplusVec)
#data.frame(h = h,  CminusVec = CminusVec)
#data.frame(h = h, vega = vegaVec)
#this case h 1e-02(0.01)で大丈夫そう。0.5998135 (Europeanでは59.9909)
#可能ならば、小さい方から値を比較して、その差が一定値を超える
#手前の値をvegaの値とするべき。
#ここでは、h 0.01->0.1で-5.050212(0.5998135->5.650025)と大きく変化している
#1%と10%の変化だから当たり前だが。
#ちなみに、h 0.001->0.01では0.5397986（これも0.06->0.59)と大きく変化
#C0$vega = vegaVec[3]

#Second Theta
#day move
#h <- 10^(-(0:10))  ## a vector of values
#thetaDayVec <- numeric(length(h))
#CminusVec <- numeric(length(h))
#for (i in seq(along.with = h)) {
#  CminusVec[i] <- (AmericanOption(type="put", underlying=xT0$UDLY[1], strike=xT0$Strike[1],
#                                  dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=(busdays_betwn-h[i])/365,
#                                  volatility=xT0$OrigIV[1],
#                                  timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
#  ## a central difference
#  thetaDayVec[i] <- (CminusVec[i]-C0$value)
#}
# the analytic solution
#C0$value
#data.frame(h = h,  CminusVec = CminusVec)
#data.frame(h = h, thetaDay = thetaDayVec)
#値は正しそうなので、
#C0$theta = thetaDayVec[1]

##Next Rho. We approximate the rho value using EuropeanOption
#rho_tmp<-(EuropeanOption(type="put", underlying=xT0$UDLY[1], strike=xT0$Strike[1],
#                dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
#                volatility=xT0$OrigIV[1]))$rho
#C0$rho<-rho_tmp/100

#Using EuropeanOption as an approximation possible?
# Approximately Correct. Probably No ploblem approximating using EuropeanOption Greeks
# Note: Vega should be Vega/100, theta should be Theta/365
#(EuropeanOption(type="put", underlying=xT0$UDLY[1], strike=xT0$Strike[1],
#              dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT0$OrigIV[1]))
##EOF 検算用

#0. Set Value Delta Gamma
set.ValueDeltaGamma <- function(xT){
  value<-rep(0,length(xT$TYPE))
  delta<-rep(0,length(xT$TYPE))
  gamma<-rep(0,length(xT$TYPE))
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
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

set.DeltaGamma <- function(xT){
  delta<-rep(0,length(xT$TYPE))
  gamma<-rep(0,length(xT$TYPE))
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
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

(deltagamma_tmp<-set.DeltaGamma(xT0))
xT0$Delta<-deltagamma_tmp[1:length(xT0$Delta)]
xT0$Gamma<-deltagamma_tmp[length(xT0$Delta)+1:length(xT0$Gamma)]

#for(i in 1:length(xT0$TYPE)){
  ##  Business days
#  busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
#                                     as.Date(xT0$Date[i],format="%Y/%m/%d"),
#                                    as.Date(xT0$ExpDate[i],format="%Y/%m/%d"))
#  if(xT0$TYPE[i] == 1){
#   C0_tmp<-AmericanOption(type="put", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                   dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT0$OrigIV[i],
#                   timeSteps=150, gridPoints=149, engine="CrankNicolson")
#  }else if(xT0$TYPE[i] == -1){
#    C0_tmp<-AmericanOption(type="call", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                           dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365, volatility=xT0$OrigIV[i],
#                           timeSteps=150, gridPoints=149, engine="CrankNicolson")
#  }
#  xT0$Delta[i] <- C0_tmp$delta
#  xT0$Gamma[i] <- C0_tmp$gamma
#}

#2.Vega

set.Vega <- function(xT){
  h <- 10^(-2)
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
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

xT0$Vega<-set.Vega(xT=xT0)

#for(i in 1:length(xT0$TYPE)){
#  h <- 10^(-2)
#  ##  Business days
#  busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
#                                     as.Date(xT0$Date[i],format="%Y/%m/%d"),
#                                     as.Date(xT0$ExpDate[i],format="%Y/%m/%d"))
#  if(xT0$TYPE[i] == 1){
#    Cplus <- (AmericanOption(type="put", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                                dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
#                                volatility=xT0$OrigIV[i]+h,
#                                timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
#    Cminus <- (AmericanOption(type="put", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                                 dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
#                                 volatility=xT0$OrigIV[i]-h,
#                                 timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
# 
#  }else if(xT0$TYPE[i] == -1){
#    Cplus <- (AmericanOption(type="call", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                                dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
#                                volatility=xT0$OrigIV[i]+h,
#                                timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
#    Cminus <- (AmericanOption(type="call", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                                 dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
#                                 volatility=xT0$OrigIV[i]-h,
#                                 timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
#  }
#  # a central difference
#  vega_tmp <- (Cplus - Cminus)/2
#  xT0$Vega[i]<-vega_tmp
#}

#3. Theta

set.Theta <- function(xT){
  h <- 1
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
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

xT0$Theta<-set.Theta(xT=xT0)

#for(i in 1:length(xT0$TYPE)){
#  h <- 1
#  Cminus<-0
#  ##  Business days
#  busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
#                                     as.Date(xT0$Date[i],format="%Y/%m/%d"),
#                                     as.Date(xT0$ExpDate[i],format="%Y/%m/%d"))
#  if(xT0$TYPE[i] == 1){
#    Cminus<- (AmericanOption(type="put", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                                dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=(busdays_betwn-h)/365,
#                                volatility=xT0$OrigIV[i],
#                                timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
#    
#  }else if(xT0$TYPE[i] == -1){
#    Cminus<- (AmericanOption(type="call", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                             dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=(busdays_betwn-h)/365,
#                             volatility=xT0$OrigIV[i],
#                             timeSteps=150, gridPoints=149, engine="CrankNicolson"))$value
#  }
  # a central difference
#  xT0$Theta[i] <- (Cminus-xT0$Price[i])
#}

#4. Rho

set.Rho <- function(xT){
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
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

xT0$Rho<-set.Rho(xT=xT0)

#for(i in 1:length(xT0$TYPE)){
#  ##  Business days
#  busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
#                                     as.Date(xT0$Date[i],format="%Y/%m/%d"),
#                                     as.Date(xT0$ExpDate[i],format="%Y/%m/%d"))
#  if(xT0$TYPE[i] == 1){
#    xT0$Rho[i]<-(EuropeanOption(type="put", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                                   dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
#                                    volatility=xT0$OrigIV[i]))$rho/100
#  }else if(xT0$TYPE[i] == -1){
#    xT0$Rho[i]<-(EuropeanOption(type="call", underlying=xT0$UDLY[i], strike=xT0$Strike[i],
#                    dividendYield=divYld_G, riskFreeRate=riskFreeRate_G, maturity=busdays_betwn/365,
#                    volatility=xT0$OrigIV[i]))$rho/100
#  }
#}

## xT0StrMns[[]],xT0StrPlus[[]] as list

##Test 検算用
#xT0_t<-xT0
##1. Underlying Price Change
#xT0_t$UDLY<-(xT0$UDLY-30)

#oom_mgn_before <- xT0$TYPE*(xT0$UDLY-xT0$Strike)
#oom_mgn_after <- xT0_t$TYPE*(xT0_t$UDLY-xT0_t$Strike)

#(vol_dif_rate <- rep(0,length(xT0$TYPE)))
#K1<-0.9
#K2<-0.9

(vol_dif_rate<-rep(0,length(xT0$TYPE)))

##OOM to OOM
#(vol_dif_rate <-vol_dif_rate+((abs(oom_mgn_after)/xT0_t$UDLY)*K1-(abs(oom_mgn_before)/xT0$UDLY)*K1)*as.numeric(oom_mgn_before>=0)*as.numeric(oom_mgn_after>=0))

##OOM to ITM
#vol_dif_rate <- vol_dif_rate+((abs(oom_mgn_after)/xT0_t$UDLY)*K2-(abs(oom_mgn_before)/xT0$UDLY)*K1)*as.numeric(oom_mgn_after<0)

##ITM to OTM
#vol_dif_rate <-vol_dif_rate+((abs(oom_mgn_after)/xT0_t$UDLY)*K1-(abs(oom_mgn_before)/xT0$UDLY)*K2)*as.numeric(oom_mgn_before<0)*as.numeric(oom_mgn_after>=0)

##ITM ot ITM
#(vol_dif_rate <-vol_dif_rate+((abs(oom_mgn_after)/xT0_t$UDLY)*K2-(abs(oom_mgn_before)/xT0$UDLY)*K2)*as.numeric(oom_mgn_before<0)*as.numeric(oom_mgn_after<0))

##Volatility Modelingに関しては後できちんとする。
##ここでは簡易版
#Underlyingの価格の変化によるVolatilityの変化
vertical.volatility.skew <- function(xTb,xTa){
  ## OOMにどれだけ離れているか。UDLYとの比率。正の数はOOM。負の数はITM
  oom_mgn_before <- xTb$TYPE*(xTb$UDLY-xTb$Strike)
  oom_mgn_after <- xTa$TYPE*(xTa$UDLY-xTa$Strike)
  
  #ここでは、UDLY値との比に比例して簡単な直線(linier)で
  #VOlatilityの変化率が決定するとする。
  #K1 OOMの係数。K2 ITMの係数
  K1<-0.9
  K2<-0.9
  
  (vol_dif_rate<-rep(0,length(xTb$TYPE)))
  
  ##OOM to OOM
  (vol_dif_rate <-vol_dif_rate+((abs(oom_mgn_after)/xTa$UDLY)*K1-(abs(oom_mgn_before)/xTb$UDLY)*K1)*as.numeric(oom_mgn_before>=0)*as.numeric(oom_mgn_after>=0))
  
  ##OOM to ITM
  vol_dif_rate <- vol_dif_rate+((abs(oom_mgn_after)/xTa$UDLY)*K2-(abs(oom_mgn_before)/xTb$UDLY)*K1)*as.numeric(oom_mgn_after<0)
  
  ##ITM to OTM
  vol_dif_rate <-vol_dif_rate+((abs(oom_mgn_after)/xTa$UDLY)*K1-(abs(oom_mgn_before)/xTb$UDLY)*K2)*as.numeric(oom_mgn_before<0)*as.numeric(oom_mgn_after>=0)
  
  ##ITM ot ITM
  (vol_dif_rate <-vol_dif_rate+((abs(oom_mgn_after)/xTa$UDLY)*K2-(abs(oom_mgn_before)/xTb$UDLY)*K2)*as.numeric(oom_mgn_before<0)*as.numeric(oom_mgn_after<0))
  
  vol_dif_rate
}

#価格変化による全体的なvolatilityの変化
#To be defined
Underlying.PriceChange.volatility.skew <- function(xTb,xTa){
  vol_dif_rate<-rep(0,length(xTb$TYPE))
  vol_dif_rate
}

##
##ListとしてxT0StrMnsを管理
##
#1つ目.Listとし宣言。
xT0_a<-xT0
#1. Underlying Price Change
xT0_a$UDLY<-(xT0$UDLY-1*ChangStrkPrUnit_G)
#2. Volatility Skew Change
xT0_a$OrigIV <- xT0$OrigIV+Underlying.PriceChange.volatility.skew(xT0,xT0_a)
xT0_a$OrigIV <- xT0_a$OrigIV+vertical.volatility.skew(xT0,xT0_a)
#3.Price Delta Gamma
valdeltagamma_tmp<-set.ValueDeltaGamma(xT0_a)
xT0_a$Price<-valdeltagamma_tmp[1:length(xT0_a$Price)]
xT0_a$Delta<-valdeltagamma_tmp[length(xT0_a$Price)+1:length(xT0_a$Delta)]
xT0_a$Gamma<-valdeltagamma_tmp[length(xT0_a$Price)+length(xT0$Delta)+1:length(xT0_a$Gamma)]
#4. Vega
xT0_a$Vega<-set.Vega(xT=xT0_a)
#5. Theta
xT0_a$Theta<-set.Theta(xT=xT0_a)
#6. Rho
xT0_a$Rho<-set.Rho(xT=xT0_a)

#Listに追加
xT0StrMns<-list(xT0_a)
for(i in 2:NumOfOnesideStrkPrice_G){
  xT0_a<-xT0
  #1. Underlying Price Change
  xT0_a$UDLY<-(xT0$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT0_a$OrigIV <- xT0$OrigIV+Underlying.PriceChange.volatility.skew(xT0,xT0_a)
  xT0_a$OrigIV <- xT0_a$OrigIV+vertical.volatility.skew(xT0,xT0_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT0_a)
  xT0_a$Price<-valdeltagamma_tmp[1:length(xT0_a$Price)]
  xT0_a$Delta<-valdeltagamma_tmp[length(xT0_a$Price)+1:length(xT0_a$Delta)]
  xT0_a$Gamma<-valdeltagamma_tmp[length(xT0_a$Price)+length(xT0$Delta)+1:length(xT0_a$Gamma)]
  #4. Vega
  xT0_a$Vega<-set.Vega(xT=xT0_a)
  #5. Theta
  xT0_a$Theta<-set.Theta(xT=xT0_a)
  #6. Rho
  xT0_a$Rho<-set.Rho(xT=xT0_a)
  #Listに追加
  xT0StrMns<-c(xT0StrMns,list(xT0_a))
}

##ListとしてxT0StrPlusを管理
#1つ目.Listとし宣言。
xT0_a<-xT0
#1. Underlying Price Change
xT0_a$UDLY<-(xT0$UDLY+1*ChangStrkPrUnit_G)
#2. Volatility Change
xT0_a$OrigIV <- xT0$OrigIV+Underlying.PriceChange.volatility.skew(xT0,xT0_a)
xT0_a$OrigIV <- xT0_a$OrigIV+vertical.volatility.skew(xT0,xT0_a)
#3.Price Delta Gamma
valdeltagamma_tmp<-set.ValueDeltaGamma(xT0_a)
xT0_a$Price<-valdeltagamma_tmp[1:length(xT0_a$Price)]
xT0_a$Delta<-valdeltagamma_tmp[length(xT0_a$Price)+1:length(xT0_a$Delta)]
xT0_a$Gamma<-valdeltagamma_tmp[length(xT0_a$Price)+length(xT0$Delta)+1:length(xT0_a$Gamma)]
#4. Vega
xT0_a$Vega<-set.Vega(xT=xT0_a)
#5. Theta
xT0_a$Theta<-set.Theta(xT=xT0_a)
#6. Rho
xT0_a$Rho<-set.Rho(xT=xT0_a)
#Listに追加
xT0StrPlus<-list(xT0_a)
for(i in 2:NumOfOnesideStrkPrice_G){
  xT0_a<-xT0
  #1. Underlying Price Change
  xT0_a$UDLY<-(xT0$UDLY+i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT0_a$OrigIV <- xT0$OrigIV+Underlying.PriceChange.volatility.skew(xT0,xT0_a)
  xT0_a$OrigIV <- xT0_a$OrigIV+vertical.volatility.skew(xT0,xT0_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT0_a)
  xT0_a$Price<-valdeltagamma_tmp[1:length(xT0_a$Price)]
  xT0_a$Delta<-valdeltagamma_tmp[length(xT0_a$Price)+1:length(xT0_a$Delta)]
  xT0_a$Gamma<-valdeltagamma_tmp[length(xT0_a$Price)+length(xT0$Delta)+1:length(xT0_a$Gamma)]
  #4. Vega
  xT0_a$Vega<-set.Vega(xT=xT0_a)
  #5. Theta
  xT0_a$Theta<-set.Theta(xT=xT0_a)
  #6. Rho
  xT0_a$Rho<-set.Rho(xT=xT0_a)
  #Listに追加
  xT0StrPlus<-c(xT0StrPlus,list(xT0_a))
}
##EOF xT0

##
## xT7,xT14,xT21,xT28,xT35
##

## xT7
xT7<-xT0
#1. Advence date (business day) 
# 時間を進めた結果、ExpDateを超えたrowが存在する場合
tmp_<-advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),7,0) > as.Date(xT0$ExpDate,format="%Y/%m/%d")
if (sum(as.numeric(tmp_))>0){
  #break;
}
xT7$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),7,0),"%Y/%m/%d")

#2. Volatility Horizental Skew adjustment
maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT0$Date,format="%Y/%m/%d"),
                                as.Date(xT0$ExpDate,format="%Y/%m/%d"))
maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT7$Date,format="%Y/%m/%d"),
                                as.Date(xT7$ExpDate,format="%Y/%m/%d"))
#orizental.volatility.skew defined on EOptionOprEurop
#xT7$OrigIV <- xT7$OrigIV+horizental.volatility.skew(vol_b=xT7$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)

#3.Price Grek
valdeltagamma_tmp<-set.ValueDeltaGamma(xT7)
xT7$Price<-valdeltagamma_tmp[1:length(xT7$Price)]
xT7$Delta<-valdeltagamma_tmp[length(xT7$Price)+1:length(xT7$Delta)]
xT7$Gamma<-valdeltagamma_tmp[length(xT7$Price)+length(xT7$Delta)+1:length(xT7$Gamma)]
xT7$Theta<-set.Theta(xT7)
xT7$Rho<-set.Rho(xT7)

##xT7StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT7_a<-xT7
  #1. Underlying Price Change
  xT7_a$UDLY<-(xT7$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT7_a$OrigIV <- xT7$OrigIV+Underlying.PriceChange.volatility.skew(xT7,xT7_a)
  xT7_a$OrigIV <- xT7_a$OrigIV+vertical.volatility.skew(xT7,xT7_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT7_a)
  xT7_a$Price<-valdeltagamma_tmp[1:length(xT7_a$Price)]
  xT7_a$Delta<-valdeltagamma_tmp[length(xT7_a$Price)+1:length(xT7_a$Delta)]
  xT7_a$Gamma<-valdeltagamma_tmp[length(xT7_a$Price)+length(xT7_a$Delta)+1:length(xT7_a$Gamma)]
  #4. Vega
  xT7_a$Vega<-set.Vega(xT=xT7_a)
  #5. Theta
  xT7_a$Theta<-set.Theta(xT=xT7_a)
  #6. Rho
  xT7_a$Rho<-set.Rho(xT=xT7_a)
  #Listに追加
  if(i==1){
    xT7StrMns<-list(xT7_a)
  } else {
    xT7StrMns<-c(xT7StrMns,list(xT7_a))
  }
}
##xT7StrPlus
for(i in 1:NumOfOnesideStrkPrice_G){
  xT7_a<-xT7
  #1. Underlying Price Change
  xT7_a$UDLY<-(xT7$UDLY+i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT7_a$OrigIV <- xT7$OrigIV+Underlying.PriceChange.volatility.skew(xT7,xT7_a)
  xT7_a$OrigIV <- xT7_a$OrigIV+vertical.volatility.skew(xT7,xT7_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT7_a)
  xT7_a$Price<-valdeltagamma_tmp[1:length(xT7_a$Price)]
  xT7_a$Delta<-valdeltagamma_tmp[length(xT7_a$Price)+1:length(xT7_a$Delta)]
  xT7_a$Gamma<-valdeltagamma_tmp[length(xT7_a$Price)+length(xT7_a$Delta)+1:length(xT7_a$Gamma)]
  #4. Vega
  xT7_a$Vega<-set.Vega(xT=xT7_a)
  #5. Theta
  xT7_a$Theta<-set.Theta(xT=xT7_a)
  #6. Rho
  xT7_a$Rho<-set.Rho(xT=xT7_a)
  #Listに追加
  if(i==1){
    xT7StrPlus<-list(xT7_a)
  } else {
    xT7StrPlus<-c(xT7StrPlus,list(xT7_a))
  }
}

## xT14
xT14<-xT0
#1. Advence date (business day) 
# 時間を進めた結果、ExpDateを超えたrowが存在する場合
tmp_<-advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),14,0) > as.Date(xT0$ExpDate,format="%Y/%m/%d")
if (sum(as.numeric(tmp_))>0){
  #break;
}
xT14$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),14,0),"%Y/%m/%d")

#2. Volatility Horizental Skew adjustment
maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT0$Date,format="%Y/%m/%d"),
                                as.Date(xT0$ExpDate,format="%Y/%m/%d"))
maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT14$Date,format="%Y/%m/%d"),
                                as.Date(xT14$ExpDate,format="%Y/%m/%d"))
#orizental.volatility.skew defined on EOptionOprEurop
#xT14$OrigIV <- xT14$OrigIV+horizental.volatility.skew(vol_b=xT14$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)

#3.Price Grek
valdeltagamma_tmp<-set.ValueDeltaGamma(xT14)
xT14$Price<-valdeltagamma_tmp[1:length(xT14$Price)]
xT14$Delta<-valdeltagamma_tmp[length(xT14$Price)+1:length(xT14$Delta)]
xT14$Gamma<-valdeltagamma_tmp[length(xT14$Price)+length(xT14$Delta)+1:length(xT14$Gamma)]
xT14$Theta<-set.Theta(xT14)
xT14$Rho<-set.Rho(xT14)

##xT14StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT14_a<-xT14
  #1. Underlying Price Change
  xT14_a$UDLY<-(xT14$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT14_a$OrigIV <- xT14$OrigIV+Underlying.PriceChange.volatility.skew(xT14,xT14_a)
  xT14_a$OrigIV <- xT14_a$OrigIV+vertical.volatility.skew(xT14,xT14_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT14_a)
  xT14_a$Price<-valdeltagamma_tmp[1:length(xT14_a$Price)]
  xT14_a$Delta<-valdeltagamma_tmp[length(xT14_a$Price)+1:length(xT14_a$Delta)]
  xT14_a$Gamma<-valdeltagamma_tmp[length(xT14_a$Price)+length(xT14_a$Delta)+1:length(xT14_a$Gamma)]
  #4. Vega
  xT14_a$Vega<-set.Vega(xT=xT14_a)
  #5. Theta
  xT14_a$Theta<-set.Theta(xT=xT14_a)
  #6. Rho
  xT14_a$Rho<-set.Rho(xT=xT14_a)
  #Listに追加
  if(i==1){
    xT14StrMns<-list(xT14_a)
  } else {
    xT14StrMns<-c(xT14StrMns,list(xT14_a))
  }
}
##xT14StrPlus
for(i in 1:NumOfOnesideStrkPrice_G){
  xT14_a<-xT14
  #1. Underlying Price Change
  xT14_a$UDLY<-(xT14$UDLY+i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT14_a$OrigIV <- xT14$OrigIV+Underlying.PriceChange.volatility.skew(xT14,xT14_a)
  xT14_a$OrigIV <- xT14_a$OrigIV+vertical.volatility.skew(xT14,xT14_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT14_a)
  xT14_a$Price<-valdeltagamma_tmp[1:length(xT14_a$Price)]
  xT14_a$Delta<-valdeltagamma_tmp[length(xT14_a$Price)+1:length(xT14_a$Delta)]
  xT14_a$Gamma<-valdeltagamma_tmp[length(xT14_a$Price)+length(xT14_a$Delta)+1:length(xT14_a$Gamma)]
  #4. Vega
  xT14_a$Vega<-set.Vega(xT=xT14_a)
  #5. Theta
  xT14_a$Theta<-set.Theta(xT=xT14_a)
  #6. Rho
  xT14_a$Rho<-set.Rho(xT=xT14_a)
  #Listに追加
  if(i==1){
    xT14StrPlus<-list(xT14_a)
  } else {
    xT14StrPlus<-c(xT14StrPlus,list(xT14_a))
  }
}

## xT21
xT21<-xT0
#1. Advence date (business day) 
# 時間を進めた結果、ExpDateを超えたrowが存在する場合
tmp_<-advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),21,0) > as.Date(xT0$ExpDate,format="%Y/%m/%d")
if (sum(as.numeric(tmp_))>0){
  #break;
}
xT21$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),21,0),"%Y/%m/%d")

#2. Volatility Horizental Skew adjustment
maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT0$Date,format="%Y/%m/%d"),
                                as.Date(xT0$ExpDate,format="%Y/%m/%d"))
maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT21$Date,format="%Y/%m/%d"),
                                as.Date(xT21$ExpDate,format="%Y/%m/%d"))
#orizental.volatility.skew defined on EOptionOprEurop
#xT21$OrigIV <- xT21$OrigIV+horizental.volatility.skew(vol_b=xT21$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)

#3.Price Grek
valdeltagamma_tmp<-set.ValueDeltaGamma(xT21)
xT21$Price<-valdeltagamma_tmp[1:length(xT21$Price)]
xT21$Delta<-valdeltagamma_tmp[length(xT21$Price)+1:length(xT21$Delta)]
xT21$Gamma<-valdeltagamma_tmp[length(xT21$Price)+length(xT21$Delta)+1:length(xT21$Gamma)]
xT21$Theta<-set.Theta(xT21)
xT21$Rho<-set.Rho(xT21)

##xT21StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT21_a<-xT21
  #1. Underlying Price Change
  xT21_a$UDLY<-(xT21$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT21_a$OrigIV <- xT21$OrigIV+Underlying.PriceChange.volatility.skew(xT21,xT21_a)
  xT21_a$OrigIV <- xT21_a$OrigIV+vertical.volatility.skew(xT21,xT21_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT21_a)
  xT21_a$Price<-valdeltagamma_tmp[1:length(xT21_a$Price)]
  xT21_a$Delta<-valdeltagamma_tmp[length(xT21_a$Price)+1:length(xT21_a$Delta)]
  xT21_a$Gamma<-valdeltagamma_tmp[length(xT21_a$Price)+length(xT21_a$Delta)+1:length(xT21_a$Gamma)]
  #4. Vega
  xT21_a$Vega<-set.Vega(xT=xT21_a)
  #5. Theta
  xT21_a$Theta<-set.Theta(xT=xT21_a)
  #6. Rho
  xT21_a$Rho<-set.Rho(xT=xT21_a)
  #Listに追加
  if(i==1){
    xT21StrMns<-list(xT21_a)
  } else {
    xT21StrMns<-c(xT21StrMns,list(xT21_a))
  }
}
##xT21StrPlus
for(i in 1:NumOfOnesideStrkPrice_G){
  xT21_a<-xT21
  #1. Underlying Price Change
  xT21_a$UDLY<-(xT21$UDLY+i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT21_a$OrigIV <- xT21$OrigIV+Underlying.PriceChange.volatility.skew(xT21,xT21_a)
  xT21_a$OrigIV <- xT21_a$OrigIV+vertical.volatility.skew(xT21,xT21_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT21_a)
  xT21_a$Price<-valdeltagamma_tmp[1:length(xT21_a$Price)]
  xT21_a$Delta<-valdeltagamma_tmp[length(xT21_a$Price)+1:length(xT21_a$Delta)]
  xT21_a$Gamma<-valdeltagamma_tmp[length(xT21_a$Price)+length(xT21_a$Delta)+1:length(xT21_a$Gamma)]
  #4. Vega
  xT21_a$Vega<-set.Vega(xT=xT21_a)
  #5. Theta
  xT21_a$Theta<-set.Theta(xT=xT21_a)
  #6. Rho
  xT21_a$Rho<-set.Rho(xT=xT21_a)
  #Listに追加
  if(i==1){
    xT21StrPlus<-list(xT21_a)
  } else {
    xT21StrPlus<-c(xT21StrPlus,list(xT21_a))
  }
}

## xT28
xT28<-xT0
#1. Advence date (business day) 
# 時間を進めた結果、ExpDateを超えたrowが存在する場合
tmp_<-advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),28,0) > as.Date(xT0$ExpDate,format="%Y/%m/%d")
if (sum(as.numeric(tmp_))>0){
  #break;
}
xT28$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),28,0),"%Y/%m/%d")

#2. Volatility Horizental Skew adjustment
maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT0$Date,format="%Y/%m/%d"),
                                as.Date(xT0$ExpDate,format="%Y/%m/%d"))
maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT28$Date,format="%Y/%m/%d"),
                                as.Date(xT28$ExpDate,format="%Y/%m/%d"))
#orizental.volatility.skew defined on EOptionOprEurop
#xT28$OrigIV <- xT28$OrigIV+horizental.volatility.skew(vol_b=xT28$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)

#3.Price Grek
valdeltagamma_tmp<-set.ValueDeltaGamma(xT28)
xT28$Price<-valdeltagamma_tmp[1:length(xT28$Price)]
xT28$Delta<-valdeltagamma_tmp[length(xT28$Price)+1:length(xT28$Delta)]
xT28$Gamma<-valdeltagamma_tmp[length(xT28$Price)+length(xT28$Delta)+1:length(xT28$Gamma)]
xT28$Theta<-set.Theta(xT28)
xT28$Rho<-set.Rho(xT28)

##xT28StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT28_a<-xT28
  #1. Underlying Price Change
  xT28_a$UDLY<-(xT28$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT28_a$OrigIV <- xT28$OrigIV+Underlying.PriceChange.volatility.skew(xT28,xT28_a)
  xT28_a$OrigIV <- xT28_a$OrigIV+vertical.volatility.skew(xT28,xT28_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT28_a)
  xT28_a$Price<-valdeltagamma_tmp[1:length(xT28_a$Price)]
  xT28_a$Delta<-valdeltagamma_tmp[length(xT28_a$Price)+1:length(xT28_a$Delta)]
  xT28_a$Gamma<-valdeltagamma_tmp[length(xT28_a$Price)+length(xT28_a$Delta)+1:length(xT28_a$Gamma)]
  #4. Vega
  xT28_a$Vega<-set.Vega(xT=xT28_a)
  #5. Theta
  xT28_a$Theta<-set.Theta(xT=xT28_a)
  #6. Rho
  xT28_a$Rho<-set.Rho(xT=xT28_a)
  #Listに追加
  if(i==1){
    xT28StrMns<-list(xT28_a)
  } else {
    xT28StrMns<-c(xT28StrMns,list(xT28_a))
  }
}
##xT28StrPlus
for(i in 1:NumOfOnesideStrkPrice_G){
  xT28_a<-xT28
  #1. Underlying Price Change
  xT28_a$UDLY<-(xT28$UDLY+i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT28_a$OrigIV <- xT28$OrigIV+Underlying.PriceChange.volatility.skew(xT28,xT28_a)
  xT28_a$OrigIV <- xT28_a$OrigIV+vertical.volatility.skew(xT28,xT28_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT28_a)
  xT28_a$Price<-valdeltagamma_tmp[1:length(xT28_a$Price)]
  xT28_a$Delta<-valdeltagamma_tmp[length(xT28_a$Price)+1:length(xT28_a$Delta)]
  xT28_a$Gamma<-valdeltagamma_tmp[length(xT28_a$Price)+length(xT28_a$Delta)+1:length(xT28_a$Gamma)]
  #4. Vega
  xT28_a$Vega<-set.Vega(xT=xT28_a)
  #5. Theta
  xT28_a$Theta<-set.Theta(xT=xT28_a)
  #6. Rho
  xT28_a$Rho<-set.Rho(xT=xT28_a)
  #Listに追加
  if(i==1){
    xT28StrPlus<-list(xT28_a)
  } else {
    xT28StrPlus<-c(xT28StrPlus,list(xT28_a))
  }
}

## xT35
xT35<-xT0
#1. Advence date (business day) 
# 時間を進めた結果、ExpDateを超えたrowが存在する場合
tmp_<-advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),35,0) > as.Date(xT0$ExpDate,format="%Y/%m/%d")
if (sum(as.numeric(tmp_))>0){
  #break;
}
xT35$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),35,0),"%Y/%m/%d")

#2. Volatility Horizental Skew adjustment
maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT0$Date,format="%Y/%m/%d"),
                                as.Date(xT0$ExpDate,format="%Y/%m/%d"))
maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                as.Date(xT35$Date,format="%Y/%m/%d"),
                                as.Date(xT35$ExpDate,format="%Y/%m/%d"))
#orizental.volatility.skew defined on EOptionOprEurop
#xT35$OrigIV <- xT35$OrigIV+horizental.volatility.skew(vol_b=xT35$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)

#3.Price Grek
valdeltagamma_tmp<-set.ValueDeltaGamma(xT35)
xT35$Price<-valdeltagamma_tmp[1:length(xT35$Price)]
xT35$Delta<-valdeltagamma_tmp[length(xT35$Price)+1:length(xT35$Delta)]
xT35$Gamma<-valdeltagamma_tmp[length(xT35$Price)+length(xT35$Delta)+1:length(xT35$Gamma)]
xT35$Theta<-set.Theta(xT35)
xT35$Rho<-set.Rho(xT35)

##xT35StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT35_a<-xT35
  #1. Underlying Price Change
  xT35_a$UDLY<-(xT35$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT35_a$OrigIV <- xT35$OrigIV+Underlying.PriceChange.volatility.skew(xT35,xT35_a)
  xT35_a$OrigIV <- xT35_a$OrigIV+vertical.volatility.skew(xT35,xT35_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT35_a)
  xT35_a$Price<-valdeltagamma_tmp[1:length(xT35_a$Price)]
  xT35_a$Delta<-valdeltagamma_tmp[length(xT35_a$Price)+1:length(xT35_a$Delta)]
  xT35_a$Gamma<-valdeltagamma_tmp[length(xT35_a$Price)+length(xT35_a$Delta)+1:length(xT35_a$Gamma)]
  #4. Vega
  xT35_a$Vega<-set.Vega(xT=xT35_a)
  #5. Theta
  xT35_a$Theta<-set.Theta(xT=xT35_a)
  #6. Rho
  xT35_a$Rho<-set.Rho(xT=xT35_a)
  #Listに追加
  if(i==1){
    xT35StrMns<-list(xT35_a)
  } else {
    xT35StrMns<-c(xT35StrMns,list(xT35_a))
  }
}
##xT35StrPlus
for(i in 1:NumOfOnesideStrkPrice_G){
  xT35_a<-xT35
  #1. Underlying Price Change
  xT35_a$UDLY<-(xT35$UDLY+i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT35_a$OrigIV <- xT35$OrigIV+Underlying.PriceChange.volatility.skew(xT35,xT35_a)
  xT35_a$OrigIV <- xT35_a$OrigIV+vertical.volatility.skew(xT35,xT35_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.ValueDeltaGamma(xT35_a)
  xT35_a$Price<-valdeltagamma_tmp[1:length(xT35_a$Price)]
  xT35_a$Delta<-valdeltagamma_tmp[length(xT35_a$Price)+1:length(xT35_a$Delta)]
  xT35_a$Gamma<-valdeltagamma_tmp[length(xT35_a$Price)+length(xT35_a$Delta)+1:length(xT35_a$Gamma)]
  #4. Vega
  xT35_a$Vega<-set.Vega(xT=xT35_a)
  #5. Theta
  xT35_a$Theta<-set.Theta(xT=xT35_a)
  #6. Rho
  xT35_a$Rho<-set.Rho(xT=xT35_a)
  #Listに追加
  if(i==1){
    xT35StrPlus<-list(xT35_a)
  } else {
    xT35StrPlus<-c(xT35StrPlus,list(xT35_a))
  }
}

##File への出力
write.table(xT0,"OptionVariablesT0.csv",row.names = FALSE,col.names=T,sep=",",append=F)
for(i in 1:NumOfOnesideStrkPrice_G){
  wfilename_<-paste("OptionVariablesT0StrMns",as.character(i),".csv",sep="")
  write.table(xT0StrMns[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
  wfilename_<-paste("OptionVariablesT0StrPlus",as.character(i),".csv",sep="")
  write.table(xT0StrPlus[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
}
write.table(xT7,"OptionVariablesT7.csv",row.names = FALSE,col.names=T,sep=",",append=F)
for(i in 1:NumOfOnesideStrkPrice_G){
  wfilename_<-paste("OptionVariablesT7StrMns",as.character(i),".csv",sep="")
  write.table(xT7StrMns[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
  wfilename_<-paste("OptionVariablesT7StrPlus",as.character(i),".csv",sep="")
  write.table(xT7StrPlus[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
}
write.table(xT14,"OptionVariablesT14.csv",row.names = FALSE,col.names=T,sep=",",append=F)
for(i in 1:NumOfOnesideStrkPrice_G){
  wfilename_<-paste("OptionVariablesT14StrMns",as.character(i),".csv",sep="")
  write.table(xT14StrMns[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
  wfilename_<-paste("OptionVariablesT14StrPlus",as.character(i),".csv",sep="")
  write.table(xT14StrPlus[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
}
write.table(xT21,"OptionVariablesT21.csv",row.names = FALSE,col.names=T,sep=",",append=F)
for(i in 1:NumOfOnesideStrkPrice_G){
  wfilename_<-paste("OptionVariablesT21StrMns",as.character(i),".csv",sep="")
  write.table(xT21StrMns[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
  wfilename_<-paste("OptionVariablesT21StrPlus",as.character(i),".csv",sep="")
  write.table(xT21StrPlus[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
}
write.table(xT28,"OptionVariablesT28.csv",row.names = FALSE,col.names=T,sep=",",append=F)
for(i in 1:NumOfOnesideStrkPrice_G){
  wfilename_<-paste("OptionVariablesT28StrMns",as.character(i),".csv",sep="")
  write.table(xT28StrMns[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
  wfilename_<-paste("OptionVariablesT28StrPlus",as.character(i),".csv",sep="")
  write.table(xT28StrPlus[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
}
write.table(xT35,"OptionVariablesT35.csv",row.names = FALSE,col.names=T,sep=",",append=F)
for(i in 1:NumOfOnesideStrkPrice_G){
  wfilename_<-paste("OptionVariablesT35StrMns",as.character(i),".csv",sep="")
  write.table(xT35StrMns[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
  wfilename_<-paste("OptionVariablesT35StrPlus",as.character(i),".csv",sep="")
  write.table(xT35StrPlus[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
}