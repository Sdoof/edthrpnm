###Start
library(RQuantLib)

## Read a txt file(csv file)
(xT0<-read.table("OptionVariables.csv",header=T,sep=","))
## Tx later data stored.
#xT7
#xT14
#xT21
#xT28
#xT35

###Global 変数及び定数.
# Possibly read from File
riskFreeRate_G=0.01
divYld_G=0.0

##xT0の列のアクセス
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

## xT0StrMns[],xT0StrPlus[]
## Further xT7,xT14,xT21,xT28,xT35

##File への出力
write.table(xT0,"OptionVariablesT0.csv",col.names=T,sep=",",append=F)
