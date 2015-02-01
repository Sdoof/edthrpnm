###Start
library(RQuantLib)

#  Greekの計算と設定

set.AmericanIVOrig <- function(xT){
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

#0. Set Value Delta Gamma
set.AmericanValueDeltaGamma <- function(xT){
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

set.AmericanDeltaGamma <- function(xT){
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

set.AmericanVega <- function(xT){
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

#3. Theta
set.AmericanTheta <- function(xT){
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


#4. Rho
set.AmericanRho <- function(xT){
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

## Read a txt file(csv file)
(xT0<-read.table("OptionVariables.csv",header=T,sep=","))
#Implied Volatility
xT0$OrigIV<-set.AmericanIVOrig(xT=xT0)
#Delta Gamma
(deltagamma_tmp<-set.AmericanDeltaGamma(xT0))
xT0$Delta<-deltagamma_tmp[1:length(xT0$Delta)]
xT0$Gamma<-deltagamma_tmp[length(xT0$Delta)+1:length(xT0$Gamma)]
#Vega
xT0$Vega<-set.AmericanVega(xT=xT0)
#Theta
xT0$Theta<-set.AmericanTheta(xT=xT0)
#Rho
xT0$Rho<-set.AmericanRho(xT=xT0)

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
valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT0_a)
xT0_a$Price<-valdeltagamma_tmp[1:length(xT0_a$Price)]
xT0_a$Delta<-valdeltagamma_tmp[length(xT0_a$Price)+1:length(xT0_a$Delta)]
xT0_a$Gamma<-valdeltagamma_tmp[length(xT0_a$Price)+length(xT0$Delta)+1:length(xT0_a$Gamma)]
#4. Vega
xT0_a$Vega<-set.AmericanVega(xT=xT0_a)
#5. Theta
xT0_a$Theta<-set.AmericanTheta(xT=xT0_a)
#6. Rho
xT0_a$Rho<-set.AmericanRho(xT=xT0_a)

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
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT0_a)
  xT0_a$Price<-valdeltagamma_tmp[1:length(xT0_a$Price)]
  xT0_a$Delta<-valdeltagamma_tmp[length(xT0_a$Price)+1:length(xT0_a$Delta)]
  xT0_a$Gamma<-valdeltagamma_tmp[length(xT0_a$Price)+length(xT0$Delta)+1:length(xT0_a$Gamma)]
  #4. Vega
  xT0_a$Vega<-set.AmericanVega(xT=xT0_a)
  #5. Theta
  xT0_a$Theta<-set.AmericanTheta(xT=xT0_a)
  #6. Rho
  xT0_a$Rho<-set.AmericanRho(xT=xT0_a)
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
valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT0_a)
xT0_a$Price<-valdeltagamma_tmp[1:length(xT0_a$Price)]
xT0_a$Delta<-valdeltagamma_tmp[length(xT0_a$Price)+1:length(xT0_a$Delta)]
xT0_a$Gamma<-valdeltagamma_tmp[length(xT0_a$Price)+length(xT0$Delta)+1:length(xT0_a$Gamma)]
#4. Vega
xT0_a$Vega<-set.AmericanVega(xT=xT0_a)
#5. Theta
xT0_a$Theta<-set.AmericanTheta(xT=xT0_a)
#6. Rho
xT0_a$Rho<-set.AmericanRho(xT=xT0_a)
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
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT0_a)
  xT0_a$Price<-valdeltagamma_tmp[1:length(xT0_a$Price)]
  xT0_a$Delta<-valdeltagamma_tmp[length(xT0_a$Price)+1:length(xT0_a$Delta)]
  xT0_a$Gamma<-valdeltagamma_tmp[length(xT0_a$Price)+length(xT0$Delta)+1:length(xT0_a$Gamma)]
  #4. Vega
  xT0_a$Vega<-set.AmericanVega(xT=xT0_a)
  #5. Theta
  xT0_a$Theta<-set.AmericanTheta(xT=xT0_a)
  #6. Rho
  xT0_a$Rho<-set.AmericanRho(xT=xT0_a)
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
valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT7)
xT7$Price<-valdeltagamma_tmp[1:length(xT7$Price)]
xT7$Delta<-valdeltagamma_tmp[length(xT7$Price)+1:length(xT7$Delta)]
xT7$Gamma<-valdeltagamma_tmp[length(xT7$Price)+length(xT7$Delta)+1:length(xT7$Gamma)]
xT7$Theta<-set.AmericanTheta(xT7)
xT7$Rho<-set.AmericanRho(xT7)

##xT7StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT7_a<-xT7
  #1. Underlying Price Change
  xT7_a$UDLY<-(xT7$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT7_a$OrigIV <- xT7$OrigIV+Underlying.PriceChange.volatility.skew(xT7,xT7_a)
  xT7_a$OrigIV <- xT7_a$OrigIV+vertical.volatility.skew(xT7,xT7_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT7_a)
  xT7_a$Price<-valdeltagamma_tmp[1:length(xT7_a$Price)]
  xT7_a$Delta<-valdeltagamma_tmp[length(xT7_a$Price)+1:length(xT7_a$Delta)]
  xT7_a$Gamma<-valdeltagamma_tmp[length(xT7_a$Price)+length(xT7_a$Delta)+1:length(xT7_a$Gamma)]
  #4. Vega
  xT7_a$Vega<-set.AmericanVega(xT=xT7_a)
  #5. Theta
  xT7_a$Theta<-set.AmericanTheta(xT=xT7_a)
  #6. Rho
  xT7_a$Rho<-set.AmericanRho(xT=xT7_a)
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
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT7_a)
  xT7_a$Price<-valdeltagamma_tmp[1:length(xT7_a$Price)]
  xT7_a$Delta<-valdeltagamma_tmp[length(xT7_a$Price)+1:length(xT7_a$Delta)]
  xT7_a$Gamma<-valdeltagamma_tmp[length(xT7_a$Price)+length(xT7_a$Delta)+1:length(xT7_a$Gamma)]
  #4. Vega
  xT7_a$Vega<-set.AmericanVega(xT=xT7_a)
  #5. Theta
  xT7_a$Theta<-set.AmericanTheta(xT=xT7_a)
  #6. Rho
  xT7_a$Rho<-set.AmericanRho(xT=xT7_a)
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
valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT14)
xT14$Price<-valdeltagamma_tmp[1:length(xT14$Price)]
xT14$Delta<-valdeltagamma_tmp[length(xT14$Price)+1:length(xT14$Delta)]
xT14$Gamma<-valdeltagamma_tmp[length(xT14$Price)+length(xT14$Delta)+1:length(xT14$Gamma)]
xT14$Theta<-set.AmericanTheta(xT14)
xT14$Rho<-set.AmericanRho(xT14)

##xT14StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT14_a<-xT14
  #1. Underlying Price Change
  xT14_a$UDLY<-(xT14$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT14_a$OrigIV <- xT14$OrigIV+Underlying.PriceChange.volatility.skew(xT14,xT14_a)
  xT14_a$OrigIV <- xT14_a$OrigIV+vertical.volatility.skew(xT14,xT14_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT14_a)
  xT14_a$Price<-valdeltagamma_tmp[1:length(xT14_a$Price)]
  xT14_a$Delta<-valdeltagamma_tmp[length(xT14_a$Price)+1:length(xT14_a$Delta)]
  xT14_a$Gamma<-valdeltagamma_tmp[length(xT14_a$Price)+length(xT14_a$Delta)+1:length(xT14_a$Gamma)]
  #4. Vega
  xT14_a$Vega<-set.AmericanVega(xT=xT14_a)
  #5. Theta
  xT14_a$Theta<-set.AmericanTheta(xT=xT14_a)
  #6. Rho
  xT14_a$Rho<-set.AmericanRho(xT=xT14_a)
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
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT14_a)
  xT14_a$Price<-valdeltagamma_tmp[1:length(xT14_a$Price)]
  xT14_a$Delta<-valdeltagamma_tmp[length(xT14_a$Price)+1:length(xT14_a$Delta)]
  xT14_a$Gamma<-valdeltagamma_tmp[length(xT14_a$Price)+length(xT14_a$Delta)+1:length(xT14_a$Gamma)]
  #4. Vega
  xT14_a$Vega<-set.AmericanVega(xT=xT14_a)
  #5. Theta
  xT14_a$Theta<-set.AmericanTheta(xT=xT14_a)
  #6. Rho
  xT14_a$Rho<-set.AmericanRho(xT=xT14_a)
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
valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT21)
xT21$Price<-valdeltagamma_tmp[1:length(xT21$Price)]
xT21$Delta<-valdeltagamma_tmp[length(xT21$Price)+1:length(xT21$Delta)]
xT21$Gamma<-valdeltagamma_tmp[length(xT21$Price)+length(xT21$Delta)+1:length(xT21$Gamma)]
xT21$Theta<-set.AmericanTheta(xT21)
xT21$Rho<-set.AmericanRho(xT21)

##xT21StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT21_a<-xT21
  #1. Underlying Price Change
  xT21_a$UDLY<-(xT21$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT21_a$OrigIV <- xT21$OrigIV+Underlying.PriceChange.volatility.skew(xT21,xT21_a)
  xT21_a$OrigIV <- xT21_a$OrigIV+vertical.volatility.skew(xT21,xT21_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT21_a)
  xT21_a$Price<-valdeltagamma_tmp[1:length(xT21_a$Price)]
  xT21_a$Delta<-valdeltagamma_tmp[length(xT21_a$Price)+1:length(xT21_a$Delta)]
  xT21_a$Gamma<-valdeltagamma_tmp[length(xT21_a$Price)+length(xT21_a$Delta)+1:length(xT21_a$Gamma)]
  #4. Vega
  xT21_a$Vega<-set.AmericanVega(xT=xT21_a)
  #5. Theta
  xT21_a$Theta<-set.AmericanTheta(xT=xT21_a)
  #6. Rho
  xT21_a$Rho<-set.AmericanRho(xT=xT21_a)
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
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT21_a)
  xT21_a$Price<-valdeltagamma_tmp[1:length(xT21_a$Price)]
  xT21_a$Delta<-valdeltagamma_tmp[length(xT21_a$Price)+1:length(xT21_a$Delta)]
  xT21_a$Gamma<-valdeltagamma_tmp[length(xT21_a$Price)+length(xT21_a$Delta)+1:length(xT21_a$Gamma)]
  #4. Vega
  xT21_a$Vega<-set.AmericanVega(xT=xT21_a)
  #5. Theta
  xT21_a$Theta<-set.AmericanTheta(xT=xT21_a)
  #6. Rho
  xT21_a$Rho<-set.AmericanRho(xT=xT21_a)
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
valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT28)
xT28$Price<-valdeltagamma_tmp[1:length(xT28$Price)]
xT28$Delta<-valdeltagamma_tmp[length(xT28$Price)+1:length(xT28$Delta)]
xT28$Gamma<-valdeltagamma_tmp[length(xT28$Price)+length(xT28$Delta)+1:length(xT28$Gamma)]
xT28$Theta<-set.AmericanTheta(xT28)
xT28$Rho<-set.AmericanRho(xT28)

##xT28StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT28_a<-xT28
  #1. Underlying Price Change
  xT28_a$UDLY<-(xT28$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT28_a$OrigIV <- xT28$OrigIV+Underlying.PriceChange.volatility.skew(xT28,xT28_a)
  xT28_a$OrigIV <- xT28_a$OrigIV+vertical.volatility.skew(xT28,xT28_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT28_a)
  xT28_a$Price<-valdeltagamma_tmp[1:length(xT28_a$Price)]
  xT28_a$Delta<-valdeltagamma_tmp[length(xT28_a$Price)+1:length(xT28_a$Delta)]
  xT28_a$Gamma<-valdeltagamma_tmp[length(xT28_a$Price)+length(xT28_a$Delta)+1:length(xT28_a$Gamma)]
  #4. Vega
  xT28_a$Vega<-set.AmericanVega(xT=xT28_a)
  #5. Theta
  xT28_a$Theta<-set.AmericanTheta(xT=xT28_a)
  #6. Rho
  xT28_a$Rho<-set.AmericanRho(xT=xT28_a)
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
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT28_a)
  xT28_a$Price<-valdeltagamma_tmp[1:length(xT28_a$Price)]
  xT28_a$Delta<-valdeltagamma_tmp[length(xT28_a$Price)+1:length(xT28_a$Delta)]
  xT28_a$Gamma<-valdeltagamma_tmp[length(xT28_a$Price)+length(xT28_a$Delta)+1:length(xT28_a$Gamma)]
  #4. Vega
  xT28_a$Vega<-set.AmericanVega(xT=xT28_a)
  #5. Theta
  xT28_a$Theta<-set.AmericanTheta(xT=xT28_a)
  #6. Rho
  xT28_a$Rho<-set.AmericanRho(xT=xT28_a)
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
valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT35)
xT35$Price<-valdeltagamma_tmp[1:length(xT35$Price)]
xT35$Delta<-valdeltagamma_tmp[length(xT35$Price)+1:length(xT35$Delta)]
xT35$Gamma<-valdeltagamma_tmp[length(xT35$Price)+length(xT35$Delta)+1:length(xT35$Gamma)]
xT35$Theta<-set.AmericanTheta(xT35)
xT35$Rho<-set.AmericanRho(xT35)

##xT35StrMns
for(i in 1:NumOfOnesideStrkPrice_G){
  xT35_a<-xT35
  #1. Underlying Price Change
  xT35_a$UDLY<-(xT35$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT35_a$OrigIV <- xT35$OrigIV+Underlying.PriceChange.volatility.skew(xT35,xT35_a)
  xT35_a$OrigIV <- xT35_a$OrigIV+vertical.volatility.skew(xT35,xT35_a)
  #3.Price Delta Gamma
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT35_a)
  xT35_a$Price<-valdeltagamma_tmp[1:length(xT35_a$Price)]
  xT35_a$Delta<-valdeltagamma_tmp[length(xT35_a$Price)+1:length(xT35_a$Delta)]
  xT35_a$Gamma<-valdeltagamma_tmp[length(xT35_a$Price)+length(xT35_a$Delta)+1:length(xT35_a$Gamma)]
  #4. Vega
  xT35_a$Vega<-set.AmericanVega(xT=xT35_a)
  #5. Theta
  xT35_a$Theta<-set.AmericanTheta(xT=xT35_a)
  #6. Rho
  xT35_a$Rho<-set.AmericanRho(xT=xT35_a)
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
  valdeltagamma_tmp<-set.AmericanValueDeltaGamma(xT35_a)
  xT35_a$Price<-valdeltagamma_tmp[1:length(xT35_a$Price)]
  xT35_a$Delta<-valdeltagamma_tmp[length(xT35_a$Price)+1:length(xT35_a$Delta)]
  xT35_a$Gamma<-valdeltagamma_tmp[length(xT35_a$Price)+length(xT35_a$Delta)+1:length(xT35_a$Gamma)]
  #4. Vega
  xT35_a$Vega<-set.AmericanVega(xT=xT35_a)
  #5. Theta
  xT35_a$Theta<-set.AmericanTheta(xT=xT35_a)
  #6. Rho
  xT35_a$Rho<-set.AmericanRho(xT=xT35_a)
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