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

set.IVOrig <- function(xT){
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
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

xT0$OrigIV<-set.IVOrig(xT=xT0)

# Set Value Greeks
set.ValueGreeks <- function(xT){
  value<-rep(0,length(xT$TYPE))
  delta<-rep(0,length(xT$TYPE))
  gamma<-rep(0,length(xT$TYPE))
  vega<-rep(0,length(xT$TYPE))
  theta<-rep(0,length(xT$TYPE))
  rho<-rep(0,length(xT$TYPE))
  for(i in 1:length(xT$TYPE)){
    ##  Business days
    busdays_betwn<-businessDaysBetween("UnitedStates/NYSE",
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

tmp_2<-(set.ValueGreeks(xT0))
xT0$Delta<-tmp_2[[2]]
xT0$Gamma<-tmp_2[[3]]
xT0$Vega<-tmp_2[[4]]/100
xT0$Theta<-tmp_2[[5]]/365
xT0$Rho<-tmp_2[[6]]/100

## xT0StrMns[[]],xT0StrPlus[[]] as list

##Test 検算用
xT0_t<-xT0
##1. Underlying Price Change
xT0_t$UDLY<-(xT0$UDLY-30)

oom_mgn_before <- xT0$TYPE*(xT0$UDLY-xT0$Strike)
oom_mgn_after <- xT0_t$TYPE*(xT0_t$UDLY-xT0_t$Strike)

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

#Horizental Volatility Skiew
#To be defined
horizental.volatility.skew <- function(vol_b,maturity_b,maturity_a){
  diff_<-maturity_a-maturity_b
  vol_dif_rate<-rep(0,length(vol_b))
  #vol_dif_rate f(vol_b,diff_)
  vol_dif_rate
}

#To be defined
Underlying.PriceChange.volatility.skew <- function(xTb,xTa){
  vol_dif_rate<-rep(0,length(xTb$TYPE))
  vol_dif_rate
}

#NumOfOnesideStrkPrice_G_TMP=4
#ChangStrkPrUnit_G_TMP=10


##ListとしてxT0StrMnsを管理
#1つ目.Listとし宣言。
xT0_a<-xT0
#1. Underlying Price Change
xT0_a$UDLY<-(xT0$UDLY-1*ChangStrkPrUnit_G)
#2. Volatility Skew Change
xT0_a$OrigIV <- xT0$OrigIV+Underlying.PriceChange.volatility.skew(xT0,xT0_a)
xT0_a$OrigIV <- xT0_a$OrigIV+vertical.volatility.skew(xT0,xT0_a)
#3.Price Greeks
tmp_2<-(set.ValueGreeks(xT0_a))
xT0_a$Price<-tmp_2[[1]]
xT0_a$Delta<-tmp_2[[2]]
xT0_a$Gamma<-tmp_2[[3]]
xT0_a$Vega<-tmp_2[[4]]/100
xT0_a$Theta<-tmp_2[[5]]/365
xT0_a$Rho<-tmp_2[[6]]/100

#Listに追加
xT0StrMns<-list(xT0_a)
for(i in 2:NumOfOnesideStrkPrice_G){
  xT0_a<-xT0
  #1. Underlying Price Change
  xT0_a$UDLY<-(xT0$UDLY-i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT0_a$OrigIV <- xT0$OrigIV+Underlying.PriceChange.volatility.skew(xT0,xT0_a)
  xT0_a$OrigIV <- xT0_a$OrigIV+vertical.volatility.skew(xT0,xT0_a)
  #3.Price Greeks
  tmp_2<-(set.ValueGreeks(xT0_a))
  xT0_a$Price<-tmp_2[[1]]
  xT0_a$Delta<-tmp_2[[2]]
  xT0_a$Gamma<-tmp_2[[3]]
  xT0_a$Vega<-tmp_2[[4]]/100
  xT0_a$Theta<-tmp_2[[5]]/365
  xT0_a$Rho<-tmp_2[[6]]/100
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
#3.Price Greeks
tmp_2<-(set.ValueGreeks(xT0_a))
xT0_a$Price<-tmp_2[[1]]
xT0_a$Delta<-tmp_2[[2]]
xT0_a$Gamma<-tmp_2[[3]]
xT0_a$Vega<-tmp_2[[4]]/100
xT0_a$Theta<-tmp_2[[5]]/365
xT0_a$Rho<-tmp_2[[6]]/100
#Listに追加
xT0StrPlus<-list(xT0_a)
for(i in 2:NumOfOnesideStrkPrice_G){
  xT0_a<-xT0
  #1. Underlying Price Change
  xT0_a$UDLY<-(xT0$UDLY+i*ChangStrkPrUnit_G)
  #2. Volatility Change
  xT0_a$OrigIV <- xT0$OrigIV+Underlying.PriceChange.volatility.skew(xT0,xT0_a)
  xT0_a$OrigIV <- xT0_a$OrigIV+vertical.volatility.skew(xT0,xT0_a)
  #3.Price Greeks
  tmp_2<-(set.ValueGreeks(xT0_a))
  xT0_a$Price<-tmp_2[[1]]
  xT0_a$Delta<-tmp_2[[2]]
  xT0_a$Gamma<-tmp_2[[3]]
  xT0_a$Vega<-tmp_2[[4]]/100
  xT0_a$Theta<-tmp_2[[5]]/365
  xT0_a$Rho<-tmp_2[[6]]/100
  #Listに追加
  xT0StrPlus<-c(xT0StrPlus,list(xT0_a))
}
##EOF xT0

##
## xT7,xT14,xT21,xT28,xT35
##

##xT7
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
xT7$OrigIV <- xT7$OrigIV+horizental.volatility.skew(vol_b=xT7$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
#3. Price Greeks
tmp_<-(set.ValueGreeks(xT7))
xT7$Price<-tmp_[[1]]
xT7$Delta<-tmp_[[2]]
xT7$Gamma<-tmp_[[3]]
xT7$Vega<-tmp_[[4]]/100
xT7$Theta<-tmp_[[5]]/365
xT7$Rho<-tmp_[[6]]/100

##xT7StrMns[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT7_a<-xT0StrMns[[i]]
  #1. Advence date (business day)
  xT7_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),7,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT7_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT7_a$ExpDate,format="%Y/%m/%d"))
  xT7_a$OrigIV <- xT7_a$OrigIV+horizental.volatility.skew(vol_b=xT7_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT7_a))
  xT7_a$Price<-tmp_[[1]]
  xT7_a$Delta<-tmp_[[2]]
  xT7_a$Gamma<-tmp_[[3]]
  xT7_a$Vega<-tmp_[[4]]/100
  xT7_a$Theta<-tmp_[[5]]/365
  xT7_a$Rho<-tmp_[[6]]/100
  #Listに追化
  if(i==1){
    xT7StrMns<-list(xT7_a)
  } else {
    xT7StrMns<-c(xT7StrMns,list(xT7_a))
  }
}

##xT7StrPlus[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT7_a<-xT0StrPlus[[i]]
  #1. Advence date (business day)
  xT7_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),7,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT7_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT7_a$ExpDate,format="%Y/%m/%d"))
  xT7_a$OrigIV <- xT7_a$OrigIV+horizental.volatility.skew(vol_b=xT7_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT7_a))
  xT7_a$Price<-tmp_[[1]]
  xT7_a$Delta<-tmp_[[2]]
  xT7_a$Gamma<-tmp_[[3]]
  xT7_a$Vega<-tmp_[[4]]/100
  xT7_a$Theta<-tmp_[[5]]/365
  xT7_a$Rho<-tmp_[[6]]/100
  #Listに追化
  if(i==1){
    xT7StrPlus<-list(xT7_a)
  } else {
    xT7StrPlus<-c(xT7StrPlus,list(xT7_a))
  }
}

##xT14
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
xT14$OrigIV <- xT14$OrigIV+horizental.volatility.skew(vol_b=xT14$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
#3. Price Greeks
tmp_<-(set.ValueGreeks(xT14))
xT14$Price<-tmp_[[1]]
xT14$Delta<-tmp_[[2]]
xT14$Gamma<-tmp_[[3]]
xT14$Vega<-tmp_[[4]]/100
xT14$Theta<-tmp_[[5]]/365
xT14$Rho<-tmp_[[6]]/100

##xT14StrMns[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT14_a<-xT0StrMns[[i]]
  #1. Advence date (business day)
  xT14_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),14,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT14_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT14_a$ExpDate,format="%Y/%m/%d"))
  xT14_a$OrigIV <- xT14_a$OrigIV+horizental.volatility.skew(vol_b=xT14_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT14_a))
  xT14_a$Price<-tmp_[[1]]
  xT14_a$Delta<-tmp_[[2]]
  xT14_a$Gamma<-tmp_[[3]]
  xT14_a$Vega<-tmp_[[4]]/100
  xT14_a$Theta<-tmp_[[5]]/365
  xT14_a$Rho<-tmp_[[6]]/100
  #Listに追化
  if(i==1){
    xT14StrMns<-list(xT14_a)
  } else {
    xT14StrMns<-c(xT14StrMns,list(xT14_a))
  }
}

##xT14StrPlus[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT14_a<-xT0StrPlus[[i]]
  #1. Advence date (business day)
  xT14_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),14,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT14_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT14_a$ExpDate,format="%Y/%m/%d"))
  xT14_a$OrigIV <- xT14_a$OrigIV+horizental.volatility.skew(vol_b=xT14_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT14_a))
  xT14_a$Price<-tmp_[[1]]
  xT14_a$Delta<-tmp_[[2]]
  xT14_a$Gamma<-tmp_[[3]]
  xT14_a$Vega<-tmp_[[4]]/100
  xT14_a$Theta<-tmp_[[5]]/365
  xT14_a$Rho<-tmp_[[6]]/100
  #Listに追化
  if(i==1){
    xT14StrPlus<-list(xT14_a)
  } else {
    xT14StrPlus<-c(xT14StrPlus,list(xT14_a))
  }
}

##xT21
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
xT21$OrigIV <- xT21$OrigIV+horizental.volatility.skew(vol_b=xT21$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
#3. Price Greeks
tmp_<-(set.ValueGreeks(xT21))
xT21$Price<-tmp_[[1]]
xT21$Delta<-tmp_[[2]]
xT21$Gamma<-tmp_[[3]]
xT21$Vega<-tmp_[[4]]/100
xT21$Theta<-tmp_[[5]]/365
xT21$Rho<-tmp_[[6]]/100

##xT21StrMns[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT21_a<-xT0StrMns[[i]]
  #1. Advence date (business day)
  xT21_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),21,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT21_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT21_a$ExpDate,format="%Y/%m/%d"))
  xT21_a$OrigIV <- xT21_a$OrigIV+horizental.volatility.skew(vol_b=xT21_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT21_a))
  xT21_a$Price<-tmp_[[1]]
  xT21_a$Delta<-tmp_[[2]]
  xT21_a$Gamma<-tmp_[[3]]
  xT21_a$Vega<-tmp_[[4]]/100
  xT21_a$Theta<-tmp_[[5]]/365
  xT21_a$Rho<-tmp_[[6]]/100
  #Listに追化
  if(i==1){
    xT21StrMns<-list(xT21_a)
  } else {
    xT21StrMns<-c(xT21StrMns,list(xT21_a))
  }
}

##xT21StrPlus[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT21_a<-xT0StrPlus[[i]]
  #1. Advence date (business day)
  xT21_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),21,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT21_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT21_a$ExpDate,format="%Y/%m/%d"))
  xT21_a$OrigIV <- xT21_a$OrigIV+horizental.volatility.skew(vol_b=xT21_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT21_a))
  xT21_a$Price<-tmp_[[1]]
  xT21_a$Delta<-tmp_[[2]]
  xT21_a$Gamma<-tmp_[[3]]
  xT21_a$Vega<-tmp_[[4]]/100
  xT21_a$Theta<-tmp_[[5]]/365
  xT21_a$Rho<-tmp_[[6]]/100
  #Listに追化
  if(i==1){
    xT21StrPlus<-list(xT21_a)
  } else {
    xT21StrPlus<-c(xT21StrPlus,list(xT21_a))
  }
}

##xT28
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
xT28$OrigIV <- xT28$OrigIV+horizental.volatility.skew(vol_b=xT28$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
#3. Price Greeks
tmp_<-(set.ValueGreeks(xT28))
xT28$Price<-tmp_[[1]]
xT28$Delta<-tmp_[[2]]
xT28$Gamma<-tmp_[[3]]
xT28$Vega<-tmp_[[4]]/100
xT28$Theta<-tmp_[[5]]/365
xT28$Rho<-tmp_[[6]]/100

##xT28StrMns[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT28_a<-xT0StrMns[[i]]
  #1. Advence date (business day)
  xT28_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),28,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT28_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT28_a$ExpDate,format="%Y/%m/%d"))
  xT28_a$OrigIV <- xT28_a$OrigIV+horizental.volatility.skew(vol_b=xT28_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT28_a))
  xT28_a$Price<-tmp_[[1]]
  xT28_a$Delta<-tmp_[[2]]
  xT28_a$Gamma<-tmp_[[3]]
  xT28_a$Vega<-tmp_[[4]]/100
  xT28_a$Theta<-tmp_[[5]]/365
  xT28_a$Rho<-tmp_[[6]]/100
  #Listに追化
  if(i==1){
    xT28StrMns<-list(xT28_a)
  } else {
    xT28StrMns<-c(xT28StrMns,list(xT28_a))
  }
}

##xT28StrPlus[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT28_a<-xT0StrPlus[[i]]
  #1. Advence date (business day)
  xT28_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),28,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT28_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT28_a$ExpDate,format="%Y/%m/%d"))
  xT28_a$OrigIV <- xT28_a$OrigIV+horizental.volatility.skew(vol_b=xT28_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT28_a))
  xT28_a$Price<-tmp_[[1]]
  xT28_a$Delta<-tmp_[[2]]
  xT28_a$Gamma<-tmp_[[3]]
  xT28_a$Vega<-tmp_[[4]]/100
  xT28_a$Theta<-tmp_[[5]]/365
  xT28_a$Rho<-tmp_[[6]]/100
  #Listに追化
  if(i==1){
    xT28StrPlus<-list(xT28_a)
  } else {
    xT28StrPlus<-c(xT28StrPlus,list(xT28_a))
  }
}

##xT35
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
xT35$OrigIV <- xT35$OrigIV+horizental.volatility.skew(vol_b=xT35$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
#3. Price Greeks
tmp_<-(set.ValueGreeks(xT35))
xT35$Price<-tmp_[[1]]
xT35$Delta<-tmp_[[2]]
xT35$Gamma<-tmp_[[3]]
xT35$Vega<-tmp_[[4]]/100
xT35$Theta<-tmp_[[5]]/365
xT35$Rho<-tmp_[[6]]/100

##xT35StrMns[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT35_a<-xT0StrMns[[i]]
  #1. Advence date (business day)
  xT35_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),35,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT35_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT35_a$ExpDate,format="%Y/%m/%d"))
  xT35_a$OrigIV <- xT35_a$OrigIV+horizental.volatility.skew(vol_b=xT35_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT35_a))
  xT35_a$Price<-tmp_[[1]]
  xT35_a$Delta<-tmp_[[2]]
  xT35_a$Gamma<-tmp_[[3]]
  xT35_a$Vega<-tmp_[[4]]/100
  xT35_a$Theta<-tmp_[[5]]/365
  xT35_a$Rho<-tmp_[[6]]/100
  #Listに追化
  if(i==1){
    xT35StrMns<-list(xT35_a)
  } else {
    xT35StrMns<-c(xT35StrMns,list(xT35_a))
  }
}

##xT35StrPlus[]
for(i in 1:NumOfOnesideStrkPrice_G){
  xT35_a<-xT0StrPlus[[i]]
  #1. Advence date (business day)
  xT35_a$Date<-format(advance("UnitedStates/NYSE",dates=as.Date(xT0$Date,format="%Y/%m/%d"),35,0),"%Y/%m/%d")
  #2. Volatility Horizental Skew adjustment
  maturity_b<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT0$Date,format="%Y/%m/%d"),
                                  as.Date(xT0$ExpDate,format="%Y/%m/%d"))
  maturity_a<-businessDaysBetween("UnitedStates/NYSE",
                                  as.Date(xT35_a$Date,format="%Y/%m/%d"),
                                  as.Date(xT35_a$ExpDate,format="%Y/%m/%d"))
  xT35_a$OrigIV <- xT35_a$OrigIV+horizental.volatility.skew(vol_b=xT35_a$OrigIV,maturity_b=maturity_b,maturity_a=maturity_a)
  #3. Price Greeks
  tmp_<-(set.ValueGreeks(xT35_a))
  xT35_a$Price<-tmp_[[1]]
  xT35_a$Delta<-tmp_[[2]]
  xT35_a$Gamma<-tmp_[[3]]
  xT35_a$Vega<-tmp_[[4]]/100
  xT35_a$Theta<-tmp_[[5]]/365
  xT35_a$Rho<-tmp_[[6]]/100
  #Listに追化
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


