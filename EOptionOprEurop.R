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
xT0$Gamma<-tmp_2[[3]]/365
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

#価格変化による全体的なvolatilityの変化
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
xT0_a$Gamma<-tmp_2[[3]]/365
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
  xT0_a$Gamma<-tmp_2[[3]]/365
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
xT0_a$Gamma<-tmp_2[[3]]/365
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
  xT0_a$Gamma<-tmp_2[[3]]/365
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

##File への出力
write.table(xT0,"OptionVariablesT0.csv",row.names = FALSE,col.names=T,sep=",",append=F)
for(i in 1:NumOfOnesideStrkPrice_G){
  wfilename_<-paste("OptionVariablesT0StrMns",as.character(i),".csv",sep="")
  write.table(xT0StrMns[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
  wfilename_<-paste("OptionVariablesT0StrPlus",as.character(i),".csv",sep="")
  write.table(xT0StrPlus[[i]],wfilename_,row.names = FALSE,col.names=T,sep=",",append=F)
}

