## Read a txt file(csv file)
(x<-read.table("OptionVariables.csv",header=T,sep=","))
#(x<-read.csv("OptionVariables.csv"))
#x$ContactName
#x[[2]]
#str_v<-x$Strike
#str_v<-x[[7]
##現在のポジションの列を取って演算用のVector
#cur_pos<-x$Position
#cur_pos<-x[[1]]
#計算に基づきその値を変化させる
# 例えば、
#   Delta合計
#     sum(cur_pos*x$Delta)
#   Gamma合計
#     sum(cur_pos*x$Gamma)
#   Vega合計
#     sum(cur_pos*x$Vega)
#   Theta合計
#     sum(cur_pos*x$Theta)
#   Rho合計
#     sum(cur_pos*x$Rho)
#   Price合計（意味ないけど）
#     sum(cur_pos*x$Price)
#   Intrisic Value
#    TYPE:1 PUT, -1 CALL, Underlying 0
#     intr_t <- (x$UDLY-x$Strike)*x$TYPE
#     if(intr_t<0) intr_t<-0
#   TIME Value
#      x$Price-intr_t
#
#  Greekの計算
#
#
# まずはOriginal IVの設定
#
#検算用
#(x$Price[1])
#(x$UDLY[1])
#(x$Strike[1])
#date_today <-as.Date(x$Date[1],format="%Y/%m/%d")
#date_exp <-as.Date(x$ExpDate[1],format="%Y/%m/%d")
#days_diff=as.numeric(difftime(date_exp,date_today,tz="",units="days"))
## Week days のみ計算。week数*5 + 余り。
## TBD. 平日のHolidayはさらに引く。余りの日が土日挟むならその日数を引く。
#days_diff_week<-((days_diff%/%7)*5+(days_diff%%7))
#x$OrigIV[1]=AmericanOptionImpliedVolatility(type="put", value=x$Price[1],underlying=x$UDLY[1],
#                                            strike=x$Strike[1],dividendYield=0,riskFreeRate=0.01,
#                                            maturity=days_diff_week/365,volatility=0.2)
for(i in 1:length(x$TYPE)){
  ##  Week days のみ計算。本当はHolidayもそこから除く必要がある。
  date_today <-as.Date(x$Date[i],format="%Y/%m/%d")
  date_exp <-as.Date(x$ExpDate[i],format="%Y/%m/%d")
  days_diff=as.numeric(difftime(date_exp,date_today,tz="",units="days"))
  ## Week days のみ計算。week数*5 + 余り。
  ## TBD. 平日のHolidayはさらに引く。余りの日が土日挟むならその日数を引く。
  days_diff_week<-((days_diff%/%7)*5+(days_diff%%7))
  if(x$TYPE[i] == 1){
    x$OrigIV[i]=AmericanOptionImpliedVolatility(type="put", value=x$Price[i],underlying=x$UDLY[i],
                                     strike=x$Strike[i],dividendYield=0,riskFreeRate=0.01,
                                     maturity=days_diff_week/365,volatility=0.2)
  }else if(x$TYPE[i] == -1){
    x$OrigIV[i]=AmericanOptionImpliedVolatility(type="call", value=x$Price[i],underlying=x$UDLY[i],
                                                strike=x$Strike[i],dividendYield=0,riskFreeRate=0.01,
                                                maturity=days_diff_week/365,volatility=0.2)
  }
}

#最後に
#もとの値に置き換える
#x[1:length(cur_pos),1]<-cur_pos
# あるいは
# x$Position<-cur_pos
