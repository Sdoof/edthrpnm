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
#     ud_t <- x$UDLY
#     st-t <- x$Strike
#     intr_t <- (x$UDLY-x$Strike)*x$TYPE
#     if(intr_t<0) intr_t<-0
#   TIME Value
#      x$Price-intr_t

#最後に
#もとの値に置き換える
#x[1:length(cur_pos),1]<-cur_pos
# あるいは
# x$Position<-cur_pos
