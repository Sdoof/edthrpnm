
#getting annual and daili volatility
annuual.daily.volatility <- function(p){
  p_ <- replace(p, rep(1:(length(p)-1)), p[2:length(p)])
  #sum(log(p_/p))
  #sum(log(p_/p)^2)
  #sum(log(p_/p)^2)/(length(p)-1-1)
  #sum(log(p_/p))^2/(length(p)-1)/(length(p)-1-1)
  daily_<-sqrt(sum(log(p_/p)^2)/(length(p)-1-1)-sum(log(p_/p))^2/(length(p)-1)/(length(p)-1-1))
  vol_<-daily_*sqrt(252)
  ret_<-list(vol_)
  ret_<-c(ret_,list(daily_))
  ret_
}
#p<-seq(1, 10, length=10)
p<-c(20,20.1,19.9,20,20.5,20.25,20.9,20.9,20.9,20.75,20.75,21,21.1,20.9,20.9,21.25,21.4,21.4,21.25,21.75,22)
pv_<-annuual.daily.volatility(p)



