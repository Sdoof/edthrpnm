
#rnorm2,rnorm2condのテスト
n = 200; mx = 10; sx = 3; my = 20; sy = 5; r = -0.8
z2 = sapply(1:n, function(x) rnorm2(mx, sx, my, sy, r))
plot(z2[1,],z2[2,]) # これで100組の乱数の散布図が描かれる
cor(z2[1,],z2[2,])  # 標本相関係数の計算
z3 = sapply(1:n, function(x) rnorm2cond(mx, sx, my, sy, r,20))
plot(z3[1,],z3[2,]) # これで100組の乱数の散布図が描かれる


#day by day price stimulation
s0<-906
#mu is daily return
#(1.1)^(1/252)-1
#  1.1(10%) annual return
#  ^(1/252) daily return (mu subracted by 1) 
mu<-(1.1)^(1/252)-1
#sigma is daily volatility
sigma<-0.2/sqrt(252)
(geombrmtn.stimulate(s0=s0,mu=mu,sigma=sigma,length=252))

## example from web not vectorized
maturity <- 15
simulation.length <- 10001
dt <-  maturity/(simulation.length-1)

timeline <- seq(0,maturity, dt)

S0<-1
r<-0.05
mu<-0.1
mu0<-0.2
sigma<-0.2
sigma0<-0.375

f <- g <- g0 <- h <- h0 <- rep(0, times=simulation.length)
g0[1] <- h0[1] <- g[1] <-  h[1] <- S0

for(i in 2:simulation.length){
  f[i] <- f[i-1]+sqrt(dt)*rnorm(1)
  g[i] <- g[1]*exp((mu-(sigma^2)/2)*(i-1)*dt+sigma*f[i])
  g0[i] <- g0[1]*exp(mu*(i-1)*dt)
  h[i] <- h[1]*exp((mu0-sigma0^2/2)*(i-1)*dt+sigma0*f[i])
  h0[i] <- h0[1]*exp(mu0*(i-1)*dt)
}

o_range <- range(f,g,g0,h,h0)

plot(timeline,f, ylim=o_range, type="l", col="coral1")
lines(timeline,g0, col="chartreuse3")
lines(timeline,g, col="chartreuse2")
lines(timeline,h, col="deepskyblue1")
lines(timeline,h0, col="deepskyblue3")

title(main="Geometric Brownian Motion trajectories", col.main="red", font.main=4)

legend(1, o_range[2], c("mu = 0.2,  sigma = 0.375","mu = 0.1,  sigma = 0.2","Brownian motion"), cex=0.8, 
       col=c("deepskyblue1","chartreuse2","coral1"), pch=1, lty=1);

#another example from web not vectorized

m = 0.1; s = 0.2; dt = 15/9999; n = 10000
x<-rep(0,1)
x[1] = 10;
for(t in 2:n) {
  x[t] = x[t-1] * (1 + m * dt + s * sqrt(dt) * rnorm(1))
}
plot(x, type="l")

# AUD USD Volatility examples --------------------

#simple sample
p<-c(20,20.1,19.9,20,20.5,20.25,20.9,20.9,20.9,20.75,20.75,21,21.1,20.9,20.9,21.25,21.4,21.4,21.25,21.75,22)
pv_<-annuual.daily.volatility(p)

#AUD uSD
p<-read.table("AUDUSD.csv",header=T,sep=",")
p<-p$AUDUSD[1:100]
pv_<-annuual.daily.volatility(p)

rm(p,pv_)

##
# functions to be loaded --------------

# Volatility Level Analysis and Regression
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
  names(ret_)<-c("anlzd","daily")
  ret_
}

#geometric brown motion
#dSt = μ*St+σ*St*dBt
#dSt は 増分。　例：金融商品の価格の変化。
#dBt は ブラウン運動の増分。
#μ(mu)は(現在の St に対する割合であらわした)平均
#σ(sigma)は(現在の St に対する割合であらわした)ボラティリティ
#s0: 初期値
#length: シミュレーションの長さ。単位時間はvolatility(σ:sigma)
#        と一貫性を保つこと
# Should be Vectorized
geombrmtn.stimulate <- function(s0,mu,sigma,length){
  f <- g <- h<- rep(0, times=(length+1))
  dt<-1
  g[1]<-s0
  for(i in 2:(length+1)){
    f[i] <- f[i-1]+sqrt(dt)*rnorm(1)
    g[i] <- g[1]*exp((mu-(sigma^2)/2)*(i-1)*dt+sigma*f[i])
  }
  g
}

#2変量正規分布の単純な関数
#(x,y) mx,my: x,yの平均。 sx,sy: x,y:SQRT(共分散)。 r:xとyの相関
#オプションのポジションをシミュレーションする時に用いる
#価格の変化をGBM（のバリエーション）で発生させた後、IVの変化を
#回帰で決定的にではなく相関のある乱数として生成する
rnorm2 <- function(mx, sx, my, sy, r) {
  y = rnorm(1, my, sy)
  x = rnorm(1, mx + r*sx/sy * (y - my), sx*sqrt(1-r^2))
  return(c(x,y))
}

#上記と同じ2変量正規分布の単純な関数。ただし、y=y0という条件付き
rnorm2cond <- function(mx, sx, my, sy, r,y0) {
  y = y0
  x = rnorm(1, mx + r*sx/sy * (y - my), sx*sqrt(1-r^2))
  return(c(x,y))
}

#1. Vectorized Geometric Brownian Motion Code
GBM = function(N,sigma,u,S0){
  Wt = cumsum(rnorm(N,0,1));
  dt<-1 #dt<-255, dt<-365
  t = (1:N)/dt;
  p1 = (u-0.5*(sigma^2))*t;
  p2 = sigma*Wt;
  St = S0*exp(p1 + p2);
  return (St);
}
# Geometric Brownian Motion Code with multiply plots
#P = 5
#GBMs = matrix(nrow = P, ncol = N)
#for(i in 1:P){
#  GBMs[i,] = GBM(N,sigma,u,S0);
#}
#plot(t,GBMs[1,],type= "l", xlim = c(0,1000), ylim=c(100,200),
#   xlab = "Time Interval",ylab="Predicted Value")
#for(i in 2:5){
#  lines(t,GBMs[i,])
#}

#3. Geometric Brownian Motion Ditribution confdence Interval
#GBMP = function(N,sigma,u,S0){
#  Wt = (cumsum(rnorm(N,0,1)));
#  WN = Wt[N];
#  p1 = (u-0.5*(sigma^2))*N; #p1 = (u-0.5*(sigma^2))*N/365
#  p2 = sigma*WN;
#  St = S0*exp(p1+p2);
#  return (St);
#}
#CI = function(data, percent){
#  m = mean(data);
#  s = sd(data);
#  p = qnorm(percent);
#  ul = m + s*p; ll = m - s*p;
#  return(c(ll,ul))
#}

#4. Votalility rate and drift rate code
#us = c(0.05,0.1,0.2,0.3,0.5,0.7,0.9,1,1.5,2,3,4)
#l = length(us)
#M = 5000; N = 1000; t = 1:N; sigma = 0.005; S0 = 100
#GBMPs = matrix(nrow = M, ncol = l);
#for(i in 1:M){
#  GBMPs[i,] = GBMP(N,sigma,us,S0)
#}
#apply(GBMPs,2,mean)
#apply(GBMPs,2,sd)

#5. Geometric Brownian Motion Model with Jump
#GBMJ = function(N,sigma,u,delta,lambda,S0){
#  Wt = cumsum(rnorm(N,0,1));
#  Nt = rpois(N,lambda)
#  dt<-1; #dt<-255,365
#  t = (1:N)/dt;
#  p1 = (u-0.5*(sigma^2))*t;
#  p2 = sigma*Wt;
#  p3 = (1-delta)^(Nt)
#  St = S0*exp(p1+p2)*p3;
#  return (St);
#}
#N = 1000; t = 1:N; sigma = 0.005; u = 0.2; S0 = 100; delta = 0.01; lambda = 2;
#G = GBMJ(N,sigma,u,delta,lambda,S0)
#plot(t,G,type= "l",main = "GBM Model with Jumps", xlab = "Time Interval",ylab="Predicted Value")
