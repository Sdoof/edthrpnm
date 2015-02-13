#geometric brown motion
#dSt = μ*St+σ*St*dBt
#dSt は 増分。　例：金融商品の価格の変化。
#dBt は ブラウン運動の増分。
#μ(mu)は(現在の St に対する割合であらわした)平均
#σ(sigma)は(現在の St に対する割合であらわした)ボラティリティ
#s0: 初期値
#length: シミュレーションの長さ。単位時間はvolatility(σ:sigma)
#        と一貫性を保つこと
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

#1. Vectorized Geometric Brownian Motion Code
#GBM = function(N,sigma,u,S0){
#  Wt = cumsum(rnorm(N,0,1));
#  dt<-1 #dt<-255, dt<-365
#  t = (1:N)/dt;
#  p1 = (u-0.5*(sigma^2))*t;
#  p2 = sigma*Wt;
#  St = S0*exp(p1 + p2);
#  return (St);
#}
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
