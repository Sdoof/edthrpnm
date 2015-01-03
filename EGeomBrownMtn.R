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


## Part.1 example from web
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

#Part.2 example from web

m = 0.1; s = 0.2; dt = 15/9999; n = 10000
x<-rep(0,1)
x[1] = 10;
for(t in 2:n) {
  x[t] = x[t-1] * (1 + m * dt + s * sqrt(dt) * rnorm(1))
}
plot(x, type="l")
