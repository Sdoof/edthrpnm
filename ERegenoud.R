#maximize the sin function
sin1 <- genoud(sin, nvars=1, max=TRUE)
#minimize the sin function
sin2 <- genoud(sin, nvars=1, max=FALSE)
## Not run:

#maximize a univariate normal mixture which looks like a claw
claw <- function(xx) {
  x <- xx[1]
  y <- (0.46*(dnorm(x,-1.0,2.0/3.0) + dnorm(x,1.0,2.0/3.0)) +
          (1.0/300.0)*(dnorm(x,-0.5,.01) + dnorm(x,-1.0,.01) + dnorm(x,-1.5,.01)) +
          (7.0/300.0)*(dnorm(x,0.5,.07) + dnorm(x,1.0,.07) + dnorm(x,1.5,.07)))
  return(y)
}
claw1 <- genoud(claw, nvars=1,pop.size=3000,max=TRUE)
## End(Not run)
## Not run:
#Plot the previous run
#xx <- seq(-3,3,.05)
#plot(xx,lapply(xx,claw),type="l",xlab="Parameter",ylab="Fit",
#     main="GENOUD: Maximize the Claw Density")
#points(claw1$par,claw1$value,col="red")


### Maximize a bivariate normal mixture which looks like a claw.
### New Example
aa <- function(xx) {
                        mNd2 <- function(x1, x2, mu1, mu2, sigma1, sigma2, rho)
                        {
                          z1 <- (x1-mu1)/sigma1
                          z2 <- (x2-mu2)/sigma2
                          w <- (1.0/(2.0*pi*sigma1*sigma2*sqrt(1-rho*rho)))
                          w <- w*exp(-0.5*(z1*z1 - 2*rho*z1*z2 + z2*z2)/(1-rho*rho))
                          return(w)
                        }
                        y <- 0.0
                        for(i in 1:(length(xx)-1)){
                          x1 <- xx[i]+1
                          x2 <- xx[i+1]+1
                          y <- y + (0.1*mNd2(x1,x2,0.0,0.0,1.0,1.0,0.0) -
                                      0.3*(mNd2(x1,x2,-1.0,-1.0,0.1,0.1,0.0) +
                                             mNd2(x1,x2,-0.5,-0.5,0.1,0.1,0.0) +
                                             mNd2(x1,x2,0.0,0.0,0.1,0.1,0.0) -
                                             mNd2(x1,x2,0.5,0.5,0.1,0.1,0.0) +
                                             mNd2(x1,x2,1.0,1.0,0.1,0.1,0.0)))
                        }
                        return(y)
}
bb <- genoud(aa, default.domains=20, data.type.int=TRUE,nvars=100,pop.size=5000,max=TRUE)
## End(Not run)
# For more examples see: http://sekhon.berkeley.edu/rgenoud/R

