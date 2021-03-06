
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
  
  ret_names<-c("anlzd","daily")
  ret_ <- vector("list",length(ret_names))
  ret_[[1]]<-vol_
  ret_[[2]]<-daily_
  names(ret_)<-ret_names
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
  f<-rep(0, times=(length+1))
  g<-rep(0, times=(length+1))
  h<-rep(0, times=(length+1))
  g[1]<-s0
  for(i in 2:(length+1)){
    f[i] <- f[i-1]+rnorm(1)
    g[i] <- g[1]*exp((mu-(sigma^2)/2)*(i-1)+sigma*f[i])
  }
  g
}

#1. Vectorized Geometric Brownian Motion Code
GBM = function(S0,u,sigma,N){
  Wt = cumsum(rnorm(N,0,1))
  dt<-1 #dt<-255, dt<-365
  t = (1:N)/dt
  p1 = (u-0.5*(sigma^2))*t
  p2 = sigma*Wt
  St = S0*exp(p1 + p2)
  return (St)
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

##
#
# MHmakeRandomString(n, length)
# function generates a random string random string of the
# length (length), made up of numbers, small and capital letters

MHmakeRandomString <- function(n=1, length=12)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    length, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}

##LOCAL UTILITY: local file distinct unique position
LocalDistinctPosition<-function(df){
  df %>% dplyr::rowwise() %>% do(md5sum=digest(paste(unlist(.)[1:length(opchain$Position)],collapse=""))) -> md5sum
  df$md5sum=unlist(md5sum)
  df %>% dplyr::distinct(md5sum,.keep_all=TRUE) %>% dplyr::arrange(.[,(length(opchain$Position)+1)]) -> df
  df$md5sum = NULL
  return(df)
}

##LOCAL UTILITY: local function to just avoid code copy
LocalMergeWriteFiles<-function(rf){
  tmp=read.table(rf,header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  #tmp=tmp[,1:(length(opchain$Position)+1)]
  fname=paste(rf,"_load.csv",sep="")
  if( file.exists(fname) ){
    tmp2=read.table(fname,header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  }else{
    tmp2=tmp[1,]
  }
  tmp %>% dplyr::full_join(tmp2) -> tmp3
  colnames(tmp3)[length(opchain$Position)+1]="eval"
  tmp3=LocalDistinctPosition(df=tmp3)
  write.table(tmp3,rf,row.names = F,col.names=F,sep=",",append=F)
}

##LOCAL UTILITY: Economic/NonEconomic Value is also evaluated for top maxNum spreads 
LocalflipScoreWriteToFile<-function(ResultFileName,maxNum){
  evaTable<-read.table(ResultFileName,
                       header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  evaTable=evaTable[,1:(length(opchain$Position)+1)]
  colnames(evaTable)=c(rep(1:length(opchain$Position)),"eval")
  flipFname=paste(ResultFileName,"_EcVFlp.csv",sep="")
  if( file.exists(flipFname)){
    file.create(flipFname)
  }
  
  for(tmp_pos_idx in 1:min(nrow(evaTable),maxNum)){
    evaPos<-unlist(evaTable[tmp_pos_idx,])[1:length(opchain$Position)]
    evaScore=evaTable[tmp_pos_idx,length(opchain$Position)+1]
    EvalFuncSetting$EvalEconomicValue<<-ifelse(EvalFuncSetting$EvalEconomicValue,F,T)
    
    originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
    EvalFuncSetting$LossLimitPrice<<-EvalFuncSetting$LossLimitPrice*10
    
    posnum=sum(abs(evaPos))
    tryCatch(
      flipScore<-obj_Income_sgmd(evaPos,EvalFuncSetting,isDebug=F,isDetail=F,
                                 udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                                 PosMultip=PosMultip,
                                 Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                                 Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum]),
      error=function(e){
        message(e)
      }
    )
    cat(evaPos,file=flipFname,sep=",",append=TRUE)
    cat(",",flipScore,",",evaScore,file=flipFname,append=TRUE)
    cat("\n",file=flipFname,append=TRUE)
    
    EvalFuncSetting$LossLimitPrice<<-originalLossLimitPrice
    EvalFuncSetting$EvalEconomicValue<<-ifelse(EvalFuncSetting$EvalEconomicValue,F,T)
  }
  tmp<-read.table(flipFname,header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp=tmp[,1:(length(opchain$Position)+2)]
  colnames(tmp)=c(rep(1:length(opchain$Position)),"eval","eval_org")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
  #posnum
  tmp[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp2
  tmp2  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp3
  tmp$putn<-unlist(tmp3$putn);tmp$calln<-unlist(tmp3$calln);rm(tmp2);rm(tmp3)
  tmp %>% dplyr::mutate(posn=(putn+calln)) -> tmp
  #write to a file
  write.table(tmp,flipFname,row.names = F,col.names=F,sep=",",append=F)
}

##LOCAL UTILITY: return String of the sampling condition
LocalcreateSampleConditionStr<-function(EvalFuncSetting){
  
  ConvexEvalInCoef_chunk=ifelse(EvalFuncSetting$EvalConvex,
                                paste(ifelse(EvalFuncSetting$ConvexNormalizeByProfit,"UseP","UseSd"),"_",
                                      EvalFuncSetting$ConvexEvalInCoef["InCoefSD"]*100,"_",
                                      EvalFuncSetting$ConvexEvalInCoef["InCoefMaxLoss"]*100,
                                      sep=""),"")
  
  Eval1DayDist_chunk=ifelse(EvalFuncSetting$Eval1DayDist,
                            paste(EvalFuncSetting$Eval1DayDistWeightROnHoldDay,"_",
                                  EvalFuncSetting$Coef1DayDist,
                                  sep=""),"")
  
  outStr=paste(EvalFuncSetting$UdlStepPct*1000,"x",EvalFuncSetting$UdlStepNum,"-",EvalFuncSetting$holdDays,"d-",
               EvalFuncSetting$Profit_Coef,"_",EvalFuncSetting$AdvEffect_Coef,"-",
               EvalFuncSetting$DrctlEffect_Coef["DeltaECoef"],"_",EvalFuncSetting$DrctlEffect_Coef["VegaECoef"],"_",EvalFuncSetting$MaxLoss_Coef,"-",
               "Pcdnl$",paste(ifelse(EvalFuncSetting$ConditonalProfitEval,"T","F")),"-",
               "DOf$",EvalFuncSetting$Delta_Neutral_Offset[1],"-",
               #"DTh$",EvalFuncSetting$Delta_Thresh_Minus[1],"_",EvalFuncSetting$Delta_Thresh_Plus[1],"-",
               #"VOf$",EvalFuncSetting$Vega_Neutral_Offset[1],
               #"VTh$",EvalFuncSetting$Vega_Thresh_Minus[1],"_",EvalFuncSetting$Vega_Thresh_Plus[1],"-",
               "HIVR$",EvalFuncSetting$HV_IV_Adjust_Ratio,"-",
               "GkW$",EvalFuncSetting$GreekEfctOnHldD,"-",
               "Cvx$",ifelse(EvalFuncSetting$EvalConvex,"T","F"),ConvexEvalInCoef_chunk,"-",
               "Sor$",ifelse(EvalFuncSetting$UseSortinoRatio,"T","F"),"-",
               "1Day$",ifelse(EvalFuncSetting$Eval1DayDist,"T","F"),Eval1DayDist_chunk,"-",
               "Ecv$",ifelse(EvalFuncSetting$EvalEconomicValue,"T","F"),"-",
               "LLt$",EvalFuncSetting$LossLimitPrice,
               sep="")
  return(outStr)
}


##LOCAL UTILITY: evaluate each position

LocalapplyEvalufunction <- function(evalx, thresh, EvalFuncSetting, 
                                    isDebug, isDetail,isFileout) {
  posnum=sum(as.numeric((evalx)!=0))
  if(posnum==0)
    return()
  
  fname=paste(".\\ResultData\\inigreedy.csv",sep="")
  #cache check and evaluate
  val<-thresh
  md5sumOfPos=digest(paste(evalx,collapse = ""))
  if(has.key(md5sumOfPos, POSITION_OPTIM_HASH)==FALSE){
    tryCatch(
      val<-obj_Income_sgmd(evalx,EvalFuncSetting,isDebug=isDebug,isDetail=isDetail,
                           udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                           PosMultip=PosMultip,
                           lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                           Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                           Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum]),
      error=function(e){
        message(e)
        cat("val:",val,"thresh",thresh,"\n")
      })
    POSITION_OPTIM_HASH[md5sumOfPos]<<-val
  }else{
    val<-POSITION_OPTIM_HASH[[md5sumOfPos]]
    HASH_HIT_NUM<<-HASH_HIT_NUM+1
  }
  
  #value check
  tryCatch(
    if(val<thresh){
      # write to the file
      if(isFileout){
        cat(evalx,
            file=fname,
            sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
        cat(val,file=fname,"\n",append=TRUE)
      }
    },
    error=function(e){
      message(e)
      cat("val:",val,"thresh:",thresh,"evalx",evalx,"\n")
      val=ifelse(is.na(val),thresh,val)
      cat("val:",val,"thresh:",thresh,"evalx",evalx,"\n")
      POSITION_OPTIM_HASH[md5sumOfPos]<<-val
    })
}


## Selective Hist IV


##
# select Suffix to icnlude the valid IV data and make the selected suffix suitable to 
# apply to vector created by PCndCtC or IVCFndCtC functions.
selectSuffixForValidIV <- function(histIV,xDayInt,a_low,d_low,a_high,d_high){
  theIV=histIV[1]
  SelectInclude=(histIV>=theIV*a_low | histIV>=(theIV-d_low))&(histIV<=theIV*a_high | histIV<=(theIV+d_high))
  
  SelectIncludeShift=rep(0,length(SelectInclude))#numeric((length(SelectInclude)-xDayInt))
  SelectIncludeShift[(xDayInt+1):length(SelectInclude)]=SelectInclude[(xDayInt+1):length(SelectInclude)]
  which(SelectIncludeShift==1)->suffix_slctd
  suffix_slctd=suffix_slctd-xDayInt
  return(suffix_slctd)
}

##
# Price2IVIDX called from saveP2IVReg
#  histPrc : vector such as histPrc$Close, histIVDf : vector such as histIV$Close
Price2IVIDX <- function(histPrc,histIV,dataNum,xDayInt,start_day=1,effectiv_suffix=0){
  
  PCxdCtC<- PCndCtC(hist=histPrc,n=xDayInt)
  IVCFxdCtC<-IVCFndCtC(iv=histIV,n=xDayInt)
  
  if(length(effectiv_suffix)>2){
    PCxdCtC[effectiv_suffix]->tmp
    na.omit(tmp) %>% as.vector() -> PCxdCtC
    IVCFxdCtC[effectiv_suffix]->tmp
    na.omit(tmp) %>% as.vector() -> IVCFxdCtC
  }
  
  cat("effectiv_suffix PCxdCtC",length(PCxdCtC),"\n")
  cat("effectiv_suffix IVCFxdCtC",length(IVCFxdCtC),"\n")
  
  if(length(PCxdCtC)<=(start_day+dataNum))
    dataNum=length(PCxdCtC)-start_day
  
  #Regression
  PCxdCtC=PCxdCtC[start_day:(start_day+dataNum-1)]
  na.omit(PCxdCtC)  %>% as.vector() -> PCxdCtC
  IVCFxdCtC=IVCFxdCtC[start_day:(start_day+dataNum-1)]
  na.omit(IVCFxdCtC)  %>% as.vector() -> IVCFxdCtC
  
  PCxdCtC=PCxdCtC[1:min(length(PCxdCtC),length(IVCFxdCtC))]
  IVCFxdCtC=IVCFxdCtC[1:min(length(PCxdCtC),length(IVCFxdCtC))]
  
  cat("dataNum PCxdCtC",length(PCxdCtC),"\n")
  cat("dataNum IVCFxdCtC",length(IVCFxdCtC),"\n")
  
  P2IVxd <- data.frame(PCxdCtC=PCxdCtC, IVCFxdCtC=IVCFxdCtC)
  co=cor(PCxdCtC[start_day:min((start_day+dataNum),length(PCxdCtC))],
         IVCFxdCtC[start_day:min((start_day+dataNum),length(IVCFxdCtC))])
  
  #returned list
  ret_names<-c("P2IVxd","cor","PCxdCtC","IVCFxdCtC")
  ret <- vector("list",length(ret_names))
  ret[[1]]<-P2IVxd
  ret[[2]]<-co
  ret[[3]]<-PCxdCtC
  ret[[4]]<-IVCFxdCtC
  names(ret)<-ret_names
  return(ret)
}

##
# save the regression result.
saveP2IVReg<-function(histPrc,histIV,dataNum,xDayInt,start_day=1,effectiv_suffix=0){
  tmp=Price2IVIDX(histPrc,histIV,dataNum,xDayInt,start_day,effectiv_suffix=effectiv_suffix)
  P2IVxd=tmp$P2IVxd
  co=tmp$cor
  print(co)
  
  #linear regression
  norns.lm<-lm(IVCFxdCtC~PCxdCtC, data=P2IVxd)
  summary(norns.lm)
  save.PC2IV(model=norns.lm,
             PC=paste("PC",xDayInt,"dCtC",sep=""),
             IVC=paste("IVCF",xDayInt,"dCtC",sep=""),
             cor=co,
             pcstat=c(mean(tmp$PCxdCtC),sd(tmp$PCxdCtC)),
             ivstat=c(mean(tmp$IVCFxdCtC),sd(tmp$IVCFxdCtC)))
  
  #returned list
  ret_names<-c("P2IVxd","lm")
  ret <- vector("list",length(ret_names))
  ret[[1]]<-P2IVxd
  ret[[2]]<-norns.lm
  names(ret)<-ret_names
  return(ret)
}

#Non Linear
saveP2IVNonLReg<-function(histPrc,histIV,dataNum,xDayInt,df,start_day=1,effectiv_suffix=0){
  tmp=Price2IVIDX(histPrc,histIV,dataNum,xDayInt,start_day,effectiv_suffix=effectiv_suffix)
  P2IVxd=tmp$P2IVxd
  co=tmp$cor
  print(co)
  
  #linear regression
  model.ss<-smooth.spline(P2IVxd$PCxdCtC,
                          P2IVxd$IVCFxdCtC,
                          df=df)
  
  (predict.c <- predict(model.ss,x=seq(-0.2,0.2,by=0.005)))
  
  save.NLPC2IV(model=model.ss,
               PC=paste("PC",xDayInt,"dCtC",sep=""),
               IVC=paste("IVCF",xDayInt,"dCtC",sep=""),
               cor=co,
               pcstat=c(mean(tmp$PCxdCtC),sd(tmp$PCxdCtC)),
               ivstat=c(mean(tmp$IVCFxdCtC),sd(tmp$IVCFxdCtC)))
  
  #returned list
  ret_names<-c("P2IVxd","model")
  ret <- vector("list",length(ret_names))
  ret[[1]]<-P2IVxd
  ret[[2]]<-predict.c
  names(ret)<-ret_names
  return(ret)
}

##
# Daily Volatility of Implied Volatility statistic of selected subdata
getDVIVStat <- function(histIV,xDayVol,effectiv_suffix,isDebug=F){
  DVIV_V=rep(0,times=length(effectiv_suffix))
  dviv_suff=1
  # histIV[i+xdays], +(xdays)(->) means data backs to xdays PAST
  # histIV[i-xdays], -(xdays)(<-) means data looks forward xdays FUTURE
  for(i in 1:length(effectiv_suffix)) {
    if(effectiv_suffix[i]<=length(histIV)-xDayVol){
      histIV[effectiv_suffix[i]:(effectiv_suffix[i]+xDayVol)]
      #toward the past
      DVIV=annuual.daily.volatility( histIV[effectiv_suffix[i]:(effectiv_suffix[i]+xDayVol)] )$daily
      if(isDebug)
        cat("at",effectiv_suffix[i],
            "DVIV",DVIV,"AVIV",DVIV*sqrt(252),
            "IV",histIV[(effectiv_suffix[i])],
            "\n")
      DVIV_V[dviv_suff]=DVIV
      dviv_suff=dviv_suff+1
    }
  }
  DVIV=DVIV_V[1:(dviv_suff-1)]
  return(DVIV)
}

##
# select Suffix to icnlude the valid IV data
selectSuffixToPredictHV <- function(histIV,a_low,d_low,a_high,d_high){
  theIV=histIV[1]
  SelectInclude=(histIV>=theIV*a_low | histIV>=(theIV-d_low))&(histIV<=theIV*a_high | histIV<=(theIV+d_high))
  
  which(SelectInclude==TRUE)->suffix_slctd
  
  return(suffix_slctd)
}

##
# realized HV/IV ratio statistic of selected subdata
getHVIVRStat <- function(histPrc,histIV,xDayVol,effectiv_suffix,isDebug=F){
  HVIVR_V=rep(0,times=length(effectiv_suffix))
  hvivr_suff=1
  # histPrc[i+xdays], +(xdays)(->) means data backs to xdays PAST
  # histPrc[i-xdays], -(xdays)(<-) means data looks forward xdays FUTURE
  for(i in 1:length(effectiv_suffix)){
    if(effectiv_suffix[i]>=xDayVol){
      histPrc[effectiv_suffix[i]:(effectiv_suffix[i]+xDayVol)]
      #toward the FUTURE
      HVIVR=annuual.daily.volatility( histPrc[(effectiv_suffix[i]-xDayVol):effectiv_suffix[i]] )$anlzd/
        (histIV[(effectiv_suffix[i])]/100)
      if(isDebug)
        cat("at",i,
            "HV",annuual.daily.volatility( histPrc[(effectiv_suffix[i]-xDayVol):effectiv_suffix[i]] )$anlzd,
            "IV",histIV[(effectiv_suffix[i])]/100,
            "HVIVR",HVIVR,"\n")
      HVIVR_V[hvivr_suff]=HVIVR
      hvivr_suff=hvivr_suff+1
    }
  }
  HVIVR=HVIVR_V[1:(hvivr_suff-1)]
  return(HVIVR)
}



