library(dplyr)
library(RQuantLib)
library(DEoptim)
library(rgenoud)
library(Rsolnp)
library(DEoptimR)
library(mcga)
library(GenSA)
library(powell)
library(hydroPSO)
library(nleqslv)
library(dfoptim)
library(GA)
library(Rmalschains)
library(soma)
library(nloptr)
library(NMOF)
library(pracma)

#Variables -----------
#Holding Period
#holdDays<-3*252/365 #Trading Days. This should be correct.
holdDays<-3
#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays<-20
#Multipler of Position
PosMultip<-100

#Option Chain and Position Data. Here we use UDL_Positions_Pre ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",")
#filtering. deleting unnecessary column
opchain %>% dplyr::select(-(contains('Frac',ignore.case=TRUE)),
                          -(IV)) %>% as.data.frame() -> opchain
#only OOM targeted
opchain %>% dplyr::filter(HowfarOOM>=0) -> opchain
#assiging initial Position?
#get position where opchain$Position!=0
opchain %>% dplyr::filter(Position!=0) -> position

##Historical Implied Volatility Data ---------------
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",nrows=1999);rm(rf)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(position$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

#Initial and evaluation vector -
iniPos<-opchain$Position
iniPos<-rep(0,length(iniPos))
evaPos<-opchain$Position

#test sample value -----------
best_result<-1.0
obj_Income_sgmd(x=evaPos,isDebug=TRUE,isDetail=TRUE,isFileout=FALSE)
obj_Income(x=evaPos,isDebug=TRUE)
#obj_Income_Cont(x=evaPos,isDebug=TRUE)
#obj_Income_genoud_lex_int(x=evaPos,isDebug=TRUE)

rm(best_result,holdDays,dviv_caldays,PosMultip)
rm(iniPos,evaPos)
rm(opchain,histIV,position)

#Data Setup. Provisioning
#Load Regression and Correlation Parameters ---------------
load.PC2IV(PC="PC3dCtC",IVC="IVCF3dCtC")
PC3dCtC_IVCF3dCtC
load.PC2IV(PC="PC5dCtC",IVC="IVCF5dCtC")
PC5dCtC_IVCF5dCtC
load.PC2IV(PC="PC7dCtC",IVC="IVCF7dCtC")
PC7dCtC_IVCF7dCtC
load.PC2IV(PC="PC1dCtC",IVC="IVCF1dCtC")
PC1dCtC_IVCF1dCtC
load.PC2IV(PC="PC1dCtO",IVC="IVCF1dCtO")
PC1dCtO_IVCF1dCtO
load.Skew()
SkewModel
load.VCone(optype=OpType_Put_G)
PutVCone
load.VCone(optype=OpType_Call_G)
CallVCone
load.IVChg(OpType_Put_G,10)
PutIVChgUp
load.IVChg(OpType_Put_G,-10)
PutIVChgDown
load.IVChg(OpType_Call_G,10)
CallIVChgUp
load.IVChg(OpType_Call_G,-10)
CallIVChgDown


#creating initial population

#sigmoid function  ------
for(tmp in 1:3){
   create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,thresh=6.0,putn=6,calln=2,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-08P6C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,thresh=6.0,putn=5,calln=3,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-08P5C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,thresh=6.0,putn=4,calln=4,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-08P4C4-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,thresh=6.0,putn=5,calln=2,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-07P5C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,thresh=6.0,putn=4,calln=3,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-07P4C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=3000,opchain$TYPE,thresh=6.0,putn=4,calln=2,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-06P4C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=2000,opchain$TYPE,thresh=6.0,putn=3,calln=3,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-06P3C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=2000,opchain$TYPE,thresh=6.0,putn=6,calln=0,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-06P6C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=1500,opchain$TYPE,thresh=6.0,putn=3,calln=2,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-05P3C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=1500,opchain$TYPE,thresh=6.0,putn=5,calln=0,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-05P5C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=1000,opchain$TYPE,thresh=6.0,putn=4,calln=0,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-04P4C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=800,opchain$TYPE,thresh=6.0,putn=2,calln=2,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-04P2C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=500,opchain$TYPE,thresh=6.0,putn=3,calln=0,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-03P3C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=500,opchain$TYPE,thresh=6.0,putn=2,calln=0,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-02P2C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
   create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,thresh=6.0,putn=0,calln=2,ml=2,
                                           fname=paste(".\\ResultData\\inipop-Exc-1000-02P0C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                           isFileout=TRUE,isDebug=FALSE)
};rm(tmp)

#creating candidate pool for combined search --------

tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-1000-08P6C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                pnum=0,nrows=-1,skip=0,method=1)
tmp<-tmp[1:1000,]
pools<-list(list(c(8,6,2),tmp)) #No.[[1]]
poolidx<-2

tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-1000-06P4C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                pnum=0,nrows=-1,skip=0,method=1)
tmp<-tmp[1:1000,]
pools[poolidx]<-list(list(c(6,4,2),tmp)) ; poolidx<-poolidx+1 #No.[[2]]

tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-1000-04P2C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                 pnum=0,nrows=-1,skip=0,method=1)
tmp<-tmp[1:300,]
pools[poolidx]<-list(list(c(4,2,2),tmp)) ; poolidx<-poolidx+1 #No.[[3]]

rm(poolidx,tmp)

#combined population serach --------------

#sigmoid evaluation
#2 Combinations
#8P6C2+6P4C2
create_combined_population(popnum=3000,thresh=6.0,plelem=c(1,2),fname=paste(".\\ResultData\\combine-Result-0862+0642-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)

#8P6C2+4P2C2
create_combined_population(popnum=3000,thresh=6.0,plelem=c(1,3),fname=paste(".\\ResultData\\combine-Result-0862+0422-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)

#6P4C2+4P2C2
create_combined_population(popnum=3000,thresh=6.0,plelem=c(2,3),fname=paste(".\\ResultData\\combine-Result-0642+0422-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)

#6P4C2+6P4C2
create_combined_population(popnum=3000,thresh=6.0,plelem=c(2,2),fname=paste(".\\ResultData\\combine-Result-0642+0642-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)

#8P6C2+8P6C2
create_combined_population(popnum=3000,thresh=6.0,plelem=c(1,1),fname=paste(".\\ResultData\\combine-Result-0862+0862-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)


#3 Combinations
#8P6C2+6P4C2+4P2C2
create_combined_population(popnum=1000,thresh=6.0,plelem=c(1,2,3),fname=paste(".\\ResultData\\combine-Result-0862+0642+0422-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)

#6P4C2+6P4C2+4P2C2
create_combined_population(popnum=1000,thresh=6.0,plelem=c(2,2,3),fname=paste(".\\ResultData\\combine-Result-0642+0642+0422-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)

#6P4C2+4P2C2+4P2C2
create_combined_population(popnum=1000,thresh=6.0,plelem=c(2,3,3),fname=paste(".\\ResultData\\combine-Result-0642+0422+0422-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)
#8P6C2+8P6C2+6P4C2
create_combined_population(popnum=1000,thresh=6.0,plelem=c(1,1,2),fname=paste(".\\ResultData\\combine-Result-0862+0862+0642-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8)

#3 3+3 Combinations

#create nested combine candidate pool 
poolidx<-length(pools)+1
tmp<-createCombineCandidatePool(fname=paste(".\\ResultData\\combine-Result-3Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                pnum=0,nrows=-1,skip=0,method=1)
tmp<-tmp[1:3000,]
pools[poolidx]<-list(list(c(0,0,0),tmp)) ; poolidx<-poolidx+1 #No.[[4]]
rm(poolidx,tmp)

create_combined_population(popnum=8000,thresh=6.0,plelem=c(4,4),fname=paste(".\\ResultData\\combine-Result-3Cb3Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=FALSE,maxposn=8) 

rm(pools)

#initially evaluate using continuous value, after some point evaluated by
#INT round values. In both cases, pos_change should be evaluated by INT.

##

# Optimize Engine  
#EDoptimR ========
#deoptR_inipop_vec<- read already created population
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
#isGenoud=FALSE ; isMCGA=FALSE
outjdopr<-JDEoptim(lower=rep(-6,length(iniPos)),upper=rep(6,length(iniPos)), fn=obj_Income_sgmd,
                   tol=1e-10,NP=10*length(iniPos),
                   maxiter=15*length(iniPos),#50*length(iniPos),
                   #add_to_init_pop=matrix(deoptR_inipop_vec,
                   #                       nrow=length(iniPos),ncol=length(iniPos)),
                   trace=TRUE,detail=TRUE)
rm(deoptR_inipop_vec,best_result,result_file,outjdopr)

#genoud ========
#genoud_inipop_vec <- create somewhere
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
domain<-matrix(c(rep(-6,length(evaPos)),rep(6,length(iniPos))), nrow=length(iniPos), ncol=2)
outgen <- genoud(#fn=obj_Income_genoud_lex_int,lexical=TRUE,
                 fn=obj_Income_sgmd,
                 nvars=length(iniPos),pop.size=1000,max.generations=100,
                 data.type.int=TRUE,#FALSE
                 wait.generations=100,gradient.check=FALSE,MemoryMatrix=TRUE,
                 boundary.enforcement=2,
                 starting.values=matrix(genoud_inipop_vec,
                                        nrow=10*length(evaPos),ncol=length(evaPos),byrow=T),
                 Domains=domain)
rm(domain,best_result,result_file,outgen)

#hydroPSO ======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
outhdpso<-hydroPSO(par=as.numeric(evaPos),#rnorm(n=length(iniPos),mean=0,sd=2)
                   fn=obj_Income_sgmd,#obj_Income_Cont,
                   lower=rep(-6,length(evaPos)), upper=rep(6,length(evaPos)))
rm(result_file,best_result,outhdpso)

#dfoptim  =======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
outhjkb<-hjkb(par=evaPos, #rnorm(n=length(iniPos),mean=0,sd=2),
              fn=obj_Income_sgmd, lower = rep(-6,length(iniPos)), upper =rep(6,length(iniPos)))
outnmkb<-nmkb(par=evaPos,#norm(n=length(iniPos),mean=0,sd=1)
              fn=obj_Income_sgmd, lower = rep(-6,length(iniPos)), upper =rep(6,length(iniPos)))

rm(result_file,best_result,outhjkb,outnmkb)

#nloptr sbplx ======= 
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-490
outnloptr <-sbplx(rnorm(n=length(iniPos),mean=0,sd=2),#as.numeric(evaPos),
                  obj_Income_Cont, lower = rep(-6,length(iniPos)), upper = rep(6,length(iniPos)))
rm(result_file,best_result,outnloptr)
#nloptr direct/directL ======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-490
nl.opts(list(xtol_rel = 1e-8, maxeval = 3000,check_derivatives = FALSE)) 
outnloptr <-directL(obj_Income_Cont,
                    #scaled=FALSE, #direct
                    randomized=TRUE, #directL
                    lower = rep(-6,length(iniPos)), upper = rep(6,length(iniPos)))
rm(result_file,best_result,outnloptr)

#nloptr newuoa,auglag ====== 
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-490
outnloptr <-newuoa(as.numeric(evaPos),#rnorm(n=length(iniPos),mean=0,sd=2),#
                  obj_Income_Cont, lower = rep(-6,length(iniPos)), upper = rep(6,length(iniPos)))
rm(result_file,best_result,outnloptr)

#malsch  ======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
#isMCGA=TRUE ;isGenoud=FALSE
outmalsch<-malschains(fn=obj_Income_sgmd, lower=rep(-6,length(iniPos)), upper=rep(6,length(iniPos)))
rm(result_file,best_result,outmalsch)

#MCGA ========
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
#isMCGA=TRUE ;isGenoud=FALSE
outm <- mcga( popsize=200,chsize=as.numeric(length(iniPos)),minval=-6,maxval=6,maxiter=300,
              crossprob=1.0,mutateprob=0.01,evalFunc=obj_Income_sgmd)
rm(result_file,best_result,outm)


#GenSA Intermediate not sufficient =======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
#isGenoud=FALSE ; isMCGA=FALSE
outgensa<-GenSA(par=as.numeric(evaPos),fn=obj_Income_sgmd,lower=rep(-6.1,length(iniPos)),upper=rep(6.1,length(iniPos)))
rm(result_file,best_result,outgensa)

#DEOptim Intermediate not sufficient  =======
deopt_inipop_vec<-create_initial_polulation(popnum=10*length(evaPos),thresh=1000)
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
outdeop <- DEoptim(fn=obj_Income_sgmd,lower = rep(-6,length(iniPos)),upper = rep(6,length(iniPos)),
                   control=DEoptim.control(itermax = 200,strategy=1,
                                           initialpop=matrix(deopt_inipop_vec,
                                                             nrow=10*length(evaPos),ncol=length(evaPos),byrow=T)))
rm(result_file,best_result,outdeop)
rm(deopt_inipop_vec)

#powell Intermediate not suffucient.  ======
powell(par=evaPos,fn=obj_Income_mcga)

#soma(m,x)  ==============
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-520
#isMCGA=TRUE ;isGenoud=FALSE
outsoma <- soma(obj_Income_Cont, list(min=rep(-6,length(iniPos)),max=rep(6,length(iniPos))))

#NMOF(DeOpt) =========
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
#isMCGA=FALSE ;isGenoud=TRUE
DEopt(OF=obj_Income_sgmd,algo=list(nP=500L,nG=150L,F=0.6,CR=0.9,min=rep(-6,length(iniPos)),max=rep(6,length(iniPos))))
rm(result_file,best_result)

#solnp XXXX not suffucient =======
result_file<-paste(".\\ResultData\\expresult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".txt",sep="")
best_result<-1.2
solnpIneqfn = function(x)
{
  x<-round(x)
  pos_change<-sum(as.numeric((x-iniPos)!=0))
  pos_change
}
solnpIneqLB = rep(0,1)
solnpIneqUB = rep(10,1)
solnpLB = rep(-6,length(iniPos))
solnpUB = rep(6,length(iniPos))

outsolnp<-solnp(pars=evaPos,fun=obj_Income_sgmd,
                #distr=rnorm(n=length(iniPos),mean=0,sd=1),
                ineqfun=solnpIneqfn,ineqLB=solnpIneqLB,ineqUB=solnpIneqUB,
                LB=solnpLB,UB=solnpUB)

rm(solnpIneqfn,solnpIneqLB,solnpIneqUB,solnpLB,solnpUB)
rm(result_file,best_result,outsolnp)

##

##
# functions optimized  -------------

obj_Income_sgmd <- function(x,isDebug=FALSE,isDetail=FALSE,isFileout=TRUE,isMCGA=FALSE,isGenoud=FALSE,
                            udlStepNum=4,udlStepPct=0.02,maxposnum=8,
                            tail_rate=0.4,lossLimitPrice=30000){
  if(isMCGA){
    x<-as.numeric(x<(-6.5))*(-6)+as.numeric(x>(6.5))*6+as.numeric(x>=(-6.5)&x<=6.5)*x
  }
  if(!isGenoud){
    x<-round(x)
  }
  if(sum(as.numeric(round(x)!=0))==0){
    x<-rnorm(n=length(iniPos),mean=0,sd=1)
    x<-round(x)
  }
  #gradually change constraint
  exp_c<-0
  #position where pos$Position != 0
  position<-hollowNonZeroPosition(pos=x)
  #position evaluated after holdDays later
  udlStepNum<-udlStepNum; udlStepPct<-udlStepPct
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  #At day 0 position price and Greeks.
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip)
  if(isDetail){print(posEvalTbl$pos)}
  if(isDetail){print(posEvalTbl)}  
  
  ##
  # penalty1: position total num
  pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-maxposnum)>0)*(pos_change-maxposnum))^5
  if(penalty1>2){
    if(isDebug){cat("pos num",pos_change,"\n")}
    return((pos_change-maxposnum)*penalty1 )
  }
  if(isDebug){cat(x," ");cat(" :p1",penalty1)}
  
  ##
  # penalty4. ThetaEffect. This should be soft constraint  
  sd_multp<-holdDays;anlzd_sd<-getIV_td(histIV$IVIDX[1]);sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  theta_ttl<-thePositionGrk$ThetaEffect+sum(posEvalTbl$ThetaEffect*weight)
  
  exp_c<-as.numeric((pos_change-maxposnum)>0)*0+as.numeric((pos_change-maxposnum)<=0)
  penalty4<-(1+as.numeric(theta_ttl<0)*(abs(theta_ttl)))^exp_c
  
  if(isDetail){cat(" :thta_ttl",theta_ttl);cat(" :p4",penalty4)}
  if(isDetail){cat(" :thta_ini",thePositionGrk$ThetaEffect);cat(" :thta_hld",sum(posEvalTbl$ThetaEffect*weight))}
  
  ##
  # penalty2 tail-risk
  tailPrice<-min(sum(getIntrisicValue(position$UDLY[1]*(1-tail_rate),position)),
                 sum(getIntrisicValue(position$UDLY[1]*(1+tail_rate),position)))
  lossLimitPrice <- (-1)*lossLimitPrice
  penalty2<-(1+as.numeric((tailPrice-lossLimitPrice)<0)*(10))^exp_c
  if(isDebug){cat(" :tlpr",tailPrice);cat(" :lslmt",lossLimitPrice);cat(" :p2",penalty2)}
  
  ##
  # penalty3, cost3: profit must be positive. also must be a cost term.
  if(isDetail){cat(" :prc_hd",posEvalTbl$Price);cat(" :prc_ini:",getPositionGreeks(position,multi=PosMultip)$Price)}
  if(isDetail){cat(" :prft",posEvalTbl$Price-getPositionGreeks(position,multi=PosMultip)$Price)}
  
  sd_multp<-holdDays;anlzd_sd<-getIV_td(histIV$IVIDX[1]);sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  if(isDebug){cat(" :wht",weight)} 
  profit_hdays<-sum((posEvalTbl$Price-thePositionGrk$Price)*weight)
  if(isDebug){cat(" :prft_wt",profit_hdays)}  
  exp_c<-as.numeric((pos_change-maxposnum)>0)*0+as.numeric((pos_change-maxposnum)<=0)
  #penalty3<-(1+as.numeric(profit_hdays<0)*(abs(profit_hdays)))^exp_c
  penalty3<-1
  if(isDebug){cat(" :p3",penalty3)}
  # cost 3
  cost3<- sigmoid(-1*profit_hdays*0.001,a=1,b=0)
  if(isDebug){cat(" :cost3",cost3)}
  ##
  # cost5 Each Effects.
  #weight is normalized
  sd_multp<-holdDays;anlzd_sd<-getIV_td(histIV$IVIDX[1]);sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  if(isDebug){cat(" :wht2",weight)}
  c5<- -sum((posEvalTbl$DeltaEffect+posEvalTbl$GammaEffect+
               posEvalTbl$VegaEffect+posEvalTbl$ThetaEffect)*weight)
  cost5<-sigmoid(c5*0.001,a=1,b=0)
  if(isDebug){cat(" c5:",c5," :cost5",cost5)}
  
  ##
  # total cost is weighted sum of each cost.
  #cost<-(cost3+cost5)
  cost<-(cost5/(1-cost3))
  if(isDebug){cat(" :cost",cost," ")}
  
  ##
  # total cost and penalty
  val<-cost*penalty1*penalty2*penalty3*penalty4
  
  if(isDebug){cat(" val:",val,"\n")}
  
  if(isFileout){
    if(val<best_result){
      cat(x,file=result_file,sep=",",append=TRUE);cat(",",file=result_file,append=TRUE)
      cat(val,file=result_file,"\n",append=TRUE)
      best_result<<-val+0.1
      #assign(best_result,val,env=.GlobalEnv)
    }
  }
  return(val)
}


obj_Income <- function(x,isDebug=FALSE,isMCGA=FALSE,isGenoud=FALSE){
  if(isMCGA){
    x<-as.numeric(x<(-6.5))*(-6)+as.numeric(x>(6.5))*6+as.numeric(x>=(-6.5)&x<=6.5)*x
  }
  if(!isGenoud){
    x<-round(x)
  }
  if(sum(as.numeric(round(x)!=0))==0){
    #x<-rep(1,length=length(x))
    x<-rnorm(n=length(iniPos),mean=0,sd=1)
    x<-round(x)
  }
  exp_c<-0
  
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-4; udlStepPct<-0.02
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip)
  if(isDebug){print(posEvalTbl$pos)}
  if(isDebug){print(posEvalTbl)}
  
  ##
  # penalty1: position total num
  pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  if(penalty1>2){
    if(isDebug){cat("pos num",pos_change,"\n")}
    return( (500+(pos_change-10))*penalty1 )
  }
  if(isDebug){cat(x," ");cat(" :p1",penalty1)}
  
  ##
  # penalty4. ThetaEffect. This should be soft constraint
  
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*2
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  theta_ttl<-thePositionGrk$ThetaEffect+sum(posEvalTbl$ThetaEffect*weight)
  
  exp_c<-as.numeric((pos_change-10)>0)*0+as.numeric((pos_change-10)<=0)
  penalty4<-(1+as.numeric(theta_ttl<0)*(abs(theta_ttl)))^exp_c
  ##
  #cost4 <- -1*theta_ttl;cost4<-0
  if(isDebug){cat(" :thta_ttl",theta_ttl);cat(" :p4",penalty4)}
  #cat(" :thta_ini",thePositionGrk$ThetaEffect);cat(" :thta_hld",sum(posEvalTbl$ThetaEffect*weight))
  
  ##
  # penalty2 tail-risk
  tail_rate<-0.4
  tailPrice<-min(sum(getIntrisicValue(position$UDLY[1]*(1-tail_rate),position)),
                 sum(getIntrisicValue(position$UDLY[1]*(1+tail_rate),position)))
  #lossLimitPrice <- -1*position$UDLY[1]*PosMultip*(tail_rate+0.1)
  lossLimitPrice <- -20000
  
  #exp_c<-as.numeric(exp_c>0)*(as.numeric((penalty4-2)>0)*0+as.numeric((penalty4-2)<0)*2)
  #penalty2<-(1+as.numeric((tailPrice-lossLimitPrice)<0)*(abs(tailPrice)))^exp_c
  penalty2<-(1+as.numeric((tailPrice-lossLimitPrice)<0)*(10))^exp_c
  
  ##
  #cost2<-(1+as.numeric(penalty2>1)*10);cost2<-0
  if(isDebug){cat(" :tlpr",tailPrice);cat(" :lslmt",lossLimitPrice);cat(" :p2",penalty2)}
  
  ##
  # penalty3, cost1 profit must be positive. also must be a cost term.
  if(isDebug){cat(" :prc_hd",posEvalTbl$Price);cat(" :prc_ini:",getPositionGreeks(position,multi=PosMultip)$Price)}
  if(isDebug){cat(" :prft",posEvalTbl$Price-getPositionGreeks(position,multi=PosMultip)$Price)}
  
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  if(isDebug){cat(" :wht",weight)}
  
  #exp_c<-as.numeric(exp_c>0)*(as.numeric((penalty3-2)>0)*0+as.numeric((penalty3-2)<0)*2)
  profit_hdays<-sum((posEvalTbl$Price-thePositionGrk$Price)*weight)
  if(isDebug){cat(" :prft_wt",profit_hdays)}
  
  penalty3<-(1+as.numeric(profit_hdays<0)*(abs(profit_hdays)))^exp_c
  penalty3<-1
  if(isDebug){cat(" :p3",penalty3)}
  ##
  # cost 3
  cost3<- -1*profit_hdays
  
  ##
  # cost5 Each Effects.
  #weight is normalized
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*3
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  if(isDebug){cat(" :wht2",weight)}
  cost5<- -sum((posEvalTbl$DeltaEffect+posEvalTbl$GammaEffect+
                  posEvalTbl$VegaEffect+posEvalTbl$ThetaEffect)*weight)
  if(isDebug){cat(" c5",cost5)}
  
  ##
  # total cost is weighted sum of each cost.
  cost<-(0.05*cost3+0.01*cost5+500)
  if(isDebug){cat(" :cost",cost," ")}
  
  ##
  # total cost and penalty
  val<-cost*penalty1*penalty2*penalty3*penalty4
  
  if(isDebug){cat(" val:",val,"\n")}
  
  if(isDebug){
    if(val<best_result){
      cat(x,file=result_file,sep=",",append=TRUE);cat(",",file=result_file,append=TRUE)
      cat(val,file=result_file,"\n",append=TRUE)
      #write(x,result_file,append=T)
      #write(val,result_file,append=T)
      #ou use cat which gives further details on the format used
      best_result<<-val
      #assign(best_result,val,env=.GlobalEnv)
    }
  }
  return(val)
}

obj_Income_Cont <- function(x,isDebug=FALSE,isGenoud=TRUE){
  x<-as.numeric(x<(-6.5))*(-6)+as.numeric(x>(6.5))*6+as.numeric(x>=(-6.5)&x<=6.5)*x
  
  if(sum(as.numeric(x!=0))==0){
    x<-rnorm(n=length(iniPos),mean=0,sd=1)
  }
  
  cat(x," ")
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-4; udlStepPct<-0.02
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip)
  #if(isDebug){print(posEvalTbl$pos)}
  #if(isDebug){print(posEvalTbl)}
  
  ##
  # penalty1 cost1: position total num
  #   pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
  #   penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))
  #   cost1<-(penalty1-1)*3
  #   
  #   if(isDebug){cat(" :pos num",pos_change);cat(" :cost1",cost1)}
  
  ##
  # penalty4. ThetaEffect. This should be soft constraint
  
  #   sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*2
  #   weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #   theta_ttl<-thePositionGrk$ThetaEffect+sum(posEvalTbl$ThetaEffect*weight)
  #   
  #   exp_c<-as.numeric((pos_change-10)>0)*0+as.numeric((pos_change-10)<=0)
  #   penalty4<-(1+as.numeric(theta_ttl<0)*(abs(theta_ttl)))^exp_c
  #   
  #   if(isDebug){cat(" :thta_ttl",theta_ttl);cat(" :p4",penalty4)}
  
  ##
  # penalty2 tail-risk
  tail_rate<-0.4
  tailPrice<-min(sum(getIntrisicValue(position$UDLY[1]*(1-tail_rate),position)),
                 sum(getIntrisicValue(position$UDLY[1]*(1+tail_rate),position)))
  #lossLimitPrice <- -1*position$UDLY[1]*PosMultip*(tail_rate+0.1)
  lossLimitPrice <- -20000
  exp_c<-1
  penalty2<-(1+as.numeric((tailPrice-lossLimitPrice)<0)*(10))^exp_c
  
  # cost 2
  cost2<- -1*as.numeric(penalty2>1)*(tailPrice-lossLimitPrice)
  if(isDebug){cat(" :tlpr",tailPrice);cat(" :lslmt",lossLimitPrice);cat(" p2:",penalty2);cat(" :c2",cost2)}
  
  ##
  # penalty3, cost1 profit must be positive. also must be a cost term.
  
  if(isDebug){cat(" :prc_hd",posEvalTbl$Price);cat(" :prc_ini:",getPositionGreeks(position,multi=PosMultip)$Price)}
  if(isDebug){cat(" :prft",posEvalTbl$Price-getPositionGreeks(position,multi=PosMultip)$Price)}
  
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht",weight)}
  
  #exp_c<-as.numeric(exp_c>0)*(as.numeric((penalty3-2)>0)*0+as.numeric((penalty3-2)<0)*2)
  profit_hdays<-sum((posEvalTbl$Price-thePositionGrk$Price)*weight)
  
  if(isDebug){cat(" :prft_wt",profit_hdays)}  
  #   penalty3<-(1+as.numeric(profit_hdays<0)*(abs(profit_hdays)))^exp_c
  #   penalty3<-1
  #  if(isDebug){cat(" :p3",penalty3)}
  ##
  
  # cost 3
  cost3<- -1*profit_hdays
  if(isDebug){cat(" :c3",cost3)}
  
  ##
  # cost5 Each Effects.
  #weight is normalized
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*3
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht2",weight)}
  
  theta_addwht<-1.5
  cost5<- -sum((posEvalTbl$DeltaEffect+posEvalTbl$GammaEffect+
                  posEvalTbl$VegaEffect+theta_addwht*posEvalTbl$ThetaEffect)*weight)
  if(isDebug){cat(" tef:",sum(posEvalTbl$ThetaEffect));cat(" c5",cost5)}
  
  ##
  # total cost is weighted sum of each cost.
  cost<-(0.05*cost3+0.01*cost5+cost2+500)
  if(isDebug){cat(" :cost",cost," ")}
  
  ##
  # total cost and penalty
  #val<-cost*penalty1*penalty2*penalty3*penalty4
  val<-cost
  
  if(isDebug){cat(" val:",val,"\n")}
  
  if(isDebug){
    if(val<best_result){
      write(x,result_file,append=T)
      write(val,result_file,append=T)
      #ou use cat which gives further details on the format used
      best_result<<-val
      #assign(best_result,val,env=.GlobalEnv)
    }
  }
  return(val)
}

obj_Income_genoud_lex_int <- function(x,isDebug=FALSE){
  #x<-round(x)
  if(sum(as.numeric(round(x)!=0))==0){
    x<-rep(1,length=length(x))
    x<-round(x)
  }
  cat(x,"\n")
  position<-hollowNonZeroPosition(pos=x)
  
  udlStepNum<-3; udlStepPct<-0.03
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  
  #if(isDebug){print(posEvalTbl$pos)}
  #if(isDebug){print(posEvalTbl)}
  
  #penalty1: position total num
  pos_change<-sum(as.numeric((round(x)-iniPos)!=0))
  penalty1<-(1+as.numeric((pos_change-10)>0)*(pos_change-10))^5
  cat("pos num",pos_change)
  if(isDebug){cat(" :p1",penalty1)}
  
  #penalty2 tail-risk
  tail_rate<-0.5
  tailPrice<-min(sum(getIntrisicValue(position$UDLY[1]*(1-tail_rate),position)),
                 sum(getIntrisicValue(position$UDLY[1]*(1+tail_rate),position)))
  lossLimitPrice <- -1*position$UDLY[1]*PosMultip*(tail_rate+0.1)
  penalty2<-(1+as.numeric((tailPrice-lossLimitPrice)<0)*(abs(tailPrice)))
  if(isDebug){cat(" :tlpr",tailPrice);cat(" :lslmt",lossLimitPrice);cat(" :p2",penalty2)}
  
  #penalty3, cost1 profit must be positive. also must be a cost term.
  #if(isDebug){cat(" :prc_hd",posEvalTbl$Price);cat(" :prc_ini:",getPositionGreeks(position,multi=PosMultip)$Price)}
  #if(isDebug){cat(" :prft",posEvalTbl$Price-getPositionGreeks(position,multi=PosMultip)$Price)}
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip)
  
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht",weight)}
  
  profit_hdays<-sum((posEvalTbl$Price-thePositionGrk$Price)*weight)
  if(isDebug){cat(" :prft_wt",profit_hdays)}
  penalty3<-(1+as.numeric(profit_hdays<0)*(abs(profit_hdays)))^2
  if(isDebug){cat(" :p3",penalty3)}
  cost1<- -1*profit_hdays
  
  #cost2 Each Effects.  
  #weight is normalized
  sd_multp<-holdDays;anlzd_sd<-0.2;sd_hd<-(anlzd_sd/sqrt(252/sd_multp))*3
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  #if(isDebug){cat(" :wht2",weight)}  
  cost2<--sum(posEvalTbl$DTRRR*weight+posEvalTbl$VTRRR*weight)
  if(isDebug){cat(" c2",cost2)}
  
  #penalty4. ThetaEffect. This should be soft constraint
  #print(thePositionGrk)
  theta_ttl<-thePositionGrk$ThetaEffect+sum(posEvalTbl$ThetaEffect*weight)
  penalty4<-(1+as.numeric(theta_ttl<0)*(abs(theta_ttl)))^2
  if(isDebug){cat(" :thta_ttl",theta_ttl);cat(" :p4",penalty4)}
  #cat(" :thta_ini",thePositionGrk$ThetaEffect);cat(" :thta_hld",sum(posEvalTbl$ThetaEffect*weight))
  
  #total cost is weighted sum of each cost.
  cost<-(0.03*cost1+cost2+15)*penalty4
  if(isDebug){cat(" :cost",cost," ")}
  
  #non lex cost function should be like this.
  #val<-grkeval*penalty1*penalty2*penalty3
  val<-c(penalty1,penalty2,penalty3,cost)
  
  if(isDebug){cat(" val:",val,"\n")}
  return(val)
}

#used for post-evaluation
getProfit <- function(x,isDebug=TRUE,udlStepNum=4,udlStepPct=0.02){
  #position where pos$Position != 0
  position<-hollowNonZeroPosition(pos=x)
  #position evaluated after holdDays later
  udlStepNum<-udlStepNum; udlStepPct<-udlStepPct
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-createPositinEvalTable(position=position,udlStepNum=udlStepNum,udlStepPct=udlStepPct)
  #At day 0 position price and Greeks.
  thePositionGrk<-getPositionGreeks(position,multi=PosMultip)
  if(isDebug){print(posEvalTbl$pos)}
  if(isDebug){print(posEvalTbl)}  
  
  ##
  # Profit
  if(isDebug){cat(" :prc_hd",posEvalTbl$Price);cat(" :prc_ini:",getPositionGreeks(position,multi=PosMultip)$Price)}
  if(isDebug){cat(" :prft",posEvalTbl$Price-getPositionGreeks(position,multi=PosMultip)$Price)}
  
  sd_multp<-holdDays;anlzd_sd<-getIV_td(histIV$IVIDX[1]);sd_hd<-(anlzd_sd/sqrt(252/sd_multp))
  weight<-dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd / sum(dnorm(udlChgPct,mean=0,sd=sd_hd)*sd_hd)
  if(isDebug){cat(" :wht",weight)} 
  profit_hdays<-sum((posEvalTbl$Price-thePositionGrk$Price)*weight)
  if(isDebug){cat(" :prft_wt",profit_hdays,"\n")}  
  
  return (as.numeric(profit_hdays))
}

##
# Functions to be loaded -------------------

#Rsk/Rtn greek related functions --------------
#get the position's total greek
getPosGreeks<-function(pos,greek,multi=PosMultip){
  pos_greek<-sum(pos*multi*greek)
  pos_greek
}

getIV_td<-function(ividx_cd){
  ividx_td <- ividx_cd*sqrt(365/252)
  ividx_td
}

getThetaEffect<-function(pos,greek,multi=PosMultip,hdd=holdDays){
  theta<-getPosGreeks(pos=pos,greek=greek)
  thetaEfct<-holdDays*theta
  thetaEfct
}

getDeltaEffect<-function(pos,greek,UDLY,ividx_td,multi=PosMultip,hdd=holdDays){
  expPriceChange<-mean(UDLY*(exp(ividx_td*sqrt(hdd/365))-1))
  delta<-getPosGreeks(pos=pos,greek=greek)
  deltaEfct<-(-abs(delta))*expPriceChange
  deltaEfct
}

getGammaEffect<-function(pos,greek,UDLY,ividx_td,multi=PosMultip,hdd=holdDays){
  expPriceChange<-mean(UDLY*(exp(ividx_td*sqrt(hdd/365))-1))
  gamma<-getPosGreeks(pos=pos,greek=greek)
  gammaEfct<-gamma*(expPriceChange^2)/2
  gammaEfct
}

#Here we do not care the effect of Volga as we did the Gamma effect, is this really appropriate?
getVegaEffect<-function(pos,greek,ividx,dviv,multi=PosMultip,hdd=holdDays){
  expIVChange<-mean(ividx*(exp(dviv*sqrt(holdDays))-1))
  #Or use Annualized Volatility of Impled Volatility. Should be the same result.
  #  aviv<-annuual.daily.volatility(histIV$IVIDX)$anlzd*sqrt(holdDays/252)
  #  expIVChange<-mean(ividx*(exp(aviv)-1))
  vega<-getPosGreeks(pos=pos,greek=greek)
  vegaEffect<-(-abs(vega))*(expIVChange*100)
  vegaEffect
}

getDTRRR<-function(position,multi=PosMultip,hdd=holdDays){
  thetaEffect<-getThetaEffect(pos=position$Position,greek=position$Theta)
  
  deltaEffect<-getDeltaEffect(pos=position$Position,greek=position$Delta,
                              UDLY=position$UDLY,ividx_td=getIV_td(position$IVIDX))
  
  gammaEffect<-getGammaEffect(pos=position$Position,greek=position$Gamma,
                              UDLY=position$UDLY,ividx_td=getIV_td(position$IVIDX)) 
  
  #DTRRR<-(deltaEffect+gammaEffect)/thetaEffect
  #DTRRR<- exp(deltaEffect+gammaEffect)*exp(thetaEffect)
  thetaEffect<-as.numeric(thetaEffect!=0)*thetaEffect+as.numeric(thetaEffect==0)*0.001
  DTRRR<- (deltaEffect+gammaEffect+(0.5*thetaEffect))/abs(thetaEffect)
  DTRRR
}

getVTRRR<-function(position,ividx,dviv,multi=PosMultip,hdd=holdDays){
  thetaEffect<-getThetaEffect(pos=position$Position,greek=position$Theta)
  #thetaEffect<-as.numeric(thetaEffect>0)*thetaEffect+as.numeric(thetaEffect<0)*(0.001)+as.numeric(thetaEffect==0)*0.001
  
  vegaEffect<-getVegaEffect(pos=position$Position,greek=position$Vega,
                            ividx=ividx,dviv=dviv)
  #VTRRR<-vegaEffect/thetaEffect
  #VTRRR<- exp(vegaEffect)*exp(-1*thetaEffect)
  
  thetaEffect<-as.numeric(thetaEffect!=0)*thetaEffect+as.numeric(thetaEffect==0)*0.001
  VTRRR<- (vegaEffect+(0.5*thetaEffect))/abs(thetaEffect)
  VTRRR
}

#Rsk/Rtn Scenario functions -----------------------------
#Factory of Volatility Level Regression Result
get.Volatility.Level.Regression<-function(Days=holdDays,ctoc=TRUE){
  if((!ctoc)*(Days==1)){
    return (PC1dCtO_IVCF1dCtO)
  }else if(ctoc*(Days==1)){
    return(PC1dCtC_IVCF1dCtC)
  } else if(ctoc*(Days==3)){
    return(PC3dCtC_IVCF3dCtC)
  }else if(ctoc*(Days==5)){
    return(PC5dCtC_IVCF5dCtC)
  }else if(ctoc*(Days==7)){
    return(PC7dCtC_IVCF7dCtC)
  }
}

#read from get.Volatility.Change.Regression.Result to get specific regression values.
get.VolChg<-function(model,month){
  chg<-predict(model,x=month)
  return(chg)
}

#get the ATMIV_pos/ATMIV_pre (Result of Vchg regression) as a vector for each position element
get.Volatility.Change.Regression.Result<-function(pos,up_dn){
  atmiv_chg<-           (pos$TYPE==OpType_Put_G)*(up_dn>=0)*(get.VolChg(model=PutIVChgUp,month=pos$TimeToExpDate))$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Call_G)*(up_dn>=0)*(get.VolChg(model=CallIVChgUp,month=pos$TimeToExpDate))$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Put_G)*(up_dn<0)*(get.VolChg(model=PutIVChgDown,month=pos$TimeToExpDate))$y
  atmiv_chg<-atmiv_chg+(pos$TYPE==OpType_Call_G)*(up_dn<0)*(get.VolChg(model=CallIVChgDown,month=pos$TimeToExpDate))$y
  atmiv_chg
}

#read from et.Volatility.Cone.Regression.Result to get specific regression values.
get.VCone<-function(model,month){
  cone<-predict(model,x=month)
  return(cone)
}

#get the ATMIV_pos/ATMIV_pre (Result of Vchg regression) as a vector for each position element
get.Volatility.Cone.Regression.Result<-function(optype,month){
  cone<-(optype==OpType_Put_G)*(get.VCone(model=PutVCone,month=month))$y
  cone<-cone+(optype==OpType_Call_G)*(get.VCone(model=CallVCone,month=month))$y
  cone
}

#when we get the Future Option, we must consider spot change and
#their forward(future) prices. here just treat stock or other (not future) option
get.UDLY.Changed.Price<-function(udly,chg_pct){
  change<-udly*chg_pct
  change
}

#functions for gruped or rowwise operations. -------------

#evalPosRskRtnXXX is just wrappers for Rsk/Rtn greek related functions.
evalPosRskRtnDTRRR<- function(pos_eval){
  pos_DTRRR<-pos_eval$pos
  dtrrr<-getDTRRR(position=pos_DTRRR)
  dtrrr
}

evalPosRskRtnVTRRR<- function(pos_eval){
  pos_DTRRR<-pos_eval$pos
  #  print(pos_DTRRR)
  #dviv should be pre-calculated when optimize
  vtrrr<-getVTRRR(position=pos_DTRRR,ividx=getIV_td(pos_DTRRR$IVIDX),dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)
  vtrrr
}

evalPosRskRtnThetaEffect<- function(pos_eval){
  pos<-pos_eval$pos
  #  print(pos)
  thetaEffect<-getThetaEffect(pos=pos$Position,greek=pos$Theta)
  thetaEffect
}

evalPosRskRtnDeltaEffect<- function(pos_eval){
  pos<-pos_eval$pos
  #  print(pos)
  deltaEffect<-getDeltaEffect(pos=pos$Position,greek=pos$Delta,
                              UDLY=pos$UDLY,ividx_td=getIV_td(pos$IVIDX))
  deltaEffect
}

evalPosRskRtnGammaEffect<- function(pos_eval){
  pos<-pos_eval$pos
  #  print(pos)
  gammaEffect<-getGammaEffect(pos=pos$Position,greek=pos$Gamma,
                              UDLY=pos$UDLY,ividx_td=getIV_td(pos$IVIDX))
  gammaEffect
}

evalPosRskRtnVegaEffect<- function(pos_eval){
  pos<-pos_eval$pos
  #  print(pos)
  #dviv should be pre-calculated when optimize
  vegaEffect<-getVegaEffect(pos=pos$Position,greek=pos$Vega,
                            ividx=getIV_td(pos$IVIDX),dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)
  vegaEffect
}

# operate to each position data frame based on scenaro changes
reflectPosChg<- function(process_df,days=holdDays){
  
  pos<-as.data.frame(process_df$pos[1])
  chg<-as.numeric(process_df$udlChgPct[1])
  # print(chg)
  
  # get (IVIDX_pre/IVIDX_pos)/(UDLY_pre/UDLY_pos)
  regression<-get.Volatility.Level.Regression(Days=days)
  ividx_chg_pct<-get.predicted.IVIDXChange(model=regression$model,xmin=chg,xmax=100,x_by=0)$IVIDXC
  pos$IVIDX<-pos$IVIDX*(1+ividx_chg_pct)
  
  # ATM IV change
  pos$ATMIV<-pos$ATMIV*(1+ividx_chg_pct)*get.Volatility.Change.Regression.Result(pos,ividx_chg_pct)
  
  #Volatility Cone の影響。時間変化した分の影響を受ける。その比の分だけ比率変化
  #ATMIV_pos <- ATMIV_pos*(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pre/(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pos
  bdays_per_month<-252/12
  TimeToExpDate_pos<-(pos$TimeToExpDate*bdays_per_month-days)/bdays_per_month
  pos$ATMIV<-pos$ATMIV *
    get.Volatility.Cone.Regression.Result(pos$TYPE,TimeToExpDate_pos)/
    get.Volatility.Cone.Regression.Result(pos$TYPE,pos$TimeToExpDate)
  
  #set new TimeToExpDate
  pos$TimeToExpDate<-TimeToExpDate_pos
  
  #Date advance
  pos$Date <- format(advance("UnitedStates/NYSE",dates=as.Date(pos$Date,format="%Y/%m/%d"),
                             days,0),"%Y/%m/%d")
  
  #set new value to UDLY
  pos$UDLY <- pos$UDLY+get.UDLY.Changed.Price(udly=pos$UDLY,chg_pct=chg)
  
  #set new value to HowfarOOM, Moneyness.Nm
  pos$Moneyness.Frac<-pos$Strike/pos$UDLY
  pos$HowfarOOM<-(1-pos$Moneyness.Frac)*pos$TYPE
  
  #if TimeToExpDate < TimeToExp_Limit_Closeness_G(0.3 etc), TimeToExpDate should be TimeToExp_Limit_Closeness_G.
  #Otherwise use the TimeToExpDate values themselves.
  eval_timeToExpDate<-as.numeric(pos$TimeToExpDate<TimeToExp_Limit_Closeness_G)*TimeToExp_Limit_Closeness_G+
    as.numeric(pos$TimeToExpDate>=TimeToExp_Limit_Closeness_G)*pos$TimeToExpDate
  pos$Moneyness.Nm<-log(pos$Moneyness.Frac)/pos$ATMIV/sqrt(eval_timeToExpDate)
  pos$Moneyness.Frac<-NULL
  
  #calculate IV_pos(OrigIV) using SkewModel based on model definition formula.
  get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
  pos$ATMIV*get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
  pos$OrigIV<-pos$ATMIV*get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
  
  #calculate pption price and thier greeks
  vgreeks<-set.EuropeanOptionValueGreeks(pos)
  pos$Price<-vgreeks$Price
  pos$Delta<-vgreeks$Delta
  pos$Gamma<-vgreeks$Gamma
  pos$Vega<-vgreeks$Vega
  pos$Theta<-vgreeks$Theta
  pos$Rho<-vgreeks$Rho
  
  pos
}

hollowNonZeroPosition<-function(pos){
  #opchain is global parameter. to avoid unnessary copying
  opchain$Position<-pos
  opchain %>% dplyr::filter(Position!=0) -> position
  position
}

createPositinEvalTable<-function(position,udlStepNum=3,udlStepPct=0.03,days=holdDays){
  udlChgPct<-seq(-udlStepPct*udlStepNum,udlStepPct*udlStepNum,length=(2*udlStepNum)+1)
  posEvalTbl<-data.frame(udlChgPct=udlChgPct) ;rm(udlStepNum,udlStepPct)
  #Set data frames as a row value of another data frame.
  posEvalTbl %>% group_by(udlChgPct) %>% do(pos=position) -> posEvalTbl
  #Modify pos based on scenario
  posEvalTbl %>% group_by(udlChgPct) %>% do(pos=reflectPosChg(.,days)) -> posEvalTbl
  #DTRRR
  #posEvalTbl %>% rowwise() %>% do(DTRRR=evalPosRskRtnDTRRR(.)) -> tmp
  #unlist(tmp$DTRRR)->tmp ; posEvalTbl$DTRRR <- tmp ;rm(tmp)
  #VTRRR
  #posEvalTbl %>% rowwise() %>% do(VTRRR=evalPosRskRtnVTRRR(.)) -> tmp
  #unlist(tmp$VTRRR)->tmp ; posEvalTbl$VTRRR <- tmp ;rm(tmp)
  
  #debugging(Just for Info ) purpose. when optimized, not necessary.
  ##
  #  Greek Effects
  
  #  ThetaEffect
  posEvalTbl %>% rowwise() %>% do(ThetaEffect=getThetaEffect(pos=.$pos$Position,greek=.$pos$Theta)) -> tmp
  unlist(tmp$ThetaEffect)->tmp ; posEvalTbl$ThetaEffect <- tmp ;rm(tmp)
  #  DeltaEffect
  posEvalTbl %>% rowwise() %>% do(DeltaEffect=getDeltaEffect(pos=.$pos$Position,greek=.$pos$Delta,
                                                             UDLY=.$pos$UDLY,
                                                             ividx_td=getIV_td(.$pos$IVIDX))) -> tmp
  unlist(tmp$DeltaEffect)->tmp ; posEvalTbl$DeltaEffect <- tmp ;rm(tmp)
  #  GammaEffect
  posEvalTbl %>% rowwise() %>% do(GammaEffect=getGammaEffect(pos=.$pos$Position,greek=.$pos$Gamma,
                                                             UDLY=.$pos$UDLY,
                                                             ividx_td=getIV_td(.$pos$IVIDX))) -> tmp
  unlist(tmp$GammaEffect)->tmp ; posEvalTbl$GammaEffect <- tmp ;rm(tmp) 
  #  VegaEffect
  posEvalTbl %>% rowwise() %>% do(VegaEffect=getVegaEffect(pos=.$pos$Position,greek=.$pos$Vega,
                                                           ividx=getIV_td(.$pos$IVIDX)
                                                           #dviv should be precalulated when optimized
                                                           ,dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)) -> tmp
  unlist(tmp$VegaEffect)->tmp ; posEvalTbl$VegaEffect <- tmp ;rm(tmp)
  
  ##
  #  Greeks
  #
  #  UDLY
  posEvalTbl %>% rowwise() %>% do(UDLY=mean(.$pos$UDLY)) ->tmp
  unlist(tmp$UDLY)->tmp ; posEvalTbl$UDLY <- tmp ;rm(tmp)
  #  Price
  posEvalTbl %>% rowwise() %>% do(Price=getPosGreeks(pos=.$pos$Position,greek=.$pos$Price)) ->tmp
  unlist(tmp$Price)->tmp ; posEvalTbl$Price <- tmp ;rm(tmp)
  #  Delta
  posEvalTbl %>% rowwise() %>% do(Delta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Delta))->tmp
  unlist(tmp$Delta)->tmp ; posEvalTbl$Delta <- tmp ;rm(tmp)
  #  Gamma
  posEvalTbl %>% rowwise() %>% do(Gamma=getPosGreeks(pos=.$pos$Position,greek=.$pos$Gamma))->tmp
  unlist(tmp$Gamma)->tmp ; posEvalTbl$Gamma <- tmp ;rm(tmp)
  #  Vega
  posEvalTbl %>% rowwise() %>% do(Vega=getPosGreeks(pos=.$pos$Position,greek=.$pos$Vega))->tmp
  unlist(tmp$Vega)->tmp ; posEvalTbl$Vega <- tmp ;rm(tmp)
  #  Theta
  posEvalTbl %>% rowwise() %>% do(Theta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Theta)) ->tmp
  unlist(tmp$Theta)->tmp ; posEvalTbl$Theta <- tmp ;rm(tmp)
  
  posEvalTbl
  
}

#posgrks can be obtained by hollowNonZeroPosition(evaPos)
#return vector. If you like to get aggrigate payoff, use sum()
getIntrisicValue<-function(udly_price,position,multip=PosMultip){
  as.numeric(((udly_price-position$Strike)*(-position$TYPE)>0))*
    (udly_price-position$Strike)*(-position$TYPE)*multip*position$Position
}

#functions for initial polulation creating  --------

##
# Creating initial candidate population of spread positions whose componets of each position are exactly spcicified by the arguments.
# if the number (putn or calln) is even number, half of the spread`s positions are assigned +1(long), the other half -1(short).
# if odd number, the first 3 positions are assigned as -1 +2 -1 to form a butterfly, the rest position are assigned in the same way
# as the case of even number (half +1 long, the other half -1 short).
# When the each position of returned compound spread is 1 or -1, the spread position are multiplied by ml. 

create_initial_exact_PutCall_polulation<-function(popnum,type,thresh=3.0,putn=6,calln=6,ml=2,fname,isFileout=FALSE,isDebug=FALSE){
  added_num<-0
  total_count<-0
  while(TRUE){
    #Put   
    idxy<-as.numeric(type==OpType_Put_G)*rep(1:length(iniPos),length=length(iniPos))
    if(isDebug){ cat(" put pos:",idxy) }
    idxy<-idxy[idxy!=0]
    if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=putn,replace=FALSE,prob=NULL)
    if(isDebug){ cat(" put idxy:",idxy) }
    y<-rep(0,times=length(iniPos))
    y_shift<-0
    putn_shift<-putn
    if((putn%%2)!=0){
      y[idxy[1:2]]<-(-1)
      y[idxy[3]]<-2
      putn_shift<-putn-3
      y_shift<-3
    }
    if(putn_shift>=2){
      y[idxy[(1+y_shift):(y_shift+(putn_shift/2))]]<-1
      y[idxy[(putn_shift/2+1+y_shift):putn]]<-(-1)
    }
    #Call
    idxy<-as.numeric(type==OpType_Call_G)*rep(1:length(iniPos),length=length(iniPos))
    idxy<-idxy[idxy!=0]
    idxy<-sample(idxy,size=calln,replace=FALSE,prob=NULL)
    if(isDebug){ cat(" call idxy:",idxy) }
    z<-rep(0,times=length(iniPos))
    z_shift<-0
    calln_shift<-calln
    if((calln%%2)!=0){
      z[idxy[1:2]]<-(-1)
      z[idxy[3]]<-2
      calln_shift<-calln-3
      z_shift<-3
    }
    if(calln_shift>=2){    
      z[idxy[1+z_shift:(z_shift+(calln_shift/2))]]<-1
      z[idxy[(calln_shift/2+1+z_shift):calln]]<-(-1)
    }
    if(isDebug){ cat(" (:y",y,")") }
    if(isDebug){ cat(" (:z",z,") :x(y+z) ") }
    x<-y+z
    x<-as.numeric(((putn%%2)==0)*((calln%%2)==0))*ml*x+as.numeric(!((putn%%2)==0)*((calln%%2)==0))*x
    #val<-obj_Income(x,isDebug=isDebug)
    val<-obj_Income_sgmd(x,isDebug=isDebug,isDetail=isDebug)
    if(val<thresh){
      if(added_num==0){
        ret_val<-x
      }else{
        ret_val<-c(ret_val,x)
      }
      added_num<-added_num+1
      if(isFileout){
        cat(x,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
        cat(val,file=fname,"\n",append=TRUE)
      }
    }
    total_count<-total_count+1
    if(((added_num%%30)==0)){cat(" added num:",added_num,"total count:",total_count,"\n")}
    if(added_num==popnum)
      break
  }
  cat(" added num:",added_num,"total count:",total_count,"\n")
  ret_val
}

#function for creating one candidate pool --------------
# nrows are rows to be read from a file, skip indicates how many rows should be skipped.
# if pnum==0, then max nrows are sampled.
createCombineCandidatePool<-function(fname,pnum=1000,nrows=-1,skip=0,method=1){
  pool<-read.csv(fname, header=FALSE,nrows=nrows,skip=skip)
  pnum<-as.numeric((pnum==0))*nrow(pool)+as.numeric((pnum!=0))*pnum
  pool %>% dplyr::arrange(pool[,(length(iniPos)+1)]) %>% dplyr::distinct() -> pool
  
  
  #select specific nums. some optional methods
  #1.top n
  if(method==1){
    pool<-pool[1:pnum,]
  }
  #2. random sample
  else if(method==2){
    idx<-rep(1:nrow(pool),length=nrow(pool))
    idx<-sort(sample(idx,size=pnum,replace=FALSE,prob=NULL))
    pool<-pool[idx,]
    rownames(pool) <- c(1:nrow(pool))
  }
  #3. bottom n
  else if(method==3){
    pool<-pool[(nrow(pool)-pnum+1):nrow(pool),]
    rownames(pool) <- c(1:nrow(pool))
  }
  pool[complete.cases(pool),] -> pool
  return(pool)
}

#function for seraching candidate by combination ------------

# two sample examples. one from pools[[2]], the other from pools[[3]]
#ceiling(runif(1, min=1e-320, max=nrow(pools[[2]][[2]])))
create_combined_population<-function(popnum,thresh=1000,plelem=c(4,5),fname,isFileout=FALSE,isDebug=FALSE,maxposn=8){
  added_num<-0
  total_count<-0
  while(TRUE) {
    s1<-pools[[ plelem[1] ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[ plelem[1] ]][[2]]))), ]
    s1_pos<-unlist(s1[1:length(iniPos)]);s1_score<-as.numeric(s1[length(s1)])
    if(isDebug){cat("s1 :",s1_pos," sc:",s1_score)   }
    
    s2<-pools[[ plelem[2] ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[  plelem[2] ]][[2]]))), ]
    s2_pos<-unlist(s2[1:length(iniPos)]);s2_score<-as.numeric(s2[length(s2)])
    if(isDebug){cat(" s2 :",s2_pos," sc:",s2_score)   }
    
    s3_pos<-rep(0,times=length(iniPos))
    if(length(plelem)==3){
      s3<-pools[[ plelem[3] ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[  plelem[3] ]][[2]]))), ]
      s3_pos<-unlist(s3[1:length(iniPos)]);s3_score<-as.numeric(s3[length(s3)])
      if(isDebug){cat(" s3 :",s3_pos," sc:",s3_score)   }
    }
    
    x_new<-rep(0,times=length(iniPos))
    x_new<-s1_pos+s2_pos+s3_pos
    x_new<-as.numeric((sum(x_new%%7)!=0))*x_new+as.numeric((sum(x_new%%7)==0))*x_new/7
    x_new<-as.numeric((sum(x_new%%5)!=0))*x_new+as.numeric((sum(x_new%%5)==0))*x_new/5
    x_new<-as.numeric((sum(x_new%%4)!=0))*x_new+as.numeric((sum(x_new%%4)==0))*x_new/4
    x_new<-as.numeric((sum(x_new%%3)!=0))*x_new+as.numeric((sum(x_new%%3)==0))*x_new/3
    x_new<-as.numeric((sum(x_new%%2)!=0))*x_new+as.numeric((sum(x_new%%2)==0))*x_new/2
    x_new<-as.numeric((max(abs(x_new))==1))*x_new*2+as.numeric((max(abs(x_new))!=1))*x_new
    if(isDebug){ cat(" x_new :",x_new) }
    total_count<-total_count+1
    
    if(sum(as.numeric((x_new-iniPos)!=0))>maxposn){
      if(isDebug){ cat("\n") }
      next
    }
    
    #evaluate
    #val<-obj_Income(x_new,isDebug=isDebug)
    val<-obj_Income_sgmd(x_new,isDebug=isDebug,isDetail=isDebug)
    if(val<thresh){
      if(added_num==0){
        ret_val<-x_new
      }else{
        ret_val<-c(ret_val,x_new)
      }
      added_num<-added_num+1
      if(isFileout){  
        cat(x_new,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE)
        cat(val,file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);
        cat(pools[[ plelem[1] ]][[1]][2:3],file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);cat(s1_score,file=fname,append=TRUE);cat(",",file=fname,append=TRUE)
        cat(pools[[ plelem[2] ]][[1]][2:3],file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);cat(s2_score,file=fname,append=TRUE)
        if(length(plelem)==3){
          cat(",",file=fname,append=TRUE)
          cat(pools[[ plelem[3] ]][[1]][2:3],file=fname,sep=",",append=TRUE);cat(",",file=fname,append=TRUE);cat(s3_score,file=fname,append=TRUE)
        }
        cat("\n",file=fname,append=TRUE)
      }
    }
    if(((added_num%%30)==0)){cat(" added num:",added_num,"total count:",total_count,"\n")}
    if(added_num==popnum)
      break
  }
}

