library(RQuantLib)
library(ggplot2)
library(dplyr)
library(pracma)
library(digest)
library(hash)
library(gsl)
library(PearsonDS)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

##Global Variables
xDayInt=18
UdlStepNum=15
ADJUST_DENSITY_MINIMUM=1/5000

#shift_right

LocalVectorShiftRight <- function(x_vec,fill_v=0){
  x_vec=replace(x_vec, rep(2:(length(x_vec))), x_vec[1:length(x_vec)-1])
  x_vec[1]=fill_v
  return(x_vec)
}

LocalVectorShiftLeft <- function(x_vec,fill_v=1){
  x_vec=replace(x_vec, rep(1:length(x_vec)-1), x_vec[2:length(x_vec)])
  x_vec[length(x_vec)]=fill_v
  return(x_vec)
}


LocalNearValueToExactValue <- function(x_vec,exct_value){
  if(length(which(x_vec==exct_value))==0){
    tmp_new_idx=which.min(abs(x_vec-exct_value))
    x_vec[tmp_new_idx]=exct_value
  } 
  return(x_vec)
}


#Data Num.
DATA_NUM=252*15 # about x years equivalent 

#read data file
histPrc<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep=""),header=T,stringsAsFactors=F,sep=",")
histIV<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep=""),header=T,stringsAsFactors=F,sep=",")

#inner joing for modify data inconsistency
dplyr::inner_join(histPrc, histIV, by = "Date")->injoinPrcIV
histPrc=injoinPrcIV$Close.x
histIV=injoinPrcIV$Close.y
Date=injoinPrcIV$Date

fname=paste(DataFiles_Path_G,Underying_Symbol_G,"_Weight_Calc.txt",sep="")
cat(file=fname,"####### creating ",fname,"\n\n",append=F)

#selective parmeters For SPX
IS_SELECTIVE_HISTIV_REGR=T
IS_SELECTIVE_WEIGHT_ESTM=T
a_low=0.9
d_low=2
a_high=1.1
d_high=2
#Smooth Spline df
DF_P2IVREG_SSPL=5.5

#selective parmeters For RUT
if(Underying_Symbol_G=="RUT"){
  IS_SELECTIVE_HISTIV_REGR=T
  IS_SELECTIVE_WEIGHT_ESTM=T
  a_low=0.75
  d_low=3
  a_high=1.3
  d_high=3
  #Smooth Spline df
  DF_P2IVREG_SSPL=5.5
}

## conditional sampling
tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
tmp$lm
if(IS_SELECTIVE_WEIGHT_ESTM){
  selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
}

moment_org=empMoments(tmp$P2IVxd$PCxdCtC)

#moment transformation
moment_trnsfm=moment_org
#drift transformed (annualized)
dfift_trnsfm=0
moment_trnsfm["mean"]=dfift_trnsfm/(252/EvalFuncSetting$holdDays)
(moment_org)
(moment_trnsfm)

cat(file=fname,"##  moment ",append=T)
cat(file=fname,c(moment_trnsfm["mean"],moment_trnsfm["variance"],moment_trnsfm["skewness"],moment_trnsfm["kurtosis"]),"\n",sep=",",append=T)
cat(file=fname,"\n\n",append=T)

#calclate total_range
total_range=seq(-1,1.2,by=0.001)
#total_range=total_range[-which(ppearson(total_range,moments=moment_trnsfm)>=1.0)]
#total_range=total_range[-which(ppearson(total_range,moments=moment_trnsfm)<=0)]
length(total_range)
total_range

#LowerLimit,UpperLimit
LowerLimit=min(-0.2,ifelse(length(total_range[-which(ppearson(total_range,moments=moment_trnsfm)<=0)])==0,
                           min(total_range),
                           min(total_range[-which(ppearson(total_range,moments=moment_trnsfm)<=0)]) ))
UpperLimit=min(0.08,ifelse(length( total_range[-which(ppearson(total_range,moments=moment_trnsfm)>=1.0)] )==0,
                          max(total_range),
                          max(total_range[-which(ppearson(total_range,moments=moment_trnsfm)>=1.0)]) ))

##
# regular intervals
tmp_by=(UpperLimit-LowerLimit)/(UdlStepNum*2)
regular_interval_range=seq(LowerLimit,UpperLimit,by=tmp_by)
regular_interval_range
regular_interval_range=LocalNearValueToExactValue(regular_interval_range,exct_value=0)
## evaluate distribution function
tmp_by
regular_interval_range
regular_cum_table=ppearson(regular_interval_range,moments=moment_trnsfm)
#regular_dn_table=regular_cum_table-LocalVectorShiftRight(regular_cum_table)
regular_dn_table=dpearson(regular_interval_range,moments=moment_trnsfm)
regular_dn_table=regular_dn_table/sum(regular_dn_table)
regular_cum_table
#regular_dn_table
#sum(regular_dn_table)
regular_dn_table
sum(regular_dn_table)
#writ to the file
cat(file=fname,"####### regular interval ",fname,"xDayInt:",xDayInt,"\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,regular_interval_range,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,regular_dn_table,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,regular_cum_table,sep="$",append=T);cat(file=fname,")","\n\n",append=T)


##
# equal % of cumulitive probablity

limit_range=seq(LowerLimit,UpperLimit,by=0.001)
limit_cum_table=ppearson(limit_range,moments=moment_trnsfm)
limit_range
limit_cum_table
limit_dn_table=limit_cum_table-LocalVectorShiftRight(limit_cum_table)
limit_dn_table

#tmp_percent=(1-limit_dn_table[1]-limit_dn_table[length(limit_dn_table)])/(UdlStepNum*2)
tmp_percent=(1-limit_dn_table[1])/(UdlStepNum*2)

while(TRUE){
  equal_percent_idx=rep(1,length = UdlStepNum*2+1)
  idx=1
  for(i in 1:(UdlStepNum*2)){
    if(max(limit_cum_table-limit_cum_table[idx])>tmp_percent){
      idx=min(which((limit_cum_table-limit_cum_table[idx])>=tmp_percent))
      #idx=max(which((limit_cum_table-limit_cum_table[idx])<=tmp_percent))
    }else{
      idx=length(limit_cum_table)-1
    }
    #print(idx)
    #print(limit_cum_table[idx])
    equal_percent_idx[i+1]=idx
  }
  #equal_percent_idx[length(equal_percent_idx)]=length(limit_cum_table)
  equal_percent_idx
  if(max(duplicated(equal_percent_idx))){
    tmp_percent=tmp_percent*0.99
    next
  }
  break
}

equal_percent_idx
limit_range[equal_percent_idx]
equal_percent_idx=LocalNearValueToExactValue(equal_percent_idx,exct_value=which(limit_range==0))
equal_percent_idx
limit_range[equal_percent_idx]
est_density=dpearson(limit_range[equal_percent_idx],moments=moment_trnsfm)
est_density=est_density/sum(est_density)
est_density
#if min(est_density)==0 then set them small enough values.
est_density[which(est_density==0)]=1.0e-30
est_density=est_density/sum(est_density)
est_density
sum(est_density)
cumsum(est_density)
#writ to the file
cat(file=fname,"####### equal percent ",fname,"xDayInt:",xDayInt,"\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,limit_range[equal_percent_idx],sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,est_density,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,cumsum(est_density),sep="$",append=T);cat(file=fname,")","\n\n",append=T)


##
# adjusted weight of cumulitive probablity
tmp_percent=(1-limit_dn_table[1])/(UdlStepNum*2)
min_percent=ADJUST_DENSITY_MINIMUM*2
f_adj=1
while(TRUE){
  adj_percent_idx=rep(1,length = UdlStepNum*2+1)
  idx=1
  for(i in 1:(UdlStepNum*2)){
    weight_factor=max(limit_dn_table[idx]*length(limit_dn_table)*f_adj,min_percent/tmp_percent)
    if(max(limit_cum_table-limit_cum_table[idx])>(tmp_percent*weight_factor)){
      idx=min(which((limit_cum_table-limit_cum_table[idx])>=(tmp_percent*weight_factor)))
    }else{
      idx=length(limit_cum_table)-1
    }
    #print(idx)
    #print(limit_cum_table[idx])
    adj_percent_idx[i+1]=idx
  }
  #adj_percent_idx[length(adj_percent_idx)]=length(limit_cum_table)
  adj_percent_idx
  if(max(duplicated(adj_percent_idx))){
    tmp_percent=tmp_percent*0.99
    next
  }#else if(min((limit_dn_table[adj_percent_idx]/sum(limit_dn_table[adj_percent_idx]))[2],
  #(limit_dn_table[adj_percent_idx]/sum(limit_dn_table[adj_percent_idx]))[length(adj_percent_idx)-1])<ADJUST_DENSITY_MINIMUM){
  else if(min(
    (dpearson(limit_range[adj_percent_idx],moments=moment_trnsfm)/sum(dpearson(limit_range[adj_percent_idx],moments=moment_trnsfm)))[2],
    (dpearson(limit_range[adj_percent_idx],moments=moment_trnsfm)/sum(dpearson(limit_range[adj_percent_idx],moments=moment_trnsfm)))[length(adj_percent_idx)-1]
  ) <ADJUST_DENSITY_MINIMUM){
    min_percent=min_percent*1.1
    next
  }
  break
}

adj_percent_idx
limit_range[adj_percent_idx]
adj_percent_idx=LocalNearValueToExactValue(adj_percent_idx,exct_value=which(limit_range==0))
adj_percent_idx
limit_range[adj_percent_idx]
est_density=dpearson(limit_range[adj_percent_idx],moments=moment_trnsfm)
est_density=est_density/sum(est_density)
est_density
#if min(est_density)==0 then set them small enough values.
est_density[which(est_density==0)]=1.0e-30
est_density=est_density/sum(est_density)
est_density
sum(est_density)
cumsum(est_density)
#write to the file
cat(file=fname,"####### adjusted weight ",fname,"xDayInt:",xDayInt,"\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,limit_range[adj_percent_idx],sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,est_density,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,cumsum(est_density),sep="$",append=T);cat(file=fname,")","\n\n",append=T)

####
##    day 1 calculation

tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,1)
tmp$lm
if(IS_SELECTIVE_WEIGHT_ESTM){
  selectSuffixForValidIV(histIV,1,a_low,d_low,a_high,d_high)->suffix_slctd
  tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,1,effectiv_suffix=suffix_slctd)
}

moment_org=empMoments(tmp$P2IVxd$PCxdCtC)
#moment transformation
moment_trnsfm=moment_org
#drift transformed (annualized)
dfift_trnsfm=0
moment_trnsfm["mean"]=dfift_trnsfm/(252/EvalFuncSetting$holdDays)
(moment_org)
(moment_trnsfm)

cat(file=fname,"##  moment ",append=T)
cat(file=fname,c(moment_trnsfm["mean"],moment_trnsfm["variance"],moment_trnsfm["skewness"],moment_trnsfm["kurtosis"]),"\n",sep=",",append=T)
cat(file=fname,"\n\n",append=T)

##
# regular interval
est_density=dpearson(regular_interval_range,moments=moment_trnsfm)
est_density=est_density/sum(est_density)
est_density
#if min(est_density)==0 then set them small enough values.
est_density[which(est_density==0)]=1.0e-30
est_density=est_density/sum(est_density)
est_density
#writ to the file
cat(file=fname,"####### regular interval ",fname,"xDayInt: 1 \n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,regular_interval_range,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,est_density,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,cumsum(est_density),sep="$",append=T);cat(file=fname,")","\n\n",append=T)

##
# equal % of cumulitive probablity
est_density=dpearson(limit_range[equal_percent_idx],moments=moment_trnsfm)
est_density=est_density/sum(est_density)
est_density
#if min(est_density)==0 then set them small enough values.
est_density[which(est_density==0)]=1.0e-30
est_density=est_density/sum(est_density)
est_density
#writ to the file
cat(file=fname,"#######  equal percent ",fname,"xDayInt: 1 \n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,limit_range[equal_percent_idx],sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,est_density,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,cumsum(est_density),sep="$",append=T);cat(file=fname,")","\n\n",append=T)

##
# adjusted weight of cumulitive probablity
est_density=dpearson(limit_range[adj_percent_idx],moments=moment_trnsfm)
est_density=est_density/sum(est_density)
est_density
#if min(est_density)==0 then set them small enough values.
est_density[which(est_density==0)]=1.0e-30
est_density=est_density/sum(est_density)
est_density
sum(est_density)
cumsum(est_density)
#writ to the file
cat(file=fname,"####### adjusted weight ",fname,"xDayInt: 1 \n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,limit_range[adj_percent_idx],sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,est_density,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
cat(file=fname,"c(",append=T);cat(file=fname,cumsum(est_density),sep="$",append=T);cat(file=fname,")","\n\n",append=T)

####

## evaluate quantile function
# quantile_gran=0.001
# 
# tmp_seq=seq(quantile_gran,1,by=quantile_gran)
# tmp_shift=replace(tmp_seq, rep(2:(length(tmp_seq))), tmp_seq[1:length(tmp_seq)-1])
# tmp_shift[1]=0
# tmp_seq
# tmp_shift
# 
# tmp_quantile=qpearson(tmp_seq,moments=moment_trnsfm)
# tmp_quantile
# tmp_quantile_shift=replace(tmp_quantile, rep(2:(length(tmp_quantile))), tmp_quantile[1:length(tmp_quantile)-1])
# tmp_quantile_shift[1]=0
# tmp_quantile_shift
# 
# i=5
# cat("from",tmp_shift[i]," to",tmp_seq[i]," percentile point is from",tmp_quantile_shift[i],"to",tmp_quantile[i])
# 
# i_from=1
# i_to=20
# cat("from",tmp_shift[i_from]," to",tmp_seq[i_to]," percentile point is from",tmp_quantile_shift[i_from],"to",tmp_quantile[i_to])
