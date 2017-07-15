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

#####
## create UDLY_OPChain_PreForSkew, UDLY_OPChain_PreForPos

dates=as.Date(Sys.time())-c(0:4)
fnames=paste(DataFiles_Path_G,Underying_Symbol_G,"OPT",dates,".csv",sep="")
fnames=fnames[file.exists(fnames)]
fnames

total_table=NULL
for(i in 1:length(fnames)){
  tmp<-read.table(fnames[i],header=T,skipNul=T,stringsAsFactors=F,sep=",")
  print(tmp)
  total_table %>% dplyr::bind_rows(tmp) -> total_table
}
total_table %>% 
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d")),as.Date(ExpDate,format="%Y/%m/%d"),TYPE,Strike) %>%
  dplyr::distinct() %>%
  dplyr::filter(Strike != "Strike") -> preForSkew
(preForSkew)
#preForSkew$Strike=as.integer(preForSkew$Strike)
preForSkew$Date=format(as.Date(preForSkew$Date,format="%Y/%m/%d"),"%Y/%m/%d")
preForSkew$ExpDate=format(as.Date(preForSkew$ExpDate,format="%Y/%m/%d"),"%Y/%m/%d")
na.omit(preForSkew)->preForSkew
preForSkew$Bid=as.numeric(preForSkew$Bid)
preForSkew$Ask=as.numeric(preForSkew$Ask)
preForSkew$Last=as.numeric(preForSkew$Last)

head(preForSkew,n=50)
tail(preForSkew,n=50)
#write to a file
write.table(preForSkew,paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_PreForSkew.csv",sep=""),row.names = F,col.names=T,sep=",",append=F)

#Pre for Pos
last_day_distance=1
preForSkew %>% 
  dplyr::filter(as.Date(Date,format="%Y/%m/%d") == (as.Date(Sys.time())-last_day_distance))  %>% 
  dplyr::distinct() -> preForPos
head(preForPos,n=50)
tail(preForPos,n=50)
#write to a file
write.table(preForPos,paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_PreForPos.csv",sep=""),row.names = F,col.names=T,sep=",",append=F)

# copy _OPChain_PreForSkew.csv _OPChain_Pre.csv
file.copy(from=paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_PreForSkew.csv",sep=""), 
          to=paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_Pre.csv",sep=""),
          overwrite=T)

#####
## Return Distribution Estimates

##  Histgram

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


fname=paste(DataFiles_Path_G,Underying_Symbol_G,"_Weight_Explicit.txt",sep="")
cat(file=fname,"####### creating ",fname,"\n\n",append=F)

for(UdlStepNum in c(14,16)){
  #for(UdlStepPct in c(seq(0.009,0.004,by=-0.0002),seq(0.01,0.0092,by=-0.0002),seq(0.0038,0.002,by=-0.0002))){
  for(UdlStepPct in c(seq(0.009,0.004,by=-0.0002))){
    cat(file=fname,"#### UdlStepPct:",UdlStepPct," x UdlStepNum:",UdlStepNum,"\n",append=T)
    for(xDayInt in c(EvalFuncSetting$holdDays,1)){
      
      cat(file=fname,"## xDayInt:",xDayInt,"\n",append=T)
      
      tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt)
      tmp$lm
      if(IS_SELECTIVE_WEIGHT_ESTM){
        selectSuffixForValidIV(histIV,xDayInt,a_low,d_low,a_high,d_high)->suffix_slctd
        tmp=saveP2IVReg(histPrc,histIV,DATA_NUM,xDayInt,effectiv_suffix=suffix_slctd)
      }
      #h <- dpih(tmp$P2IVxd$PCxdCtC)
      #bins <- seq(min(tmp$P2IVxd$PCxdCtC)-3*h/2, max(tmp$P2IVxd$PCxdCtC)+3*h/2, by=h)
      #hist(tmp$P2IVxd$PCxdCtC, breaks=bins)
      #rm(h)
      
      #randam generate
      #tmp_data=rpearson(20000,moments=empMoments(tmp$P2IVxd$PCxdCtC))
      #empMoments(tmp_data)
      #histgram
      #h <- dpih(tmp_data)
      #bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
      #hist(tmp_data, breaks=bins)
      #density estimation
      est_density=dpearson(seq(
        (-UdlStepPct*UdlStepNum),
        UdlStepPct*UdlStepNum,
        by=UdlStepPct),
        moments=empMoments(tmp$P2IVxd$PCxdCtC))
      
      est_density=est_density/sum(est_density)
      
      #frame()
      #plot(seq(
      #  (-UdlStepPct*UdlStepNum),
      #  UdlStepPct*UdlStepNum,
      #  by=UdlStepPct),
      #  est_density,col="blue")
      
      ##  moment estimation and moment transformation for desirable "should be headged" Distribution
      
      moment_org=empMoments(tmp$P2IVxd$PCxdCtC)
      #just show anualized mean and sd
      cat(file=fname,c(moment_org["mean"],moment_org["variance"],moment_org["skewness"],moment_org["kurtosis"]),"\n",sep=",",append=T)
      cat(file=fname,"c(",append=T);cat(file=fname,est_density,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
      
      
      ##  transormed moment mean neutral
      moment_trnsfm=moment_org
      
      #drift transformed (annualized)
      dfift_trnsfm=0
      moment_trnsfm["mean"]=dfift_trnsfm/(252/EvalFuncSetting$holdDays)
      
      #volatility transformed (annualized)
      #vol_trnsfm=sqrt(moment_org["variance"])*sqrt(252/EvalFuncSetting$holdDays)*1
      #moment_trnsfm["variance"]=(vol_trnsfm/sqrt(252/EvalFuncSetting$holdDays))^2
      (moment_org)
      (moment_trnsfm)
      #randam generate
      #tmp_data=rpearson(20000,moments=moment_trnsfm)
      #empMoments(tmp_data)
      #histgram
      #h <- dpih(tmp_data)
      #bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
      #hist(tmp_data, breaks=bins)
      #density estimation
      est_density=dpearson(seq(
        (-UdlStepPct*UdlStepNum),
        UdlStepPct*UdlStepNum,
        by=UdlStepPct),
        moments=moment_trnsfm)
      
      est_density=est_density/sum(est_density)
      
      cat(file=fname,c(moment_trnsfm["mean"],moment_trnsfm["variance"],moment_trnsfm["skewness"],moment_trnsfm["kurtosis"]),"\n",sep=",",append=T)
      cat(file=fname,"c(",append=T);cat(file=fname,est_density,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
      
      ##  transormed moment opposite mean
      moment_trnsfm=moment_org
      
      #drift transformed (annualized)
      dfift_trnsfm_ration=1
      moment_trnsfm["mean"]=moment_org["mean"]*(-1)*dfift_trnsfm_ration
      #(moment_org)
      #(moment_trnsfm)
      #randam generate
      #tmp_data=rpearson(20000,moments=moment_trnsfm)
      #empMoments(tmp_data)
      #histgram
      #h <- dpih(tmp_data)
      #bins <- seq(min(tmp_data)-3*h/2, max(tmp_data)+3*h/2, by=h)
      #hist(tmp_data, breaks=bins)
      #density estimation
      est_density=dpearson(seq(
        (-UdlStepPct*UdlStepNum),
        UdlStepPct*UdlStepNum,
        by=UdlStepPct),
        moments=moment_trnsfm)
      
      est_density=est_density/sum(est_density)
      
      cat(file=fname,c(moment_trnsfm["mean"],moment_trnsfm["variance"],moment_trnsfm["skewness"],moment_trnsfm["kurtosis"]),"\n",sep=",",append=T)
      cat(file=fname,"c(",append=T);cat(file=fname,est_density,sep="$",append=T);cat(file=fname,")","\n\n",append=T)
      
    }
  }
}

#####
## aggregate UDLY_EvalPosition_Grdyxxyymm.csv to make the best result.


st <- "powershell.exe .\\shell\\cmd13.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd14.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\Grdy-.csv \" "
system(st) ;rm(st)

tmp<-read.table(paste(ResultFiles_Path_G,"Grdy.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
tmp=tmp[,1:(length(opchain$Position)+1)]
colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp

tmp[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp2
tmp2  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp3
tmp$putn<-unlist(tmp3$putn);tmp$calln<-unlist(tmp3$calln);rm(tmれ2);rm(tmp3)
tmp %>% dplyr::mutate(posn=(putn+calln)) -> tmp

#write table and file copy
fname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition_Grdy",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
write.table(tmp,fname,row.names = F,col.names=F,sep=",",append=F)
file.copy(from=fname, 
          to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),
          overwrite=T)
LocalflipScoreWriteToFile(fname,50)

#remove Greedy.csv
st <- "powershell.exe -Command \" del .\\ResultData\\Grdy.csv \" "
system(st) ;rm(st)
