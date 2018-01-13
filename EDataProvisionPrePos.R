library(RQuantLib)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')
source('./EDataProvisionUtil.R',encoding = 'UTF-8')

expdateElements=c("2018/03/16","2018/03/29","2018/04/20","2018/04/30","2018/05/16","2018/05/31","2018/06/15")
expdateNotExist=c("2018/05/16")

bdays_per_month<-252/12
get.busdays.between(start=opch$Date,end=opch$ExpDate)/bdays_per_month

tmp=as_tibble(expand.grid(expdateElements,expdateElements,KEEP.OUT.ATTRS=T,stringsAsFactors = F))
tmp %>% dplyr::rename(Front=Var1,Back=Var2) -> tmp

#get.busdays.between(start=opch$Date,end=opch$ExpDate)/bdays_per_month
bdays_per_month<-252/12
tmp %>% 
  dplyr::filter(as.Date(Front,format="%Y/%m/%d")<as.Date(Back,format="%Y/%m/%d")) %>%
  dplyr::mutate(TImeToExpDate=get.busdays.between(start=as.Date(Front,format="%Y/%m/%d"),end=as.Date(Back,format="%Y/%m/%d"))/bdays_per_month) %>%
  dplyr::filter(TImeToExpDate>0.9&TImeToExpDate<2.5) -> tmp

tmp %>%
  dplyr::anti_join(tibble(Front=expdateNotExist)) %>%
  dplyr::anti_join(tibble(Back=expdateNotExist)) %>%
  dplyr::arrange(TImeToExpDate) -> Expdates

Expdates %>% 
  dplyr::mutate(Front=str_extract(Front,"\\d*/\\d*/\\d*")) %>%
  dplyr::mutate(Front=str_replace_all(Front,"/0","/")) %>%
  dplyr::mutate(Back=str_extract(Back,"\\d*/\\d*/\\d*")) %>%
  dplyr::mutate(Back=str_replace_all(Back,"/0","/")) -> Expdates



for( itrnum in 1:length(Expdates$Front)){
  TARGET_EXPDATE_FRONT=Expdates$Front[itrnum]
  TARGET_EXPDATE_BACK=Expdates$Back[itrnum]
  
  #switch: only for today or multiple days for skew calculation
  ProcessFileName=paste("_OPChain_PreForPos.csv",sep="")
  #set TRUE if this opchain is for Future Option 
  isFOP=F
  #Price is Interpolated or not
  isInterpolatedPrice=F
  #Saved File Name
  #TargetFileName=paste("_Positions_Pre_",itrnum,"_",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
  TargetFileName=paste("_Positions_Pre_",itrnum,"_",
                       format(as.Date(TARGET_EXPDATE_FRONT,format="%Y/%m/%d"),"%Y%m%d"),"_",
                       format(as.Date(TARGET_EXPDATE_BACK,format="%Y/%m/%d"),"%Y%m%d"),".csv",sep="")
  
  #create Option Chain Container
  if(!isFOP){
    opchain<-makeOpchainContainer()
  }else{
    opchain <- makeFOPChainContainer()
    cat("(:divYld_G)",divYld_G,"\n")
  }
  na.omit(opchain)->opchain
  
  #inconsistent data purged
  opchain %>% filter(Price!=0) -> opchain
  opchain$Strike=as.numeric(opchain$Strike)
  opchain$TYPE=as.numeric(opchain$TYPE)
  opchain %>% filter((Strike-UDLY)*TYPE<Price) -> opchain
  
  #spread<ASK*k
  k<-0.4
  opchain$Ask=as.numeric(opchain$Ask)
  opchain$Bid=as.numeric(opchain$Bid)
  opchain %>% filter(!((Ask-Bid)>(Ask*k))) -> opchain
  rm(k)
  
  #Implied Volatility Set
  delete<-c(-1)
  N<-nrow(opchain)
  for(i in 1:N){
    tryCatch(a<-set.IVOrig(xT=opchain[i,]),
             error=function(e){
               message(e)
               print(i)
               delete<<-c(delete,-i)
             })
  }
  #要素を削除
  (delete)
  delete<-delete[-1]
  (delete)
  nrow(opchain)
  nrow(opchain[delete,])
  if(length(delete)>0)
    opchain<-opchain[delete,]
  nrow(opchain)
  rm(delete,N,i,a)
  
  #IVの計算とOption PriceとGreeksの設定
  opchain$OrigIV<-set.IVOrig(xT=opchain)
  tmp<-set.EuropeanOptionValueGreeks(opchain)
  opchain$Price<-tmp$Price
  opchain$Delta<-tmp$Delta
  opchain$Gamma<-tmp$Gamma
  opchain$Vega<-tmp$Vega
  opchain$Theta<-tmp$Theta
  opchain$Rho<-tmp$Rho
  
  #opchain$Vomma<-get.EuropeanOptionVomma(opchain)
  rm(tmp)
  #renumber row names
  rownames(opchain) <- c(1:nrow(opchain))
  
  #not adjusted atmiv
  # atmiv_na=makeNotAdjustedBySkewATMiv(opchain)
  
  ## opchain is created
  opchain<-makePosition(opchain,isSkewCalc=F)
  
  opchain<-filterPosition(opchain,TARGET_EXPDATE=TARGET_EXPDATE,TARGET_EXPDATE_FRONT=TARGET_EXPDATE_FRONT,TARGET_EXPDATE_BACK=TARGET_EXPDATE_BACK)
  
  #select and sort
  opchain %>% dplyr::select(Date,ExpDate,TYPE,Strike,ContactName,Position,UDLY,Price,
                            Delta,Gamma,Vega,Theta,Rho,OrigIV,ATMIV,IVIDX,
                            HowfarOOM,TimeToExpDate,Moneyness.Nm) %>% 
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opchain
  opchain$Position<-ifelse(is.na(opchain$Position), 0, opchain$Position)
  
  #Write to a file (RUT_Positions_Pre)
  write.table(opchain,paste(DataFiles_Path_G,Underying_Symbol_G,TargetFileName,sep=""),quote=T,row.names=F,sep=",")
}
