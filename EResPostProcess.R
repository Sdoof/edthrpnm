library(dplyr)
library(RQuantLib)
library(ggplot2)

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")
#Calendar
CALENDAR_G=ConfigParameters["CALENDAR_G",1]

# Possibly read from File
riskFreeRate_G=as.numeric(ConfigParameters["riskFreeRate_G",1])
divYld_G=as.numeric(ConfigParameters["divYld_G",1])

#Definition
OpType_Put_G=as.numeric(ConfigParameters["OpType_Put_G",1])
OpType_Call_G=as.numeric(ConfigParameters["OpType_Call_G",1])

#Skewness Calculation
TimeToExp_Limit_Closeness_G=as.numeric(ConfigParameters["TimeToExp_Limit_Closeness_G",1])

#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

#holdDays Trading Days. GreeksEffect are calculated based on this holding days.
holdDays=as.numeric(ConfigParameters["holdDays",1])

#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays=as.numeric(ConfigParameters["dviv_caldays",1])

#Multipler of Position
PosMultip=as.numeric(ConfigParameters["PosMultip",1])

#HV_IV_Adjust_Ratio
HV_IV_Adjust_Ratio=as.numeric(ConfigParameters["EvalFnc_HV_IV_Adjust_Ratio",1])

#Threshhold
Thresh_Score1=as.numeric(ConfigParameters["ResultProcess_Thresh_Score1",1])
Thresh_Score2=as.numeric(ConfigParameters["ResultProcess_Thresh_Score2",1])
Thresh_Score3=as.numeric(ConfigParameters["ResultProcess_Thresh_Score3",1])
Thresh_AdvEffect=as.numeric(ConfigParameters["ResultProcess_Thresh_AdvEffect",1])

#Save the rest position
SAVE_ALL=FALSE

#ophcain 
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",",stringsAsFactors=FALSE)
#filtering. deleting unnecessary column
opchain %>% dplyr::select(-(contains('Frac',ignore.case=TRUE)),
                          -(IV)) %>% as.data.frame() -> opchain
#only OOM targeted
#opchain %>% dplyr::filter(HowfarOOM>=0) -> opchain
rm(rf)
#iniPos
iniPos<-opchain$Position

#create put and call pos num of the specified spread
getPutCallnOfthePosition<-function(x){
  type<-opchain$TYPE
  putpos<-as.numeric(type==OpType_Put_G)
  putn<-sum( as.numeric((putpos*x)!=0) )
  callpos<-as.numeric(type==OpType_Call_G)
  calln<-sum( as.numeric((callpos*x)!=0) )
  return (c(putn,calln))
}

##
# Exact (1Cb)
res1<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
res1 %>% dplyr::arrange(res1[,(length(iniPos)+1)]) %>% dplyr::distinct() -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<Thresh_Score1) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
res1 -> total_res

##
#  2Cb
res1<-read.table(paste(ResultFiles_Path_G,"2Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
res1 %>% dplyr::arrange(res1[,(length(iniPos)+1)]) %>% dplyr::distinct() -> res1
#必要のない列の削除
res1 %>% select(1:(length(iniPos)+1)) -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<Thresh_Score2) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res
rm(res1)

##
#  3Cb
res1<-read.table(paste(ResultFiles_Path_G,"3Cb.csv",sep=""),header=F,skipNul=TRUE,sep=",")
res1 %>% dplyr::arrange(res1[,(length(iniPos)+1)]) %>% dplyr::distinct() -> res1
#必要のない列の削除
res1 %>% select(1:(length(iniPos)+1)) -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<Thresh_Score3) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

##Historical Implied Volatility Data
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",nrows=1000);rm(rf)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(opchain$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

# Writing to files based on option legs total number
total_res %>% mutate(posn=(putn+calln)) -> total_res
total_res %>%  filter(posn<=6) %>% filter(.[,length(iniPos)+1]<2.0) ->tmp_fil 

## Advantageous Effect

#create put and call pos num of the specified spread
getPositionWithGreeks<-function(tmp_fil){
  tmp_fil[,1:length(iniPos)] %>% rowwise() %>% 
    do(theGreks=getPositionGreeks(hollowNonZeroPosition(unlist(.)),multi=PosMultip,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)) -> tmp
  tmp_fil$theGreks<-tmp$theGreks
  tmp_fil %>% rowwise() %>% do(Delta=.$theGreks$Delta,DeltaEffect=.$theGreks$DeltaEffect,VegaEffect=.$theGreks$VegaEffect,
                               ThetaEffect=.$theGreks$ThetaEffect,GammaEffect=.$theGreks$GammaEffect) -> tmp2
  tmp_fil$Delta<-unlist(tmp2$Delta)
  tmp_fil$DeltaEffect<-unlist(tmp2$DeltaEffect)
  tmp_fil$ThetaEffect<-unlist(tmp2$ThetaEffect)
  tmp_fil$GammaEffect<-unlist(tmp2$GammaEffect)
  tmp_fil$VegaEffect<-unlist(tmp2$VegaEffect)
  tmp_fil$theGreks<-NULL
  tmp_fil$AdvEffect<-unlist(tmp2$ThetaEffect)+unlist(tmp2$GammaEffect)
  
  return(tmp_fil)
}

getPositionWithGreeks(tmp_fil) %>% arrange(desc(AdvEffect)) -> tmp_fil

## Filtering
tmp_fil %>% arrange(desc(AdvEffect)) %>%  filter(Delta>-50) %>% filter(Delta<30) %>%
  top_n(100) -> tmp_fil_w ; print(tmp_fil_w)
tmp_fil %>% arrange(desc(AdvEffect)) %>%  filter(.[,length(iniPos)+1]<1.6) %>% 
  filter(Delta>-40) %>% filter(Delta<30) %>% top_n(100) -> tmp_fil_w ; print(tmp_fil_w)

## Save to a file
write.table(tmp_fil_w,paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)
rm(tmp_fil_w)

## Advantageous Effect for the Rest
if(SAVE_ALL){
  total_res -> tmp_fil2
  getPositionWithGreeks(tmp_fil2) %>% arrange(desc(AdvEffect)) -> tmp_fil2
 
  ##Filtering
  tmp_fil2 %>% arrange(desc(AdvEffect)) %>%  filter(.[,length(iniPos)+1]<1.2) %>% 
    filter(Delta>-50) %>% filter(Delta<30) %>% top_n(5000) -> tmp_fil2_w ; print(tmp_fil2_w) ; print(tail(tmp_fil2_w,5))
  
  ##Save to a file
  write.table(tmp_fil2_w,paste(ResultFiles_Path_G,Underying_Symbol_G,"_ALLEvalPosition.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)
  rm(tmp_fil2_w)
}

rm(getPutCallnOfthePosition,getPositionWithGreeks)
rm(tmp,res1)
rm(histIV,opchain,iniPos)

rm(tmp_fil,tmp_fil2,total_res)
rm(HV_IV_Adjust_Ratio,Thresh_Score1,Thresh_Score2,Thresh_Score3,Thresh_AdvEffect,SAVE_ALL)
rm(Thresh_Score1,Thresh_Score2,Thresh_Score3,Thresh_AdvEffect,SAVE_ALL)
rm(CALENDAR_G,PosMultip,divYld_G,dviv_caldays,holdDays,riskFreeRate_G)
rm(ConfigFileName_G,ConfigParameters)
rm(DataFiles_Path_G,ResultFiles_Path_G,OpType_Call_G,OpType_Put_G)
rm(Underying_Symbol_G,TimeToExp_Limit_Closeness_G)

##
#  2Cb+2Cb
# res1<-createCombineCandidatePool(fname=paste(ResultFiles_Path_G,"4Cb.csv",sep=""),
#                                 pnum=0,nrows=-1,skip=0,method=1)
#必要のない列の削除
# res1 %>% select(1:(length(iniPos)+1)) -> res1
#over the specified socre
# res1 %>% filter(.[,length(iniPos)+1]<1.02) -> res1
#posnum put call
# res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
# tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
# res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
#factorと認識されたときの変換 #res1$V1<-as.numeric(as.character(res1$V1))
#full join
# full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

##
# 3Cb+3Cb
# res1<-createCombineCandidatePool(fname=paste(ResultFiles_Path_G,"6Cb.csv",sep=""),
#                                  pnum=0,nrows=-1,skip=0,method=1)
# res1 %>% select(1:(length(iniPos)+1)) -> res1
#over the specified socre
# res1 %>% filter(.[,length(iniPos)+1]<1.01) -> res1
#posnum put call
# res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
# tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
# res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
#full join
#
