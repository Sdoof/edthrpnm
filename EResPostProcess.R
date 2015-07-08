library(dplyr)
library(RQuantLib)
library(ggplot2)

###Global 変数及び定数.

#Definition
OpType_Put_G=1
OpType_Call_G=-1
#Skewness Calculation
TimeToExp_Limit_Closeness_G=0.3
#File
Underying_Symbol_G="RUT"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"
ResultFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\ResultData\\"

#ophcain 
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",",stringsAsFactors=FALSE)
#filtering. deleting unnecessary column
opchain %>% dplyr::select(-(contains('Frac',ignore.case=TRUE)),
                          -(IV)) %>% as.data.frame() -> opchain
#only OOM targeted
opchain %>% dplyr::filter(HowfarOOM>=0) -> opchain
rm(rf)
#iniPos
iniPos<-opchain$Position

##
# Exact (1Cb)
#res1<-createCombineCandidatePool(fname=paste(ResultFiles_Path_G,"1Cb.csv",sep=""),
#                                pnum=0,nrows=-1,skip=0,method=1)
res1<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,sep=",")
res1 %>% dplyr::arrange(res1[,(length(iniPos)+1)]) %>% dplyr::distinct() -> res1
#above average score
#res1 %>% filter(.[,length(iniPos)+1]<mean(res1[,length(iniPos)+1]))
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<1.04) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#write.table(res1,paste(ResultFiles_Path_G,"1Cb-out.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)

##
#  2Cb
#res2<-createCombineCandidatePool(fname=paste(ResultFiles_Path_G,"2Cb.csv",sep=""),
#                                 pnum=0,nrows=-1,skip=0,method=1)
res2<-read.table(paste(ResultFiles_Path_G,"2Cb.csv",sep=""),header=F,sep=",")
res2 %>% dplyr::arrange(res2[,(length(iniPos)+1)]) %>% dplyr::distinct() -> res2
#必要のない列の削除
res2 %>% select(1:(length(iniPos)+1)) -> res2

#over the specified socre
res2 %>% filter(.[,length(iniPos)+1]<1.02) -> res2

#posnum put call
res2[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res2$putn<-unlist(tmp2$putn);res2$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#write.table(res1,paste(ResultFiles_Path_G,"2Cb-out.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)

#
#  total_res  一つにまとめたdata.frame. 
full_join(res1,res2) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res
rm(res1,res2)

##
#  3Cb
#res1<-createCombineCandidatePool(fname=paste(ResultFiles_Path_G,"3Cb.csv",sep=""),
#                                 pnum=0,nrows=-1,skip=0,method=1)
res1<-read.table(paste(ResultFiles_Path_G,"3Cb.csv",sep=""),header=F,sep=",")
res1 %>% dplyr::arrange(res1[,(length(iniPos)+1)]) %>% dplyr::distinct() -> res1

#必要のない列の削除
res1 %>% select(1:(length(iniPos)+1)) -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<1.0) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
#write.table(res1,paste(ResultFiles_Path_G,"3Cb-out.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)

#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

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
# full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

##
# 条件指定
#option position total number
total_res %>% mutate(posn=(putn+calln)) -> total_res
total_res %>%  filter(posn<=6) %>% filter(.[,length(iniPos)+1]<1.2) ->tmp_fil 
total_res %>%  filter(posn==7) %>% filter(.[,length(iniPos)+1]<1.1) ->tmp_fil2
total_res %>%  filter(posn>=8) %>% filter(.[,length(iniPos)+1]<1.0) ->tmp_fil3

write.table(tmp_fil,paste(ResultFiles_Path_G,"posnLE6.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)
write.table(tmp_fil2,paste(ResultFiles_Path_G,"posnEQ7.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)
write.table(tmp_fil3,paste(ResultFiles_Path_G,"posnGT8.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)

#profit impact
# total_res$profit<-NULL
# total_res[,1:length(iniPos)] %>% rowwise() %>% do(profit=getProfit(unlist(.),isDebug=FALSE,udlStepNum=4,udlStepPct=0.02)) -> tmp
# as.vector(unlist(tmp))
# total_res$profit<-as.vector(unlist(tmp)) ; rm(tmp)

#callの数でフィルタかけてみる
# total_res %>% filter(calln>=2) %>% arrange(.[,length(iniPos)+1]) -> tmp_fil

#profitでフィルタ
# total_res %>% filter(profit>90) %>% arrange(.[,length(iniPos)+1]) -> tmp_fil

##
# , 形式での表示
#1st 
# cat(unlist(total_res[1,]),sep=",")
#filtered tmp's 15th
# cat(unlist(tmp_fil[15,]),sep=",")

#上位score1000の評価値の平均
# mean(tmp_fil[,length(iniPos)+1])
# mean(total_res[,length(iniPos)+1])

rm(genoud_inipop_vec,dfoptim_inipop_vec)
rm(tmp,tmp_fil,tmp_fil2,tmp_fil3,res1,res2)
rm(total_res,opchain,iniPos)
rm(DataFiles_Path_G,ResultFiles_Path_G,OpType_Call_G,OpType_Put_G)
rm(Underying_Symbol_G,TimeToExp_Limit_Closeness_G)

##
# Functions to be loaded -------------------------

#create put and call pos num of the specified spread
getPutCallnOfthePosition<-function(x){
  type<-opchain$TYPE
  putpos<-as.numeric(type==OpType_Put_G)
  putn<-sum( as.numeric((putpos*x)!=0) )
  callpos<-as.numeric(type==OpType_Call_G)
  calln<-sum( as.numeric((callpos*x)!=0) )
  return (c(putn,calln))
}

# create initial population vector from data frame
createInitialPopulationVecForOptimizer<-function(res_df, cand_num){
  if(cand_num>nrow(res_df)){ cand_num<-nrow(res_df)  }
  ret_vec<-rep(0,times=0)
  for(i in 1:cand_num){
    cat(unlist(res_df[i,1:length(iniPos)]),"\n",sep=",")
    x<-unlist(res_df[i,1:length(iniPos)])
    ret_vec<-as.numeric((i==1))*x+as.numeric((i!=1))*c(ret_vec,x)
  }
  return (ret_vec)
}