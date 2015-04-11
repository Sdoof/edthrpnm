library(dplyr)
library(RQuantLib)
library(ggplot2)

#add posnum info to result data frmae. this case pools[[6]][[2]]
#pools[[6]][[2]][,1:length(iniPos)] %>% rowwise() %>% do(s=sum( as.numeric(unlist(.)!=0) )) -> tmp
#unlist(tmp)
#pools[[6]][[2]]$posnum<-unlist(tmp) ; rm(tmp)

#read from file to post process. reuse createCombineCandidatePool to get a data.frame

##
# Exact (1Cb)
res1<-createCombineCandidatePool(fname=paste(".\\ResultData\\1Cb-2015-4-09.csv",sep=""),
                                pnum=0,nrows=-1,skip=0,method=1)

#above average score
#res1 %>% filter(.[,length(iniPos)+1]<mean(res1[,length(iniPos)+1]))
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<1.2) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

##
#  2Cb
res2<-createCombineCandidatePool(fname=paste(".\\ResultData\\2Cb-2015-4-09.csv",sep=""),
                                 pnum=0,nrows=-1,skip=0,method=1)
#必要のない列の削除
res2 %>% select(1:(length(iniPos)+1)) -> res2

#over the specified socre
res2 %>% filter(.[,length(iniPos)+1]<1.2) -> res2

#posnum put call
res2[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res2$putn<-unlist(tmp2$putn);res2$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#
#  total_res  一つにまとめたdata.frame. 
full_join(res1,res2) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res
rm(res1,res2)

##
#  3Cb
res1<-createCombineCandidatePool(fname=paste(".\\ResultData\\3Cb-2015-4-09.csv",sep=""),
                                 pnum=0,nrows=-1,skip=0,method=1)
#必要のない列の削除
res1 %>% select(1:(length(iniPos)+1)) -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<1.2) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

##
#  2Cb+2Cb
res1<-createCombineCandidatePool(fname=paste(".\\ResultData\\combine-Result-2Cb(+1Cb+1Cb)2Cb(+1Cb+1Cb)-2015-4-09.csv",sep=""),
                                 pnum=0,nrows=-1,skip=0,method=1)
#必要のない列の削除
res1 %>% select(1:(length(iniPos)+1)) -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<1.2) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

##
# 3Cb+3Cb
res1<-createCombineCandidatePool(fname=paste(".\\ResultData\\combine-Result-3Cb(+1Cb+1Cb+1Cb)3Cb(+1Cb+1Cb+1Cb)-2015-4-09.csv",sep=""),
                                 pnum=0,nrows=-1,skip=0,method=1)
#必要のない列の削除
res1 %>% select(1:(length(iniPos)+1)) -> res1
#over the specified socre
res1 %>% filter(.[,length(iniPos)+1]<1.2) -> res1
#posnum put call
res1[,1:length(iniPos)] %>% rowwise() %>% do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% rowwise() %>% do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)

#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

##
# 条件指定
total_res %>% mutate(posn=(putn+calln)) -> total_res
total_res %>%  filter(posn<=8) %>% filter(.[,length(iniPos)+1]<1.1) ->tmp_fil; total_res <- tmp_fil
#profit impact
total_res$profit<-NULL
total_res[,1:length(iniPos)] %>% rowwise() %>% do(profit=getProfit(unlist(.),isDebug=FALSE,udlStepNum=4,udlStepPct=0.02)) -> tmp
as.vector(unlist(tmp))
total_res$profit<-as.vector(unlist(tmp)) ; rm(tmp)

#callの数でフィルタかけてみる
total_res %>% filter(calln>=2) %>% arrange(.[,length(iniPos)+1]) -> tmp_fil

#profitでフィルタ
total_res %>% filter(profit>90) %>% arrange(.[,length(iniPos)+1]) -> tmp_fil

##
# , 形式での表示
#1st 
cat(unlist(total_res[1,]),sep=",")
#filtered tmp's 15th
cat(unlist(tmp_fil[15,]),sep=",")

#上位score1000の評価値の平均
mean(tmp_fil[,length(iniPos)+1])
mean(total_res[,length(iniPos)+1])

# create initial population for optimizer
#genoud
genoud_inipop_vec<-createInitialPopulationVecForOptimizer(res_df=total_res, cand_num=10*length(iniPos))
genoud_inipop_vec<-as.vector(genoud_inipop_vec)
#dfoptiom, 38th element selected.
unlist(total_res[38,1:length(iniPos)]) -> dfoptim_inipop_vec
dfoptim_inipop_vec<-as.vector(dfoptim_inipop_vec)

rm(genoud_inipop_vec,dfoptim_inipop_vec)
rm(tmp,tmp_fil,res1,res2)
rm(total_res)

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