library(dplyr)
library(RQuantLib)
library(ggplot2)

#add posnum info to result data frmae. this case pools[[6]][[2]]
pools[[6]][[2]][,1:length(iniPos)] %>% rowwise() %>% do(s=sum( as.numeric(unlist(.)!=0) )) -> tmp
unlist(tmp)
pools[[6]][[2]]$posnum<-unlist(tmp) ; rm(tmp)

#read from file to post process. reuse createCombineCandidatePool to get a data.frame

##
# Exact
res1<-createCombineCandidatePool(fname=paste(".\\ResultData\\inipop-Exc-3-24.csv",sep=""),
                                pnum=0,nrows=-1,skip=0,method=1)
res1[,1:length(iniPos)] %>% rowwise() %>% do(s=sum( as.numeric(unlist(.)!=0) )) -> tmp
unlist(tmp)
res1$posnum<-unlist(tmp) ; rm(tmp)
#平均以上のスコアだけ抽出
res1 %>% filter(.[,length(iniPos)+1]<mean(res1[,length(iniPos)+1]))
#top 5000
res1 %>% arrange(.[,length(iniPos)+1]) %>% head(5000) -> res1
  
##
#  3Cb3Cb
res2<-createCombineCandidatePool(fname=paste(".\\ResultData\\combine-Result-3Cb3Cb-2015-3-29.csv",sep=""),
                                 pnum=0,nrows=-1,skip=0,method=1)
#必要のない列の削除
res2 %>% select(1:(length(iniPos)+1)) -> res2
#posnumの項目追加
res2[,1:length(iniPos)] %>% rowwise() %>% do(s=sum( as.numeric(unlist(.)!=0) )) -> tmp
unlist(tmp)
res2$posnum<-unlist(tmp) ; rm(tmp)

#
#  total_res  一つにまとめたdata.frame. 
full_join(res1,res2) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res
rm(res1,res2)

##
#  2Cb
res1<-createCombineCandidatePool(fname=paste(".\\ResultData\\combine-Result-2CB-3-27.csv",sep=""),
                                 pnum=0,nrows=-1,skip=0,method=1)
#必要のない列の削除
res1 %>% select(1:(length(iniPos)+1)) -> res1
#posnum
res1[,1:length(iniPos)] %>% rowwise() %>% do(s=sum( as.numeric(unlist(.)!=0) )) -> tmp
unlist(tmp)
res1$posnum<-unlist(tmp) ; rm(tmp)
#top 5000
res1 %>% arrange(.[,length(iniPos)+1]) %>% head(5000) -> res1

#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

##
#  3Cb
res1<-createCombineCandidatePool(fname=paste(".\\ResultData\\combine-Result-3Cb-2015-3-28.csv",sep=""),
                                 pnum=0,nrows=-1,skip=0,method=1)
#必要のない列の削除
res1 %>% select(1:(length(iniPos)+1)) -> res1
#posnum
res1[,1:length(iniPos)] %>% rowwise() %>% do(s=sum( as.numeric(unlist(.)!=0) )) -> tmp
unlist(tmp)
res1$posnum<-unlist(tmp) ; rm(tmp)
#top 5000
res1 %>% arrange(.[,length(iniPos)+1]) %>% head(5000) -> res1
#full join
full_join(total_res,res1) %>% arrange(.[,length(iniPos)+1])  %>% distinct() -> total_res

##
# 条件指定
total_res %>%  filter(posnum<=8) %>% filter(.[,length(iniPos)+1]<499) ->tmp_fil
total_res %>% filter(.[,length(iniPos)+1]<495) -> total_res

##
# , 形式での表示
#1st 
cat(unlist(total_res[1,]),sep=",")
#filtered tmp's 15th
cat(unlist(tmp_fil[15,]),sep=",")

#上位score1000の評価値の平均
mean(tmp_fil[,length(iniPos)+1])
mean(total_res[,length(iniPos)+1])

rm(tmp,tmp_fil,res1,res2)
rm(total_res)

