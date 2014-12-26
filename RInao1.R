##Rのデータ型。数値、文字、日付、カテゴリ
(group <- c("A","A","B","C","C")) #文字型
(group <-  as.factor(group)) #カテゴリ（因子）に変換
(groupc <- as.character(group)) #文字型に変換
#文字列を日付型に変換
(date<-as.Date("141223",format="%y%m%d"))
(date<-as.Date("2014/12/23",format="%Y/%m/%d"))
x <- as.Date("2014/12/23",format="%Y/%m/%d") - as.Date("131123",format="%y%m%d")
as.numeric(x)

##data.frame again
data.frame(ID=c(2,4,6,3,1,5),
           AGE=c(12,54,53,22,13,33),
           GENDER=c(1,2,2,1,2,1),
           DATE=c("2012/1/1","2009/03/11",
                  "2001/1/3","2004/04/04",
                  "2005/05/05","2006/06/06"))

## Read a txt file(csv file)
(x<-read.table("foo2.csv",header=T,sep=","))
(x<-read.csv("foo2.csv"))

#列名がない場合の読み込み時の指定方法 skip.

#access
head(x)
head(x,n=2) #先頭2行

#内容確認
x$AGE
subset(x,select=AGE)
(subset(x,select=c(ID,AGE)))
(subset(x,select=c(ID,AGE)))$ID

##SQL like oparation select
#比較演算子 == != >= > <= V
x
(subset(x,AGE>=30))
(subset(x,AGE>=40,select=GENDER))
(subset(x,AGE>=30,select=c(ID,GENDER)))
(subset(x,AGE>=30,select=c(ID,GENDER))$GENDER)
mode(x$AGE);mode(x$GENDER); #?^???O?̂??߂Ƀ`?F?b?N
#条件演算子 ! & |
subset(x,GENDER==1 & AGE>30)

## instruction to data.frame
x[2];x[[2]]
x[3,2];x[[3,2]]
x[3,"GENDER"];x[[3,"AGE"]]
x[,c(1,2)]
x[c(3,4),]
head(x,n=3)

ncol(x);nrow(x);names(x)
#rbind(x,y);cbind(x,y);data.frame(x,y);merge(x,y)
