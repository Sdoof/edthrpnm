## Usage of help()
help(sin)
help("histogram")
help.search("histogram")

## Complex number, pi, exp, log
1+2i
pi
sqrt(2)
8^(1/3)
exp(2)
log2(10)
log10(10)
log(10)
log(exp(1))

## 一様分布の例
runif(5,min=-1,max=1)
runif(10)

## 正規分布の例
rnorm(5,mean=3,sd=2)
rnorm(15,mean=3,sd=2)
rnorm(15)

## sample
sample(1:10,5)
sample(1:10,5,replace=TRUE)
sample(1:10,10)

## 変数への代入。コンソールへの表示
x<-4
x
print(x)
(y<--3.5)
x+y

## character 文字列の簡単な演算

(y<-"foo")
z<-"bar"
paste(y,z)
paste(y,z,sep="")
y+z

## 論理演算子 AND & OR | NOT !
!(T & F) | F

## 強制的に数値として扱う。タイプの変換
as.number(T)
as.numberic(T)
as.numeric(T)
as.numeric(F)

## Vector ベクトル
(x<-c(0,1,2,3,4))
(y<-c("foo","bar"))
(z<-c("foo",1))
x[2]
(x[c(1,3,5)]) #xの要素番号をベクトルで指定。順にアクセスする
# seq() 0から3まで0.5刻み。Vectorの作成
seq(0,3,by=0.5)
seq(0,3,length=10)
(x<-seq(0,2,by=0.3))
length(x) #Vectorの長さ（要素数）

# 整数のVectorを簡単に作成
1:10
(x<10:1) #10:1のvectorそれぞれの要素とxを比較し、その論理演算結果を返す
(x<-10:1) #連続整数をVector xに代入
x[3:8] #Vector xの要素番号をベクトルで指定。その要素を順にアクセスして、そのVectorを返す
rep(1,7) #同じ数(1)を回数(7)繰り返した値のVectorを返す
rep(c("a","b","c"),3) #c()で指定したVectorの繰り返しで出来たVector
rep(c(1,2,3),each=3) #c()で指定された要素それぞれを繰り返しで出来たVector
(y<-2:5)
(z<-c(x,y)) #c()によりVectorとVectorを結合したVector
rev(z) #Vectorの反転

## Matrix 行列
(x<-1:8)
(A<-matrix(x,2,4)) #Vector xを2x4(row 2 x col 4)のMatrixとして扱う。↓↓と割り振る
(B<-matrix(x,2,4,byrow=TRUE)) #Vector xを2x4(row 2 x col 4)のMatrixとして扱うが→→と割り振る
A+B #Matrix の加算
A %*% t(B)　# Matrixの積

x<-1:4　#同じ長さのVector
y<-5:8　#同じ長さのVector
# 列ベクトルとして結合してMatrix作成
cbind(x,y)
# 行ベクトルとして結合してMatrix作成
rbind(x,y)
# Matrix 行列同士を結合する
(x<-1:6)
(y<-1:-4)
cbind(matrix(x,2,3),matrix(y,2,3))
#対角行列を生成
diag(c(1,2,3))
diag(3)
#seq()を用いたMatrix作成
(x<-matrix(seq(1,15,by=2),2,4))
dim(x) # 行列の次元を調べる
nrow(x) #行数を調べる
ncol(x) #列数を調べる
x[2,3] # 2行3列要素
x[2,] # 2行目
x[,3] # 3列目

## List
(stock<-list(month=c(1,2,3),price=c(900,1000,1200),brand="AB"))
stock$month #listの要素を$でアクセス
stock$price[2] #listのVector要素の内容をさらに[]でアクセス
stock[3];stock[[3]] #listの要素、要素の中身をアクセス
# Listを結合。Nest構造になっているよう。
(x<-list(month=c(1,2,3),price=c(500,400,300),brand="A"))
(y<-list(month=c(1,2,3,4,5),price=c(600,700,750,700,300),brand="B"))
(z<-list(x,y)) #リストのリストを作る
# Listをバラバラにして結合
(w<-c(x,y)) #Listの内容をVectorとして、Nestなしに結合
(w$brand) #先頭の要素にマッチ
w[[3]]
w[[6]] #添字では後方の値にもアクセスできる。
#names()
(stock<-list(month=c(1,2,3),price=c(900,1000,1200),brand="AB"))
names(stock) #$something をVectorとして取得
names(stock)[3] #3番目の要素値。ここでは$brandの値。
names(stock)[3];names(stock)<-"Company"　#3番目の要素値、ここでは$brandの値を変更
#Listの要素を取り除く
stock$price <- NULL;stock

## Data Frame
#作成
(x<-data.frame(month=c(1,2,3),price=c(9,10,12),deal=c(10,8,5)))

#アクセス
x$price #$something でのアクセス。return Vector
x[[2]] #2列目。ここでは$price。Vectorとしてreturn?
x[2] #2列目。ここでは$price。Matrixとしてreturn?
x[2,] #2行目。Matrixとしてreturn?

#変換
(stock<-list(month=c(1,2,3),price=c(9,10,12),deal=c(10,8,5)))
(y<-data.frame(stock)) #Listのdata.frameへの変換。
(market<-matrix(c(1,2,3,90,100,120,100,80,50),3,3))
(z<-data.frame(market)) #matrixのdata.frameへの変換。

#属性の取得。変更
(x<-data.frame(month=c(1,2,3),price=c(90,100,120),deal=c(100,80,50)))
rownames(x) # 行の名前を確認
rownames(x)<-c("Jan","Feb","Mar")
colnames(x) #列の名前を確認
colnames(x)<-c("tsuki","kakaku","torihiki")
x

### オブジェクトの型を調べる。変換。
x<-1
mode(x)
is.numeric(x)
x<-"foo"
is.character(x)
mode(x)
x<-2+3i
mode(x)
x<-c(T,F)
mode(x)
x
#オブジェクトの型を矯正変換
x<-5;mode(x)
(y<-as.character(x))
mode(y)
(x<- -3:3)
(y<-as.logical(x))
(x<- c(T,F))
(y<-as.numeric(x))
# VectorとMatrix
x<-c(1,2,3,4)
is.vector(x)
is.matrix(x)
(x<-matrix(x,2,2))
is.vector(x)
is.matrix(x)
# List
(x<-list(c(1,2,3),c("foo","bar")))
is.list(x)
is.vector(x) #List also  Vector
is.data.frame(x)
(x<-data.frame(c(1,2,3),c(10,8,6)))
is.data.frame(x)
is.vector(x)
is.list(x) #data.frame also Vector
is.matrix(x)

# vector から list,matrix,data.frame
(x<-c(1,2,3,4)) #Vector
as.list(x) #VectorをListに変換
as.list(x)[[2]];as.list(x)[2] #中身と要素名。要素名はこの場合$でない。
(x<-matrix(x,2,2))
as.vector(x) #matrixをvectorとして。
as.list(x)　#matrixをlistとして。
as.data.frame(x) #matrixをdata.frameとして

#list から vector
(x<-list(c(1,2,3),c("foo","bar")))
(as.vector(x))
x[1]
#data.frame からmatrix
(x<-data.frame(A=c(1,2,3),B=c(10,8,6)))
as.matrix(x)

##制御構造
is.odd <- function(n){
  if(n%%2==1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
is.odd(4)

fact<-function(n){
  tmp <- 1
  for(i in 1:n){
    tmp<-tmp*i
  }
  return(tmp)
}
fact(3)
fact2<-function(n){
  tmp <- 1
  i<-1
  while(i<=n){
    tmp<-tmp*i
    i<-i+1
  }
  return(tmp)
}
fact2(4)
#repeat文。省略

##Drawing Graphs
(x<-rnorm(10));(y<-rnorm(10));plot(x,y)
x<-seq(0,5,by=0.2);y<-2*x^2-5*x;plot(x,y,type="l")
plot(sin,-4*pi,4*pi)
x<-seq(-2,2,by=0.2);plot(x,x^3,type="l");points(x,x^2)
x<-seq(0,5,by=0.2);y<-(x-1)*(x-2)*(x-3)+rnorm(length(x));plot(x,y);lines(x,(x-1)*(x-2)*(x-3),col="red")
x<-rnorm(1000);hist(x) #histgram
x1<-rnorm(50);x2<-rnorm(50)+1;x3<-rnorm(1000);boxplot(x1,x2,x3) #boxplot

##Drawing Pi Chart
#10 types whose each categoies given random number
x<-runif(10,min=0,max=100)

# categories A-J
names(x)<-LETTERS[1:10]

# Standard R PiChart From 3 clockwise
pie(x,col=gray(seq(0,1,length=10)))

# From 12 clockwise
pie(x,clockwise=TRUE,col=rainbow(10))

#pairs() multiple dimensions projected to multiple 2D
x<-rnorm(100)
y<-3*x+rnorm(100)
z<-x-y+rnorm(100)*0.3
pairs(data.frame(x,y,z))

#3D Perspective, Contour
#x-y planes
x<-seq(-3,3,length=50)
y<-seq(-3,3,length=50)
#define function. dnorm() PDF of norm
gauss3d<-function(x,y){dnorm(x)*dnorm(y)}
#z value
z<-outer(x,y,gauss3d)
persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue")
title("Perspective Plot")
dev.new()
contour(x,y,z)
title("Countour Plot")

# Multipe Graphic Device Windows
dev.new();dev.new();dev.new()
dev.list()
dev.cur()
dev.set(dev.list()[2])
dev.set(dev.next())
graphics.off()
#2D
t<-seq(0,1,length=200)
x<-cos(2*pi*t)+rnorm(200,sd=0.05)
y<-sin(2*pi*t)+rnorm(200,sd=0.05)
grph<-layout(matrix(c(2,2,1,3),2,2,byrow=TRUE),width=c(2,1),height=c(1,2),respect=TRUE)
layout.show(grph)
#1
par(mar=c(4,4,4,2))
plot(x,y,col=4,type='p',main="Lisajour figure")
#2
par(mar=c(4,4,4,2))
plot(x,y,col=2,pch=22,xlab="time",ylab="x,y",main="time series")
points(t,y,col=3,pch=23)
#3
yhist<-hist(y,plot=F)
par(mar=c(4,1,4,2))
barplot(yhist$counts,space=0,horiz=TRUE,main="histgram of y")

#File IO
x<-seq(0,6*pi,by=0.1)
#type=n only allocate the region without actually plotting
plot(c(0,6*pi),c(-2,2),type="n",xlab="x",ylab="y")
# draw necessary lines using lines()
lines(x,cos(x),col="blue")
lines(x,sin(x),col="red")
##writng to foo.eps
#dev.copy2eps(file="foo.eps")

## Wrting to a pdf file
#pdf(file="bar.pdf")
##plot(1:10,rep(5,10),xlab="x",ylab="y")
#plot(1:10,20:11,xlab="x",ylab="y")
#plot(1:10,exp(1:10),xlab="x",ylab="y")
#dev.off()

##Functions Definition
area<-function(r){
  S<-pi*r^2
  return(S)
}
area(1)
area(c(1,2,3))

arc<-function(r,theta){
  l<-r*theta
  return(l)
}
arc(1,pi/4)
arc(c(1,2,3),c(pi/4,pi/8,pi/3))

#Input a file as a program
#source("program.r",echo=TRUE) #Programの中を表示しながら

#Output a file of data
market<-data.frame(tsuki=c("Jan","Feb","Mar"),kakaku=c(9,10,12),torihiki=c(12,8,5))
market
#write.table(market,"foo.txt")
write.csv(market,"foo.csv")
#Read csv file
tmp_read<-read.csv("foo.csv",row.names=1)
tmp_read
tmp_read$tsuki
tmp_read$kakaku
mode(tmp_read)


