library(ggplot2)
library(plyr)
library(gcookbook)
library(rgl)

##散布図の作成
qplot(mtcars$wt,mtcars$mpg)
#dataframeの列を指定したsub data.frame
#heightweight[,c("sex","ageYear","heightIn")]
ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+geom_point()
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex))+geom_point()
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex,colour=sex))+geom_point()
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex,colour=sex))+geom_point()+scale_shape_manual(values=c(1,2))+scale_colour_brewer(palette="Set1")

##折れ線グラフ
qplot(pressure$temperature,pressure$pressure,geom="line")
##data.frame pressure に対して
#pressue$temperature,pressue$pressureがVector
qplot(temperature,pressure,data=pressure,geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()
#折れ線と点の両方を描画する。goemの指定の仕方に注意
qplot(temperature,pressure,data=pressure,geom=c("line","point"))
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()
##折れ線グラフ。BODはdat.frame
ggplot(BOD,aes(x=Time,y=demand))+geom_line()
ggplot(BOD,aes(x=Time,y=demand))+geom_line(linetype="dashed",size=1,colour="blue")
#Timeをfactor(ラベル、カテゴリとして扱う）
BOD1<-BOD
BOD1$Time<-factor(BOD1$Time)
#group化が必要
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line()
#y軸の範囲の指定の仕方
ggplot(BOD,aes(x=Time,y=demand))+geom_line()+ylim(0,max(BOD$demand))
ggplot(BOD,aes(x=Time,y=demand))+geom_line()+expand_limits(y=0)
#点を追加する
ggplot(BOD,aes(x=Time,y=demand))+geom_line()+geom_point()
#y軸を対数表示に
ggplot(worldpop,aes(x=Year,y=Population))+geom_line()+geom_point()
ggplot(worldpop,aes(x=Year,y=Population))+geom_line()+geom_point()+scale_y_log10()
#2本以上の折れ線グラフ
#ddplyはdata.frameをカテゴライズして集計して、集計されたdata.frameを返すものらしい
tg<-ddply(ToothGrowth,c("supp","dose"),summarise,length=mean(len))
#colour,linetypeに離散値変数をマップする
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line()
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line(linetype="dashed",size=1,colour="blue")
#点のサイズ変更など
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line()+geom_point(size=4,shape=21)
#重ならないように線と点をずらす
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line(position=position_dodge(0.2))+geom_point(position=position_dodge(0.2),size=4)
#線の体裁を変更
#両方の線の属性が同じ
ggplot(tg,aes(x=dose,y=length,group=supp))+geom_line(colour="darkgreen",size=1.5)
#suppがcolourにマッピングされているため、自動的にグループ分けに使用される
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line(linetype="dashed")+geom_point(shape=22,size=3,fill="white")
#factor。ggplotのgroupに同じ変数を指定する必要あり
ggplot(tg,aes(x=factor(dose),y=length,colour=supp,group=supp))+geom_line()
ggplot(tg,aes(x=factor(dose),y=length,linetype=supp,group=supp))+geom_line()

##１次元のデータの分布をhistgramで表す
#Only a vector is given to qplot. mtcars is data.frame. mtcars$mpg is vector.
qplot(mtcars$mpg)
#for the same data.frame
qplot(mpg,data=mtcars,binwidth=4)

##関数曲線をplotする
myfun <- function(xvar){
  1/(1+exp(-xvar+10))
}
#xの範囲は0-20
qplot(c(0,20),fun=myfun,stat="function",geom="line")
ggplot(data.frame(x=c(0,20)),aes(x=x))+stat_function(fun=myfun,geom="line")

##
#rglによる散布図と3次元予測面
#

#2つのベクトル要素を交互に並べる関数
interleave<-function(v1,v2) as.vector(rbind(v1,v2))

#
predictgrid<-function(model,xvar,yvar,zvar,res=16,type=NULL){
  xrange<-range(model$model[[xvar]])
  yrange<-range(model$model[[yvar]])
  
  newdata<-expand.grid(x=seq(xrange[1],xrange[2],length.out=res),
                       y=seq(yrange[1],yrange[2],length.out=res))
  names(newdata)<-c(xvar,yvar)
  newdata[[zvar]]<-predict(model,newdata,type=type)
  newdata
}

#
df2mat<-function(p,xvar=NULL,yvar=NULL,zvar=NULL){
  if(is.null(xvar)) xvar <- names(p)[1]
  if(is.null(yvar)) yvar <- names(p)[2]
  if(is.null(zvar)) zvar <- names(p)[3]
  
  x<-unique(p[[xvar]])
  y<-unique(p[[yvar]])
  z<-matrix(p[[zvar]],nrow=length(y),ncol=length(x))
  
  m<-list(x,y,z)
  names(m)<-c(xvar,yvar,zvar)
  m
}

m<-mtcars
mod<-lm(mpg~wt+disp+wt:disp,data=m)
#mod<-lm(mpg~wt+disp,data=m)

m$pred_mpg<-predict(mod)

mgrid_df<-predictgrid(mod,"wt","disp","mpg")
mgrid_list<-df2mat(mgrid_df)

plot3d(m$wt,m$disp,m$mpg,
       xlab="",ylab="",zlab="",
       axes=FALSE,
       type="s",size=0.5,lit=FALSE)


spheres3d(m$wt,m$disp,m$pred_mpg,alpha=0.4,type="s",size=0.5,lit=FALSE)

segments3d(interleave(m$wt,m$wt),
          interleave(m$disp,m$disp),
          interleave(m$mpg,m$pred_mpg),
          alpha=0.4,col="red")

surface3d(mgrid_list$wt,mgrid_list$disp,mgrid_list$mpg,
          alpha=0.4,front="lines",back="lines")

rgl.bbox(color="grey50",
         emission="grey50",
         xlen=0,ylen=0,zlen=0)

rgl.material(color="black")

axes3d(edges=c("x--","y+-","z--"),
       ntick=6,
       cex=0.75)

mtext3d("Weight",edge="x--",line=2)
mtext3d("Displacement",edge="y+-",line=3)
mtext3d("MPG",edge="z--",line=3)

rm(mgrid_list,mod,m,mgrid_df)
