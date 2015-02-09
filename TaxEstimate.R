library(ggplot2)
library(plyr)
library(dplyr)

#
# Functions
#

get.syotokuzei<-function(x){
  x<-x*10000
  tax<-0
  tax<-as.numeric(x>=1000)*as.numeric(x<1950000)*(x*0.05)
  tax<-tax+as.numeric(x>=1950000)*as.numeric(x<3300000)*(x*0.1-97500)
  tax<-tax+as.numeric(x>=3300000)*as.numeric(x<6950000)*(x*0.2-427500)
  tax<-tax+as.numeric(x>=6950000)*as.numeric(x<9000000)*(x*0.23-636000)
  tax<-tax+as.numeric(x>=9000000)*as.numeric(x<18000000)*(x*0.33-1536000)
  tax<-tax+as.numeric(x>=18000000)*(x*0.4-2796000)
  return(tax/10000)
}

get.kokuho<-function(x){
  x<-x*10000
  kokuho<-51200+15300
  kokuho<-kokuho+as.numeric(x>=330000)*as.numeric((x-330000)*0.1<810000)*(x-330000)*0.1
  kokuho<-kokuho+as.numeric(x>=330000)*as.numeric((x-330000)*0.1>=810000)*810000
  return(kokuho/10000)
}

total.tax.kokuho<-function(tax_df,syotoku,gensen_bunri,sinkoku_bunri){
  kokuho_kijyun<-tax_df[syotoku,]$s+sinkoku_bunri
  total_tax<-tax_df[syotoku,]$s*tax_df[syotoku,]$ttl_tax_rate+
    sinkoku_bunri*0.2015+
    tax_df[kokuho_kijyun,]$s*tax_df[kokuho_kijyun,]$kokuho_rate+
    gensen_bunri*0.2015
  total_tax
  
}

total.tax.kenpo<-function(tax_df,syotoku,gensen_bunri,sinkoku_bunri,kenpo,kosenenkin){
  total_tax<-tax_df[syotoku,]$s*tax_df[syotoku,]$ttl_tax_rate+
    sinkoku_bunri*0.2015+
    gensen_bunri*0.2015+
    kenpo+
    kosenenkin
  
  total_tax
  
}

get.aoiro_kojo<-function(){
  aoiro_kiso_kojo<-690000
  aioro_keihi<-(53500+2500+750+4000)*12
  aoiro_kojo<-aoiro_kiso_kojo+aioro_keihi
  aoiro_kojo
}

get.zei_kojo<-function(kokuho){
  kiso_kojo<-380000
  kojin_401k<-680000
  tyusyo_kyosai<-840000
  syakaihoken<-kokuho+179160
  seimeihoken<-50000
  zei_kojo<-get.aoiro_kojo()+kiso_kojo+kojin_401k+
    tyusyo_kyosai+syakaihoken+seimeihoken
  zei_kojo
}

get.jyumin_zei<-function(syotoku,kojo){
  jyumin_zei<-as.numeric(syotoku>=kojo)*(syotoku-kojo)*0.08
  jyumin_zei
}

get.jigyou_zei_kojo<-function(kurikoshi){
  kojo<-get.aoiro_kojo()-680000+2900000
  kojo<-kojo+kurikoshi
  kojo
  
}
get.jigyou_zei<-function(syotoku,kurikoshi){
  syotoku<-syotoku*10000
  kurikoshi<-kurikoshi*10000
  kojo<-get.jigyou_zei_kojo(kurikoshi)
  jigyou_zei<-as.numeric(syotoku>=kojo)*(syotoku-kojo)*0.05
  jigyou_zei<-jigyou_zei/10000
  jigyou_zei
}

aoiro.total.zei<-function(syotoku){
  syotoku<-syotoku*10000
  
  kokuho<-get.kokuho((syotoku-get.aoiro_kojo())/10000)*10000
  
  zei_kojo<-get.zei_kojo(kokuho)
  syotoku_zei<-get.syotokuzei((syotoku-zei_kojo)/10000)*10000
  
  jyumin_zei<-get.jyumin_zei(syotoku/10000,zei_kojo/10000)*10000
  
  jigyou_zei<-get.jigyou_zei(syotoku/10000,0)*10000
  
  total_zei<-(kokuho+syotoku_zei+jyumin_zei+jigyou_zei)/10000
  return(total_zei)
}

aoiro.total.zei.rate<-function(syotoku){
  return(aoiro.total.zei(syotoku)/syotoku)
}

##
# aoiro Rate
##
syotoku<-seq(1,1200,by=1)
tax_aoiro_ttl<-data.frame(s=syotoku,aoiro_tax=aoiro.total.zei(syotoku))
tax_aoiro_ttl$aoiro_tax_rate<-tax_aoiro_ttl$aoiro_tax/tax_aoiro_ttl$s

tax_aoiro_ttl %>% filter(aoiro_tax>6.7) %>% filter(aoiro_tax_rate<0.3) -> tax_aoiro_ttl

(gg_<-ggplot(tax_aoiro_ttl,aes(x=s,y=tax))+
   geom_line(data=tax_aoiro_ttl,aes(s,aoiro_tax)))

(gg_<-ggplot(tax_aoiro_ttl,aes(x=s,y=tax_rate))+
   geom_line(data=tax_aoiro_ttl,aes(s,aoiro_tax_rate)))

rm(syotoku,tax_aoiro_ttl,gg_,)

##
# Kyosai Rate
##
syotoku<-seq(1,2000,by=1)
tax<-get.syotokuzei(syotoku)
tax_df<-data.frame(s=syotoku,tax=tax)
tax_df$tax_rate<-tax_df$tax/tax_df$s
tax_df$ttl_tax_rate<-tax_df$tax_rate+0.08
rm(syotoku,tax)

(total.tax.kenpo(tax_df,570,100,110,50,50)-14)/1210
(total.tax.kenpo(tax_df,680,100,0,50,50)-14)/1210
(total.tax.kenpo(tax_df,670,0,110,50,50)-14-5)/1210

(gg_<-ggplot(tax_df,aes(x=s,y=tax))+
  geom_line(data=tax_df,aes(s,tax)))
(gg_<-ggplot(tax_df,aes(x=s,y=ttl_tax_rate))+
   geom_line(data=tax_df,aes(s,ttl_tax_rate)))
rm(gg_)

##
# Clean up
##
rm(tax_df,get.syotokuzei,get.jyumin_zei,get.kokuho,
   get.aoiro_kojo,get.zei_kojo,aoiro.total.zei,aoiro.total.zei.rate,
   get.jigyou_zei_kojo,get.jigyou_zei,
   total.tax.kenpo,total.tax.kokuho)
