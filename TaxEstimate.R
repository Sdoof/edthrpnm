library(ggplot2)
library(plyr)
library(dplyr)

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

syotoku<-seq(1,2000,by=1)
tax<-get.syotokuzei(syotoku)
tax_df<-data.frame(s=syotoku,tax=tax)
tax_df$tax_rate<-tax_df$tax/tax_df$s
tax_df$ttl_tax_rate<-tax_df$tax_rate+0.1
tax_df$kokuho<-get.kokuho(syotoku)
tax_df$kokuho_rate<-tax_df$kokuho/tax_df$s
tax_df$ttl_tax_kokuho_rate<-tax_df$ttl_tax_rate+tax_df$kokuho_rate
rm(syotoku,tax)

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

total.tax.kokuho(tax_df,250,150,50)/450
total.tax.kokuho(tax_df,250,200,0)/450
total.tax.kokuho(tax_df,250,0,200)/450
total.tax.kokuho(tax_df,500-300,300,0)/800
total.tax.kokuho(tax_df,500-300,0,300)/800
total.tax.kokuho(tax_df,800-300,0,0)/800

(total.tax.kenpo(tax_df,570,100,110,50,50)-14)/1210
(total.tax.kenpo(tax_df,680,100,0,50,50)-14)/1210
(total.tax.kenpo(tax_df,670,0,110,50,50)-14-5)/1210

(gg_<-ggplot(tax_df,aes(x=s,y=tax))+
  geom_line(data=tax_df,aes(s,tax)))
(gg_<-ggplot(tax_df,aes(x=s,y=ttl_tax_rate))+
   geom_line(data=tax_df,aes(s,ttl_tax_rate)))
(gg_<-ggplot(tax_df %>% filter(s>130),aes(x=s,y=ttl_tax_kokuho_rate))+
   geom_line(data=tax_df %>% filter(s>130),aes(s,ttl_tax_kokuho_rate)))
(gg_<-ggplot(tax_df %>% filter(s>130),aes(x=s,y=s*ttl_tax_kokuho_rate))+
   geom_line(data=tax_df %>% filter(s>130),aes(s,s*ttl_tax_kokuho_rate)))
rm(gg_)

rm(tax_df,get.syotokuzei,get.kokuho)
