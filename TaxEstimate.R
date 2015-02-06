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

get.kokuho<-function(x,seppan=F){
  x<-x*10000
  kokuho<-51200+15300
  kokuho<-kokuho+as.numeric(x>=330000)*as.numeric((x-330000)*0.1<810000)*(x-330000)*0.1
  kokuho<-kokuho+as.numeric(x>=330000)*as.numeric((x-330000)*0.1>=810000)*810000
  if(seppan)
    kokuho<-kokuho/2
  return(kokuho/10000)
}

syotoku<-seq(1,2000,by=1)
tax<-get.syotokuzei(syotoku)
tax_df<-data.frame(s=syotoku,tax=tax)
tax_df$tax_rate<-tax_df$tax/tax_df$s
tax_df$ttl_tax_rate<-tax_df$tax_rate+0.1
tax_df$kokuho<-get.kokuho(syotoku,seppan=T)
tax_df$kokuho_rate<-tax_df$kokuho/tax_df$s
tax_df$ttl_tax_kokuho_rate<-tax_df$ttl_tax_rate+tax_df$kokuho_rate
rm(syotoku,tax)
  
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
