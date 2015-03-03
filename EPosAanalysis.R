library(dplyr)
library(RQuantLib)
library(ggplot2)

# set days interval between which the position is analyzed step by step.
stepdays<-5
udlStepNum<-2
udlStepPct=0.05

#get evaluation days vector, evaldays
opchain$Date<-as.character(opchain$Date);
opchain$ExpDate<-as.character(opchain$ExpDate)
max_days<-min(get.busdays.between(opchain$Date,opchain$ExpDate))
totalstep=max_days%/%stepdays ; rm(max_days)

evaldays<-rep(stepdays,times=totalstep)
evaldays<- cumsum(evaldays)

#read analyzed positon. here we give by copy and paste
pos_anlys<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,-3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-3,2,0,0,0,0)

#note opchain is already instanciated by the proceduces of ERskRtnEval
opchain$Position<-pos_anlys
opchain %>% dplyr::filter(Position!=0) -> thePosition

#initial price
iniPrice <- getPosGreeks(pos=thePosition$Position,greek=thePosition$Price,multi=PosMultip)
iniCredit <- -1*iniPrice

#total data frame
posStepDays<-data.frame(days=evaldays)
#Set data frames as a row value of another data frame.
posStepDays %>% group_by(days) %>%
  do(scene=createPositinEvalTable(position=thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,days=stepdays)) -> posStepDays
#We must adjust each position values
posStepDays %>% group_by(days) %>% rowwise() %>% do(days=.$days,scene2=adjustPosChg(.$scene,.$days-stepdays)) -> tmp
unlist(tmp$days) -> posStepDays$days ; tmp$scene2 -> posStepDays$scene ;rm(tmp)
#We've got the complete posStepDays.
#show for a test.
#posStepDays$scene[[2]];
#posStepDays$scene[[2]]$pos

#Now drawing
drawtbl<-createdAgrregatedPriceTbl(posStepDays,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,iniCredit=iniCredit)
gg<-ggplot(drawtbl,aes(x=UDLY,y=profit,group=day,colour=day))
(
  gg + geom_point()+geom_line(linetype="dashed")+geom_point()
   # +ylim(min(c(min(drawtbl$profit),-20000)),max(drawtbl$profit+200)) +
   # + xlim(min(c(min(thePosition$UDLY)*(1-0.2),min(drawtbl$UDLY))),max(c(min(thePosition$UDLY)*(1+0.2),max(drawtbl$UDLY))))
 )

rm(gg,drawtbl); rm(stepdays,pos_anlys,totalstep,udlStepNum,udlStepPct); rm(posStepDays,thePosition)

#inner functions : graphical related.
#create Aggregated Price Table for Drawing
createdAgrregatedPriceTbl<-function(posStepDays,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,
                                    multi=PosMultip,iniCredit=iniCredit){
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createPriceTbl(.$days,.$scene,iniCredit)) -> tmp
# posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Price,iniCredit)) -> tmp
  agr_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    agr_tbl<-full_join(agr_tbl,tmp$ptbl[[i]])
  }
  
  #thePosition's data frame
  udly_<-posStepDays$scene[[1]]$UDLY
  day_<-min(get.busdays.between(thePosition$Date,thePosition$ExpDate))
  day_<-rep(day_,times=length(udly_))
  
  tmp<-createPositinEvalTable(position=thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,days=c(0))  

  for(i in 1:length(day_)){ 
    if(i==1){
      profit_<- c(
        sum (
        as.numeric(((tmp$pos[[i]]$UDLY-tmp$pos[[i]]$Strike)*(-tmp$pos[[i]]$TYPE)>0))*
          (tmp$pos[[i]]$UDLY-tmp$pos[[i]]$Strike)*(-tmp$pos[[i]]$TYPE)*PosMultip*tmp$pos[[i]]$Position
        )
      )
    }else{
      profit_<-c(profit_,
                 sum(
                   as.numeric(((tmp$pos[[i]]$UDLY-tmp$pos[[i]]$Strike)*(-tmp$pos[[i]]$TYPE)>0))*
                     (tmp$pos[[i]]$UDLY-tmp$pos[[i]]$Strike)*(-tmp$pos[[i]]$TYPE)*PosMultip*tmp$pos[[i]]$Position
                 )
      )
    }
  }
  profit_<-profit_+iniCredit
  intr_val<-data.frame(day=day_,UDLY=udly_,profit=profit_)

  agr_tbl<-full_join(agr_tbl,intr_val)
  agr_tbl
}

getThePositionDrawtable <- function (posStepDays, thePosition, udlStepNum=udlStepNum, udlStepPct=udlStepPct, PosMultip=PosMulti) {
  udly_<-posStepDays$scene[[1]]$UDLY
  day_<-min(get.busdays.between(thePosition$Date,thePosition$ExpDate))
  day_<-rep(day_,times=length(udly_))
  
  tmp<-createPositinEvalTable(position=thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,days=c(0))  
  
  for(i in 1:length(day_)){ 
    if(i==1){
      profit_<- c(
        sum (
          as.numeric(((tmp$pos[[i]]$UDLY-tmp$pos[[i]]$Strike)*(-tmp$pos[[i]]$TYPE)>0))*
            (tmp$pos[[i]]$UDLY-tmp$pos[[i]]$Strike)*(-tmp$pos[[i]]$TYPE)*PosMultip*tmp$pos[[i]]$Position
        )
      )
    }else{
      profit_<-c(profit_,
                 sum(
                   as.numeric(((tmp$pos[[i]]$UDLY-tmp$pos[[i]]$Strike)*(-tmp$pos[[i]]$TYPE)>0))*
                     (tmp$pos[[i]]$UDLY-tmp$pos[[i]]$Strike)*(-tmp$pos[[i]]$TYPE)*PosMultip*tmp$pos[[i]]$Position
                 )
      )
    }
  }
  profit_<-profit_+iniCredit
  intr_val<-data.frame(day=day_,UDLY=udly_,profit=profit_)
  intr_val
}

createPriceTbl<-function(days,pos_smry,credit){
  pos_smry$UDLY
  pos_smry$Price+credit
  
  pricetbl<-data.frame(day=days,UDLY=pos_smry$UDLY, profit=(pos_smry$Price+credit))
  pricetbl
}

createGreekTbl<-function(days,pos_smry_x,pos_smry_greek,credit){ 
  greektbl<-data.frame(day=days,x=pos_smry_x, greek=(pos_smry_greek+credit))
  greektbl
}


#innfer functions : position operation related.

#After each posTable is created by createPositinEvalTable(), 
#we must adjust actual Date and related conditions. Date (and TimeToExpDate), 
#Moneyness.nm,, IV(OrigIV) and time decayed Greeks.

#assuming on each day, IVIDX doesn't change at udlChgPcg==0.
#instance price change had only occured from stepdays before.

#nested. adjustPosChg() calls adjustPosChgInner()
adjustPosChgInner<-function(process_df,time_advcd){
  pos<-as.data.frame(process_df$pos[1])
  if(sum(as.numeric(time_advcd==0))!=0) return(pos)
#  print(pos)

  #Volatility Cone の影響。時間変化した分の影響を受ける。その比の分だけ比率変化
  #ATMIV_pos <- ATMIV_pos*(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pre/(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pos
   bdays_per_month<-252/12
   TimeToExpDate_pos<-(pos$TimeToExpDate*bdays_per_month-time_advcd)/bdays_per_month
   pos$ATMIV<-pos$ATMIV *
     get.Volatility.Cone.Regression.Result(pos$TYPE,TimeToExpDate_pos)/
     get.Volatility.Cone.Regression.Result(pos$TYPE,pos$TimeToExpDate)
  
  #set new TimeToExpDate
   pos$TimeToExpDate<-TimeToExpDate_pos
  
  #Date advance
   pos$Date <- format(advance("UnitedStates/NYSE",dates=as.Date(pos$Date,format="%Y/%m/%d"),
                              time_advcd,0),"%Y/%m/%d")

  #Moneyness.nm which reflects the value of TimetoExpDate
  pos$Moneyness.Frac<-pos$Strike/pos$UDLY
  eval_timeToExpDate<-as.numeric(pos$TimeToExpDate<TimeToExp_Limit_Closeness_G)*TimeToExp_Limit_Closeness_G+
    as.numeric(pos$TimeToExpDate>=TimeToExp_Limit_Closeness_G)*pos$TimeToExpDate
  pos$Moneyness.Nm<-log(pos$Moneyness.Frac)/pos$ATMIV/sqrt(eval_timeToExpDate)
  pos$Moneyness.Frac<-NULL

  
  #calculate IV_pos(OrigIV) using SkewModel based on model definition formula.
   get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
   pos$ATMIV*get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
   pos$OrigIV<-pos$ATMIV*get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
  
  #calculate pption price and thier greeks
   vgreeks<-set.EuropeanOptionValueGreeks(pos)
   pos$Price<-vgreeks$Price
   pos$Delta<-vgreeks$Delta
   pos$Gamma<-vgreeks$Gamma
   pos$Vega<-vgreeks$Vega
   pos$Theta<-vgreeks$Theta
   pos$Rho<-vgreeks$Rho
  
  pos
}

adjustPosChg<-function(process_df,time_advcd){
  print(process_df)
  print(time_advcd)
  
  process_df %>% group_by(udlChgPct) %>% do(pos=adjustPosChgInner(.,time_advcd)) -> process_df
  
  ##
  #  Greeks
  #
  #  UDLY
  process_df %>% rowwise() %>% do(UDLY=mean(.$pos$UDLY)) ->tmp
  unlist(tmp$UDLY)->tmp ; process_df$UDLY <- tmp ;rm(tmp)
  #  Price
  process_df %>% rowwise() %>% do(Price=getPosGreeks(pos=.$pos$Position,greek=.$pos$Price)) ->tmp
  unlist(tmp$Price)->tmp ; process_df$Price <- tmp ;rm(tmp)
  #  Delta
  process_df %>% rowwise() %>% do(Delta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Delta))->tmp
  unlist(tmp$Delta)->tmp ; process_df$Delta <- tmp ;rm(tmp)
  #  Gamma
  process_df %>% rowwise() %>% do(Gamma=getPosGreeks(pos=.$pos$Position,greek=.$pos$Gamma))->tmp
  unlist(tmp$Gamma)->tmp ; process_df$Gamma <- tmp ;rm(tmp)
  #  Vega
  process_df %>% rowwise() %>% do(Vega=getPosGreeks(pos=.$pos$Position,greek=.$pos$Vega))->tmp
  unlist(tmp$Vega)->tmp ; process_df$Vega <- tmp ;rm(tmp)
  #  Theta
  process_df %>% rowwise() %>% do(Theta=getPosGreeks(pos=.$pos$Position,greek=.$pos$Theta)) ->tmp
  unlist(tmp$Theta)->tmp ; process_df$Theta <- tmp ;rm(tmp)
  
  return(process_df)
}


