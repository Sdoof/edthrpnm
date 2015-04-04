library(dplyr)
library(RQuantLib)
library(ggplot2)

# set days interval between which the position is analyzed step by step.
stepdays<-3
udlStepNum<-80
udlStepPct=0.005

#get evaluation days vector, evaldays
opchain$Date<-as.character(opchain$Date);
opchain$ExpDate<-as.character(opchain$ExpDate)
max_days<-min(get.busdays.between(opchain$Date,opchain$ExpDate))
totalstep=max_days%/%stepdays 
evaldays<-rep(stepdays,times=totalstep)
evaldays<- cumsum(evaldays)
#if the lastday is ExpDate, we set the lastday as (lastday-1), 
# because the ExpDate option price is unstable.
if(evaldays[length(evaldays)]==max_days){
  if(stepdays==1){
    evaldays<-evaldays[-length(evaldays)]
  }else{
    evaldays[length(evaldays)]<-evaldays[length(evaldays)]-1
  }
}
rm(max_days)
#read analyzed positon. here we give by copy and paste
pos_anlys<-evaPos
 
#note opchain is already instanciated by the proceduces of ERskRtnEval
opchain$Position<-pos_anlys
opchain %>% dplyr::filter(Position!=0) -> thePosition

#thePosition's greek df and initial price
thePositonGrks<-getPositionGreeks(thePosition)
iniPrice <- thePositonGrks$Price
iniCredit <- -1*iniPrice

#total data frame
posStepDays<-data.frame(days=evaldays)

#Set data frames as a row value of another data frame.
posStepDays %>% group_by(days) %>%
  do(scene=createPositinEvalTable(position=thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,days=stepdays)) -> posStepDays
posStepDays_vc<-posStepDays
#We must adjust each position values
posStepDays %>% group_by(days) %>% rowwise() %>% do(days=.$days,scene2=adjustPosChg(.$scene,.$days-stepdays,base_vol_chg=0)) -> tmp
unlist(tmp$days) -> posStepDays$days ; tmp$scene2 -> posStepDays$scene ;rm(tmp)
#We've got the complete posStepDays.

####volatility change scenario if needed
# set the percent by which volatility index (IVIDX) changes. Up(%)>0 Down(%)<0
#vol_chg<-0.2
#posStepDays_vc %>% group_by(days) %>% rowwise() %>% do(days=.$days,scene2=adjustPosChg(.$scene,.$days-stepdays,base_vol_chg=vol_chg)) -> tmp
#unlist(tmp$days) -> posStepDays_vc$days ; tmp$scene2 -> posStepDays_vc$scene ;rm(tmp)

#Now drawing
drawGrktbl<-createAgrregatedGreekTbl(posStepDays,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,multi=PosMultip,iniCredit=iniCredit)

##Delta Hedge Effect
if(FALSE){
  #initial Delta
  iniDelta<-getPosGreeks(pos=thePosition$Position,greek=thePosition$Delta,multi=PosMultip)
  #new Profit
  drawGrktbl$profit<-drawGrktbl$profit-as.numeric(iniDelta)*(drawGrktbl$UDLY-mean(thePosition$UDLY))
  #new DeltaEffect
   #temporal. you shoud get IVIX and calculte #expPriceChange<-mean(UDLY*(exp(ividx_td*sqrt(hdd/365))-1))
  expPriceChange<-as.numeric(drawGrktbl$Delta!=0)*drawGrktbl$DeltaEffect/(-abs(drawGrktbl$Delta))
  drawGrktbl$DeltaEffect<-drawGrktbl$DeltaEffect-as.numeric(iniDelta)*expPriceChange
  rm(expPriceChange)
  #new delta
  drawGrktbl$Delta<-drawGrktbl$Delta-as.numeric(iniDelta)
}
#Effect Aggregation
drawGrktbl %>% dplyr::mutate(TotalEffect=ThetaEffect+DeltaEffect+GammaEffect+VegaEffect) -> drawGrktbl
drawGrktbl %>% dplyr::mutate(NdEffect=ThetaEffect+GammaEffect) -> drawGrktbl
drawGrktbl %>% dplyr::mutate(DEffect=DeltaEffect+VegaEffect) -> drawGrktbl

## Drawing profit
#limit the UDLY range.
#Combined Graph
drawGrktbl %>% dplyr::filter(UDLY>mean(thePosition$UDLY)*(1-0.08)) %>% 
  dplyr::filter(UDLY<mean(thePosition$UDLY)*(1+0.08)) -> drawGrktbl
gg<-ggplot(drawGrktbl,aes(x=UDLY,y=profit,group=day))
(
  gg
  + geom_line(size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays))
  + geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DeltaEffect,colour="blue",size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays))
  + geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$GammaEffect,colour="red",size=0.9-0.01*round(drawGrktbl$day/stepdays),group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  + geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$VegaEffect,colour="green",size=0.9-0.01*round(drawGrktbl$day/stepdays),group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  + geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$ThetaEffect,colour="orange",size=0.9-0.01*round(drawGrktbl$day/stepdays),group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_point(x=mean(thePosition$UDLY),y=0,size=3.5,colour="green")
  +ylim(
    min(c(min(drawGrktbl$ThetaEffect),min(drawGrktbl$DeltaEffect),min(drawGrktbl$GammaEffect),min(drawGrktbl$VegaEffect),min(drawGrktbl$profit))),
    max(c(max(drawGrktbl$ThetaEffect),max(drawGrktbl$DeltaEffect),max(drawGrktbl$GammaEffect),max(drawGrktbl$VegaEffect),max(drawGrktbl$profit))))
)
gg<-ggplot(drawGrktbl,aes(x=UDLY,y=ThetaEffect,group=day))
(
  gg + geom_line(size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="orange",linetype=round(drawGrktbl$day/stepdays))
  + geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DeltaEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="blue",group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  + geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$GammaEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="red",group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  + geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$VegaEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="green",group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  + geom_point(x=mean(thePosition$UDLY),y=0,size=3.5)
  +ylim(
    min(c(min(drawGrktbl$ThetaEffect),min(drawGrktbl$DeltaEffect),min(drawGrktbl$GammaEffect),min(drawGrktbl$VegaEffect))),
    max(c(max(drawGrktbl$ThetaEffect),max(drawGrktbl$DeltaEffect),max(drawGrktbl$GammaEffect),max(drawGrktbl$VegaEffect))))
)

#ThetaEffect+DeltaEffect+GammaEffect+VegaEffect
gg<-ggplot(drawGrktbl,aes(x=UDLY,y=TotalEffect,group=day))
(
  gg + geom_line(size=0.9-0.05*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="black")
  + geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$NdEffect,size=0.9-0.05*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="red")
  + geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DEffect,size=0.9-0.05*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="orange")
  + geom_point(x=mean(thePosition$UDLY),y=0,size=3.5)
  +ylim( 
     min(c(min(drawGrktbl$TotalEffect),min(drawGrktbl$NdEffect))), 
     max(c(max(drawGrktbl$TotalEffect),max(drawGrktbl$NdEffect))))
)

rm(gg,drawGrktbl)
rm(stepdays,pos_anlys,totalstep,udlStepNum,udlStepPct,vol_chg,iniPrice,iniCredit,iniDelta)
rm(posStepDays,posStepDays_vc,thePosition,thePositonGrks)

##
# Functions to be loaded --------

#inner functions : graphical related.
#create Aggregated Price Table for Drawing
createAgrregatedGreekTbl<-function(posStepDays,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,multi=PosMultip,iniCredit=iniCredit){
  
  #Delta
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Delta)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% dplyr::rename(UDLY=x,Delta=greek) -> greek_tbl
  agr_tbl <- greek_tbl
  
  #Gamma
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Gamma)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,Gamma=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #Vega
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Vega)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,Vega=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #Theta
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$Theta)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,Theta=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #ThetaEffect
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$ThetaEffect)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,ThetaEffect=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #GammaEffect
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$GammaEffect)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,GammaEffect=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #DeltaEffect
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$DeltaEffect)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,DeltaEffect=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #VegaEffect
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createGreekTbl(.$days,.$scene$UDLY,.$scene$VegaEffect)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  greek_tbl %>% rename(UDLY=x,VegaEffect=greek) -> greek_tbl
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  #profit
  posStepDays %>% group_by(days) %>% rowwise() %>% do(ptbl=createPriceTbl(.$days,.$scene,iniCredit)) -> tmp
  greek_tbl<-full_join(tmp$ptbl[[1]],tmp$ptbl[[2]])
  for(i in 2:length(tmp$ptbl)){
    greek_tbl<-full_join(greek_tbl,tmp$ptbl[[i]])
  }
  agr_tbl  %>% left_join(greek_tbl)  -> agr_tbl
  
  agr_tbl
}

getThePositionDrawtable <- function (posStepDays, thePosition, udlStepNum=udlStepNum, udlStepPct=udlStepPct, PosMultip=PosMultip) {
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


createGreekTbl<-function(days,pos_smry_x,pos_smry_greek){ 
  greektbl<-data.frame(day=days,x=pos_smry_x, greek=pos_smry_greek)
  greektbl
}


#innfer functions : position operation related.

#After each posTable is created by createPositinEvalTable(), 
#we must adjust actual Date and related conditions. Date (and TimeToExpDate), 
#Moneyness.nm,, IV(OrigIV) and time decayed Greeks.

#assuming on each day, IVIDX doesn't change at udlChgPcg==0.
#instance price change had only occured from stepdays before.

#nested. adjustPosChg() calls adjustPosChgInner()
adjustPosChgInner<-function(process_df,time_advcd, base_vol_chg=0){
  pos<-as.data.frame(process_df$pos[1])
  if(sum(as.numeric(time_advcd==0))!=0) return(pos)
  #print(pos)

  #base volatility change relrected to IVIDX
  ividx_chg_pct<-as.numeric(base_vol_chg)
  pos$IVIDX<-pos$IVIDX*(1+ividx_chg_pct)

  # ATM IV change
  pos$ATMIV<-pos$ATMIV*(1+ividx_chg_pct)*get.Volatility.Change.Regression.Result(pos,ividx_chg_pct)

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

adjustPosChg<-function(process_df,time_advcd,base_vol_chg=0){
  print(process_df)
  print(time_advcd)
  
  process_df %>% group_by(udlChgPct) %>% do(pos=adjustPosChgInner(.,time_advcd,base_vol_chg=base_vol_chg)) -> process_df
  
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
  
  ##
  # Greek Effects
  
  # ThetaEffect
  process_df %>% rowwise() %>% do(ThetaEffect=getThetaEffect(pos=.$pos$Position,greek=.$pos$Theta)) -> tmp
  unlist(tmp$ThetaEffect)->tmp ; process_df$ThetaEffect <- tmp ;rm(tmp)
  # DeltaEffect
  process_df %>% rowwise() %>% do(DeltaEffect=getDeltaEffect(pos=.$pos$Position,greek=.$pos$Delta,
                                                             UDLY=.$pos$UDLY,
                                                             ividx_td=getIV_td(.$pos$IVIDX))) -> tmp
  unlist(tmp$DeltaEffect)->tmp ; process_df$DeltaEffect <- tmp ;rm(tmp)
  # GammaEffect
  process_df %>% rowwise() %>% do(GammaEffect=getGammaEffect(pos=.$pos$Position,greek=.$pos$Gamma,
                                                             UDLY=.$pos$UDLY,
                                                             ividx_td=getIV_td(.$pos$IVIDX))) -> tmp
  unlist(tmp$GammaEffect)->tmp ; process_df$GammaEffect <- tmp ;rm(tmp)
  # VegaEffect
  process_df %>% rowwise() %>% do(VegaEffect=getVegaEffect(pos=.$pos$Position,greek=.$pos$Vega,
                                                           ividx=getIV_td(.$pos$IVIDX)
                                                           #dviv should be precalulated when optimized
                                                           ,dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)) -> tmp
  unlist(tmp$VegaEffect)->tmp ; process_df$VegaEffect <- tmp ;rm(tmp)
  
  return(process_df)
}

#One position's greeks are retuned as a data frame which has only one row.
getPositionGreeks<-function(position,multi=PosMultip){
  price<-getPosGreeks(pos=position$Position,greek=position$Price,multi=PosMultip)
  delta<-getPosGreeks(pos=position$Position,greek=position$Delta,multi=PosMultip)
  gamma<-getPosGreeks(pos=position$Position,greek=position$Gamma,multi=PosMultip)
  theta<-getPosGreeks(pos=position$Position,greek=position$Theta,multi=PosMultip)
  vega<-getPosGreeks(pos=position$Position,greek=position$Vega,multi=PosMultip)
  udly<-mean(position$UDLY)
  
  thetaEffect<-getThetaEffect(pos=position$Position,greek=position$Theta)
  vegaEffect<-getVegaEffect(pos=position$Position,greek=position$Vega,
                            ividx=getIV_td(position$IVIDX),dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)
  deltaEffect<-getDeltaEffect(pos=position$Position,greek=position$Delta,
                              UDLY=position$UDLY,ividx_td=getIV_td(position$IVIDX))
  
  gammaEffect<-getGammaEffect(pos=position$Position,greek=position$Gamma,
                              UDLY=position$UDLY,ividx_td=getIV_td(position$IVIDX))
  
  #DTRRR<-getDTRRR(position=position)
  #VTRRR<-getVTRRR(position=position,
  #                ividx=getIV_td(position$IVIDX),
  #                dviv=annuual.daily.volatility(getIV_td(histIV$IVIDX))$daily)
  
  data.frame(Price=price,Delta=delta,Gamma=gamma,Theta=theta,Vega=vega,UDLY=udly,
             ThetaEffect=thetaEffect,GammaEffect=gammaEffect,DeltaEffect=deltaEffect,VegaEffect=vegaEffect)
             #,DTRRR=DTRRR,VTRRR=VTRRR)
  
}
