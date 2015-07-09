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
drawGrktbl %>% dplyr::filter(UDLY>mean(thePosition$UDLY)*(1-0.08)) %>% 
  dplyr::filter(UDLY<mean(thePosition$UDLY)*(1+0.08)) -> drawGrktbl
#Profit and Greeks Combined Graph
gg<-ggplot(drawGrktbl,aes(x=UDLY,y=profit,group=day))
(
  gg
  +geom_line(size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DeltaEffect,colour="blue",size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$GammaEffect,colour="red",size=0.9-0.01*round(drawGrktbl$day/stepdays),group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$VegaEffect,colour="green",size=0.9-0.01*round(drawGrktbl$day/stepdays),group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$ThetaEffect,colour="orange",size=0.9-0.01*round(drawGrktbl$day/stepdays),group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
  +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$DeltaEffect,size=4.0,colour="blue")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$GammaEffect,size=4.0,colour="red")
  +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$VegaEffect,size=4.0,colour="green")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$ThetaEffect,size=4.0,colour="orange")
  +ylim(
    min(c(min(drawGrktbl$ThetaEffect),min(drawGrktbl$DeltaEffect),min(drawGrktbl$GammaEffect),min(drawGrktbl$VegaEffect),min(drawGrktbl$profit))),
    max(c(max(drawGrktbl$ThetaEffect),max(drawGrktbl$DeltaEffect),max(drawGrktbl$GammaEffect),max(drawGrktbl$VegaEffect),max(drawGrktbl$profit))))
)

#Profit + Directional + Non Didectional(Intrinsic Advantageous) + Total Effect Graph
gg<-ggplot(drawGrktbl,aes(x=UDLY,y=profit,group=day))
(
  gg + geom_line(size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="black")
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$TotalEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="darkgreen") 
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$NdEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="red")
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="blue")
  +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
  +geom_point(x=thePositonGrks$UDLY,
              y=thePositonGrks$DeltaEffect+thePositonGrks$VegaEffect+thePositonGrks$GammaEffect+thePositonGrks$ThetaEffect,
              size=4.0,colour="darkgreen")
  +geom_point(x=thePositonGrks$UDLY,
              y=thePositonGrks$GammaEffect+thePositonGrks$ThetaEffect,
              size=4.0,colour="red")
  +geom_point(x=thePositonGrks$UDLY,
              y=thePositonGrks$DeltaEffect+thePositonGrks$VegaEffect,
              size=4.0,colour="blue")
  +ylim( 
    min(c(min(drawGrktbl$TotalEffect),min(drawGrktbl$NdEffect),min(drawGrktbl$DEffect),min(drawGrktbl$profit))), 
    max(c(max(drawGrktbl$TotalEffect),max(drawGrktbl$NdEffect),max(drawGrktbl$DEffect),max(drawGrktbl$profit))))
)

#Greeks Only Graph
gg<-ggplot(drawGrktbl,aes(x=UDLY,y=ThetaEffect,group=day))
(
  gg + geom_line(size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="orange",linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DeltaEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="blue",group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$GammaEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="red",group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$VegaEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="green",group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
  +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$DeltaEffect,size=4.0,colour="blue")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$GammaEffect,size=4.0,colour="red")
  +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$VegaEffect,size=4.0,colour="green")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$ThetaEffect,size=4.0,colour="orange")
  +ylim(
    min(c(min(drawGrktbl$ThetaEffect),min(drawGrktbl$DeltaEffect),min(drawGrktbl$GammaEffect),min(drawGrktbl$VegaEffect))),
    max(c(max(drawGrktbl$ThetaEffect),max(drawGrktbl$DeltaEffect),max(drawGrktbl$GammaEffect),max(drawGrktbl$VegaEffect))))
)

rm(gg,drawGrktbl)
rm(stepdays,pos_anlys,totalstep,udlStepNum,udlStepPct,vol_chg,iniPrice,iniCredit,iniDelta)
rm(posStepDays,posStepDays_vc,thePosition,thePositonGrks)
