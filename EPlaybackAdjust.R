library(ggplot2)
library(RQuantLib)
library(dplyr)

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

#Multipler of Position
PosMultip=as.numeric(ConfigParameters["PosMultip",1])

#Adjust Trading Target Spreads
AdjustSpreads=eval(parse(text=gsub("\\$",",",ConfigParameters["PlaybackAdjustSpreads",1])))

#Delta Hedge target spread IDs
DhSpreads=eval(parse(text=gsub("\\$",",",ConfigParameters["PlaybackDeltaHedgeSpreds",1])))

#ScenarioMode
ScenarioMode=ConfigParameters["PlaybackScenarioMode",1]
#ScenarioMode="_modelScenario_"
#ScenarioMode="_adjustedScenario_"


DeltaHedge<-function(){
  for(SpreadID in DhSpreads){
    #load modelStimRawlist and modelScenario.
    #modelStimRawlist and modelScenario correspond to the specific Spread
    fn<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_modelStimRawlist_",SpreadID,sep="")
    load(file=fn)
    fn<-paste(ResultFiles_Path_G,Underying_Symbol_G,ScenarioMode,SpreadID,sep="")
    load(file=fn)
    
    #process each scenario
    ScenarioNum<-length(modelStimRawlist$stimrslt)
    for(scenario_idx in 1:ScenarioNum){
      start_t<-proc.time()
      SimNum<-length(modelStimRawlist$stimrslt[[scenario_idx]])
      #process each simulation
      for(sim_idx in 1:SimNum){
        theIniEvalScore<-modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$IniEvalScore
        ExitDay<-modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$AdjustDay
        ##
        # prepare data structure for the sim_idx'th simulation
        # At the end of 'Day'  -> today's 'Payoff' -> new Delta hedged('HeadgedDelta')
        #   at the end of the Day, the Day's p/l from yesterday is Payoff.
        #   based on the last value of today's delta, adjust the hedge ratio and the result of the delta is HedgedDelta.
        DeltaHedgeScore=data.frame(Day=rep(1:ExitDay),Payoff=rep(0,times=ExitDay),
                                   HedgedDelta=rep(0,times=ExitDay))
        IniDeltaHedgeScore=data.frame(Day=0,Payoff=0,HedgedDelta=-theIniEvalScore$Delta)
        theProfit<-modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$Profit
        newEvalScore<-vector("list",ExitDay)
        ##
        # process each day
        for(ith_day in 1:ExitDay){
          theEvalScore<-modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$EvalScore[[ith_day]]
          lastEvalScore<-(ith_day==1)*theIniEvalScore+(ith_day>=2)*modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$EvalScore[[
            (ith_day>=2)*(ith_day-1)+(ith_day==1)*1]]
          lastDeltaHedgeScoreRow<-(ith_day==1)*IniDeltaHedgeScore+(ith_day>=2)*DeltaHedgeScore[(ith_day>=2)*(ith_day-1)+(ith_day==1)*1,]
          lastHedgedDelta<-lastDeltaHedgeScoreRow$HedgedDelta
          
          newEvalScore[[ith_day]]<-theEvalScore
          #Payoff by yesterday's headged delta
          DeltaHedgeScore[ith_day,]$Payoff<-lastHedgedDelta/100*(theEvalScore$UDLY-lastEvalScore$UDLY)*PosMultip
          #First assume the same delta hedged position held.
          DeltaHedgeScore[ith_day,]$HedgedDelta<-lastHedgedDelta
          
          #On ExitDay, do not need to adjust and make new Hedge. Just exit the yesterday's headge.
          if(ith_day==ExitDay){
            DeltaHedgeScore[ith_day,]$HedgedDelta<-0
            break
          }
          
          #Delta Hedage performed only the Entry day.
          DeltaHedagedEntrydayOnly<-FALSE
          if(DeltaHedagedEntrydayOnly)
            next
          
          #Adjust Delta Hedge causing condition
          DeltaHedgeThresh_Max=0
          DeltaHedgeThresh_Min=(-80)
          if(theEvalScore$Delta>DeltaHedgeThresh_Max)
            next
          if(theEvalScore$Delta<DeltaHedgeThresh_Min)
            next
          
          #Adjust hedge ratio and new Delta is calculated
          DeltaHedgeScore[ith_day,]$HedgedDelta<-(-1)*theEvalScore$Delta
          newDelta<-theEvalScore$Delta+DeltaHedgeScore[ith_day,]$HedgedDelta
          newEvalScore[[ith_day]]$Delta<-newDelta
          if(theEvalScore$Delta!=0){
            #新しいDeltaEffectを計算するために、expPriceChangeを元のEvalScoreから逆算する
            theExpPriceChange<-(-theEvalScore$DeltaEffect/abs(theEvalScore$Delta))
            #new Delta and DeltaEffect updatedd
            newEvalScore[[ith_day]]$DeltaEffect<-(-abs(newDelta))*theExpPriceChange
          }
        }#Exit day
        
        #新しいProfitとEvalScoreの計算と更新
        newProfit<-theProfit+cumsum(DeltaHedgeScore$Payoff)
        for(i in 1:ExitDay){ newEvalScore[[i]]$Price<-newProfit[i]+theIniEvalScore$Price }
        #Update
        modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$EvalScore<-newEvalScore
        modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$Profit<-newProfit
      } #EOF SimNum
      cat(" scenario ",scenario_idx, " time: ",(proc.time()-start_t)[3])
    } #EOF every Scenario
    ##
    # modefied modelStimRawlist$stimrslt is to be reflected
    
    #show the profit profiles before the reflection
    modelScenario %>% select(min_profit,max_profit,mean_profit,median_profit,profit_sd) %>% print()
    data.frame(min_profit=min(modelScenario$min_profit),max_profit=max(modelScenario$max_profit),
               expected_profit=sum(modelScenario$weight*modelScenario$mean_profit)) %>% print()
    
    #reflection each-scenario-rowwise()
    #first resdf
    modelStimRawlist %>% rowwise() %>% do(resdf=getStimResultDataFrame(.$stimrslt,SimuNum)) -> tmp
    modelScenario$resdf<-tmp$resdf ; rm(tmp)
    #second profit profiles
    modelScenario %>% rowwise() %>% do(min_profit=min(.$resdf$profit),max_profit=max(.$resdf$profit),
                                       mean_profit=mean(.$resdf$profit),median_profit=median(.$resdf$profit),
                                       profit_sd=sd(.$resdf$profit)) -> tmp
    modelScenario$min_profit<-unlist(tmp$min_profit)
    modelScenario$max_profit<-unlist(tmp$max_profit)
    modelScenario$mean_profit<-unlist(tmp$mean_profit)
    modelScenario$median_profit<-unlist(tmp$median_profit)
    modelScenario$profit_sd<-unlist(tmp$profit_sd)
    
    #show the profit profiles after the reflection
    #modelScenario %>% rowwise() %>% do(.$resdf %>% print()  )
    modelScenario %>% select(min_profit,max_profit,mean_profit,median_profit,profit_sd) %>% print()
    data.frame(min_profit=min(modelScenario$min_profit),max_profit=max(modelScenario$max_profit),
               expected_profit=sum(modelScenario$weight*modelScenario$mean_profit)) %>% print()
    
    fn<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_adjustedScenario_",SpreadID,sep="")
    save(modelScenario,file=fn)
  } #EOF every Spread
}
DeltaHedge()

exitDecision<-function(IniEvalScore,EvalScore){
  AllEffect<-EvalScore$DeltaEffect+EvalScore$VegaEffect+EvalScore$ThetaEffect+EvalScore$GammaEffect
  Profit<-EvalScore$Price-IniEvalScore$Price
  
  ##
  # Decision based on linear combination of GreekEfeect and Profit, and these compound conditions
  # linear combination: a1*DeltaEffect+a2*VegaEffect+a3*ThetaEffect+a4*GammaEffect+a5*Profit
  # compound conditions: if(comb1<200) ... else if(comb2>300).. else ...
  
  ## ThetaEffect+GammaEffect < Thresh
  #AdvantageousEfct<-EvalScore$ThetaEffect+EvalScore$GammaEffect
  #if(AdvantageousEfct<(0))
  #  return(TRUE)
  #else
  #  return(FALSE)
  
  ## ThetaEffect+GammaEffect < Thresh also dislike undirectional uncertaintity
  AdvantageousEfct<-EvalScore$ThetaEffect+EvalScore$GammaEffect
  #DirectionalEft<-EvalScore$DeltaEffect+EvalScore$VegaEffect
  if(AdvantageousEfct<(-50))
    return(TRUE)
  # else if((DirectionalEft<(-1800)) && (AdvantageousEfct<(300)))
  else if(AllEffect<(-1500))
    return(TRUE)
  else
    return(FALSE)
  
  ## AllEffect < Thresh
  #if(AllEffect<(-1000))
  #  return(TRUE)
  #else
  #  return(FALSE)
  
  ## Profit > Thresh
  #   if(Profit>=600)
  #     return(TRUE)
  #   else
  #     return(FALSE)
  
  ## Profit < Thresh
  #   if(Profit<0)
  #     return(TRUE)
  #   else
  #     return(FALSE)
  
  ## Maybe depends on each situation.
  # (Profit > Thresh1) OR (AllEffect<Thresh2)
  #        if(Profit>=550)
  #          return(TRUE)
  #        else if(AllEffect<(-1000))
  #          return(TRUE)
  #        else
  #          return(FALSE)
  
  ## Not yet tested.
  ## (Profit < Thresh1) OR (AllEffect<Thresh2)
  #     if(Profit<0)
  #       return(TRUE)
  #     else if(AllEffect<(-1200))
  #       return(TRUE)
  #     else
  #       return(FALSE)
  
  ## -1*AllEffect+Profit > Thresh
  #   AllScore<-AllEffect*(-1)+Profit
  #   if(AllScore>=1200)
  #     return(TRUE)
  #   else
  #     return(FALSE)
  
  ## AllEffect+Profit < Thresh
  #AllScore2<-AllEffect+Profit
  #if(AllScore2>(-800))
  #  return(TRUE)
  #else
  #  return(FALSE)
}

PlaybackAdjust<-function(){
  for(Counter in AdjustSpreads){
    #load modelStimRawlist and modelScenario.
    #modelStimRawlist and modelScenario correspond to the specific Spread
    fn<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_modelStimRawlist_",Counter,sep="")
    load(file=fn)
    fn<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_modelScenario_",Counter,sep="")
    load(file=fn)
    
    #process each scenario
    ScenarioNum<-length(modelStimRawlist$stimrslt)
    for(scenario_idx in 1:ScenarioNum){
      start_t<-proc.time()
      SimuNum<-length(modelStimRawlist$stimrslt[[scenario_idx]])
      #process each simulation
      for(sim_idx in 1:SimuNum){
        #theIniEvalScore<-modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$IniEvalScore
        StimDays<-length(modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$EvalScore)
        #process each day
        for(ith_day in 1:StimDays){
          #theEvalScore<-modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$EvalScore[[ith_day]]
          if(exitDecision(modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$IniEvalScore,
                          modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$EvalScore[[ith_day]])){
            modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$AdjustDay<-ith_day
            break
          }    
        }
      }
      cat(" scenario ",scenario_idx, " time: ",(proc.time()-start_t)[3])
    }
    ##
    # modefied modelStimRawlist$stimrslt is to be reflected
    
    #show the profit profiles before the reflection
    modelScenario %>% select(min_profit,max_profit,mean_profit,median_profit,profit_sd) %>% print()
    data.frame(min_profit=min(modelScenario$min_profit),max_profit=max(modelScenario$max_profit),
               expected_profit=sum(modelScenario$weight*modelScenario$mean_profit)) %>% print()
    
    #reflection each-scenario-rowwise()
    #first resdf
    modelStimRawlist %>% rowwise() %>% do(resdf=getStimResultDataFrame(.$stimrslt,SimuNum)) -> tmp
    modelScenario$resdf<-tmp$resdf ; rm(tmp)
    #second profit profiles
    modelScenario %>% rowwise() %>% do(min_profit=min(.$resdf$profit),max_profit=max(.$resdf$profit),
                                       mean_profit=mean(.$resdf$profit),median_profit=median(.$resdf$profit),
                                       profit_sd=sd(.$resdf$profit)) -> tmp
    modelScenario$min_profit<-unlist(tmp$min_profit)
    modelScenario$max_profit<-unlist(tmp$max_profit)
    modelScenario$mean_profit<-unlist(tmp$mean_profit)
    modelScenario$median_profit<-unlist(tmp$median_profit)
    modelScenario$profit_sd<-unlist(tmp$profit_sd)
    
    #show the profit profiles after the reflection
    #modelScenario %>% rowwise() %>% do(.$resdf %>% print()  )
    modelScenario %>% select(min_profit,max_profit,mean_profit,median_profit,profit_sd) %>% print()
    data.frame(min_profit=min(modelScenario$min_profit),max_profit=max(modelScenario$max_profit),
               expected_profit=sum(modelScenario$weight*modelScenario$mean_profit)) %>% print()
    
    fn<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_adjustedScenario_",Counter,sep="")
    save(modelScenario,file=fn)
  }
}

#PlaybackAdjust()

rm(ConfigFileName_G,ConfigParameters)
rm(DataFiles_Path_G,ResultFiles_Path_G,Underying_Symbol_G,evalPosStart,evalPosEnd)
rm(exitDecision,PlaybackAdjust)