library(ggplot2)
library(RQuantLib)
library(plyr)
library(dplyr)

#File
Underying_Symbol_G="RUT"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"
ResultFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\ResultData\\"

#Evaluatin Table Position start
evalPosStart<-1

#Evaluatin Table Position end
evalPosEnd<-1

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
  for(Counter in evalPosStart:evalPosEnd){
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

PlaybackAdjust()

rm(DataFiles_Path_G,ResultFiles_Path_G,Underying_Symbol_G,evalPosStart,evalPosEnd)
rm(exitDecision,PlaybackAdjust)
