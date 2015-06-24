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
  AllEffect<-EvalScore$DeltaEffect+EvalScore$VegaEffect+EvalScore$DeltaEffect+EvalScore$ThetaEffect+EvalScore$GammaEffect
  if(AllEffect<-100)
    return(TRUE)
  else
    return(FALSE)
  
}

for(Counter in evalPosStart:evalPosEnd){
  fn<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_modelStimRawlist_",Counter,sep="")
  load(file=fn)
  #modelStimRawlist corresponds to a specific Spread
  ScenarioNum<-length(modelStimRawlist$stimrslt)
  for(scenario_idx in 1:ScenarioNum){
    SimuNum<-length(modelStimRawlist$stimrslt[[ScenarioNum]])
    for(sim_idx in 1:SimuNum){
      #theSimulation<-modelStimRawlist$stimrslt[[ScenarioNum]][[sim_idx]]
      theIniEvalScore<-modelStimRawlist$stimrslt[[ScenarioNum]][[sim_idx]]$IniEvalScore
      StimDays<-length(modelStimRawlist$stimrslt[[ScenarioNum]][[sim_idx]]$EvalScore)
      for(ith_day in 1:StimDays){
        theEvalScore<-modelStimRawlist$stimrslt[[ScenarioNum]][[sim_idx]]$EvalScore[[ith_day]]
        ##print(theEvalScore)
        exitDecision(theIniEvalScore,theEvalScore)
      }    
    }
    
  }
  #print(modelStimRawlist$stimrslt)
  #$AdjustDay
}
