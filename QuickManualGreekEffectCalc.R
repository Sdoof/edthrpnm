#Load library and Configfile Setting in EOptimize.R beforehand
#Set the Value below
IVIDX=0.23
UDLY=1880.23
gamma= (-0.0007)
theta= (0.4647)
delta= (-0.11)

#Calc starts here
hdd=EvalFuncSetting$holdDays
deviation_sd2expct_convratio=0.7978846
rlzdvol_td=IVIDX*EvalFuncSetting$HV_IV_Adjust_Ratio
rlzdvol_td <- rlzdvol_td*deviation_sd2expct_convratio
#Gamme Effect
expPriceChange<-mean(UDLY*(exp(rlzdvol_td*sqrt(hdd/252))-1))
gammaEfct<-gamma*(expPriceChange^2)/2*PosMultip
#Theta Effect
thetaEfct<-hdd*theta*PosMultip
#Delta Effect
deltaEfct<-(-abs(delta))*expPriceChange*PosMultip

#print Each Effect
(gammaEfct)
(thetaEfct)
(deltaEfct)
