#Data Num.
DATA_NUM=252*18 # about x years equivalent 

#selective parmeters For SPX
IS_SELECTIVE_HISTIV_REGR=T
IS_SELECTIVE_WEIGHT_ESTM=T
a_low=0.9
d_low=2
a_high=1.1
d_high=2
#Smooth Spline df
DF_P2IVREG_SSPL=5.5

#selective parmeters For RUT
if(Underying_Symbol_G=="RUT"){
  IS_SELECTIVE_HISTIV_REGR=T
  IS_SELECTIVE_WEIGHT_ESTM=T
  a_low=0.75
  d_low=3
  a_high=1.3
  d_high=3
  #Smooth Spline df
  DF_P2IVREG_SSPL=5.5
}
