##
# Data preproceccing, preparing, bridge between Excel and R
##

#Read Excel CSV file and reformat as "OptionVariables format", saves as a CSV file.
#OptionVariables format is 
#  Position  ContactName	Date	ExpDate	TYPE	UDLY	Strike	Price	..(next line)
#  Delta	Gamma	Vega	Theta	Rho	OrigIV	IV

# We must read both (UDL)_HIST.csv and (UDLOPChain)_Pre.csv
# UDLY row must be merged. Following example must be referred.

#OPtionOpr/EOptionOprEuro 's functions should be pre-read
#When setting IV and Greeks
#EOPtionStimulations set.EuropeanOptionValueGreeks, etc should be also read.

#id     <- c("A","A","C","E")
#height <- c(158,152,177,166)
#D1     <- data.frame(ID=id, H=height)
#id     <- c("A","B","c","D","E")
#weight <- c(51, 55,56,57, 55)
#D2     <- data.frame(ID=id, W=weight)
#merge(D1, D2, all.x=T)
#以下で置き換えて考える
#D1<-OPChainPre
#D2<-data.frame(HIST$Date,HIST$Close)

