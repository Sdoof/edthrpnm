library(RQuantLib)

#1. Simple Payoff
# x-lay:UDLY, y-lay: Price
#When T advances like T+7, T+14,T+21,T+28、
# IF volatility remains constant(adjust horizental volatility skew only)
#  At T+X, get the position's price assuming UDLY target the price without 
#  probability consideration, only payoff.
#Namely for example, at T+7
# sum(T7$Position*T7$Price) at UDLY at original price. Likewise,
# sum(T7Mns[[i]]$Position*T7Mns[[i]]$Price) at lower than original price.
# sum(T7Pls[[i]]$Position*T7Pls[[i]]$Price) at higher than original price.



#2. Expected Return Scenario Stimulation
#Advance T 1 by 1. Get Payoff Dist. Calc Expected Payoff.
#The most basic price movement is Geometric Random Walk.
#
#  If.  1. Price moves with Geometric Random Walk
#       2. volatility remains constant
#       3. There is no vertical/horizontal/price movement
#          volatility skews
#  Expected Payoff should be the Price position values at T+x day.
# 
#  But We should investigate: 
#    1. The Effect of volatility Skewness
#    2. The Effect of volatility Changes. Up trend/Down trend
#    3. The Effect of Underlying Price Trend
#    4. The Effect of Mechanical Position Adjust(Liquidation)
#       when Forward Risk/Return Ration becomes unfavorable.
#
#  We use only T0. then Monte-Carlo based on various scenario.
#

# Import various volatility skew functions included in other files.


#Back Test Functions.
#To Be Designed Later.