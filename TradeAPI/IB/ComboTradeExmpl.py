from ib.ext.Order import Order
from ib.ext.Contract import Contract
from ib.ext.ContractDetails import ContractDetails
from ib.ext.ComboLeg import ComboLeg
from ib.opt import Connection
from time import sleep

#-- globals  ------------------------------------------------------------------
nextOrderId = -1

#-- message handlers  ---------------------------------------------------------
def watcher(msg):
    print msg

def NextValidIdHandler(msg):
    global nextOrderId
    nextOrderId = msg.orderId

def ContractDetailsHandler(msg):
    global contractDetails
    contractDetails = msg.contractDetails

#-- functions  -----------------------------------------------------------------

def makeOptContract(sym, exp, strike, right):
    newOptContract = Contract()
    newOptContract.m_symbol = sym
    newOptContract.m_secType = "OPT"
    newOptContract.m_expiry = exp
    newOptContract.m_strike = strike
    newOptContract.m_right = right
    newOptContract.m_multiplier = 100
    newOptContract.m_exchange = "SMART"
    newOptContract.m_currency = "USD"
    return newOptContract

def makeComboLeg(conId, action):
    newComboLeg = ComboLeg()
    newComboLeg.m_conId = conId
    newComboLeg.m_ratio = 1
    newComboLeg.m_action = action
    newComboLeg.m_exchange = "SMART"
    newComboLeg.m_openClose = 0
    newComboLeg.m_shortSaleSlot = 0
    newComboLeg.m_designatedLocation = ""
    return newComboLeg

def makeBagContract(legs):
    newBagContract = Contract()
    newBagContract.m_symbol = "USD"
    newBagContract.m_secType = "BAG"
    newBagContract.m_exchange = "SMART"
    newBagContract.m_currency = "USD"
    newBagContract.m_comboLegs = legs
    return newBagContract

def makeOrder(action, qty, price):
    newOrder = Order()
    newOrder.m_action = action
    newOrder.m_totalQuantity = qty
    newOrder.m_orderType = "LMT"
    newOrder.m_lmtPrice = price
    newOrder.m_tif = ''
    newOrder.m_parentId = 0
    newOrder.m_discretionaryAmt = 0
    newOrder.m_transmit = False
    return newOrder

#-- main  ---------------------------------------------------------------------

if __name__ == '__main__':
    con = Connection.create(port=7496, clientId=678)
    con.registerAll(watcher)
    con.register(NextValidIdHandler, 'NextValidId')
    con.register(ContractDetailsHandler, 'ContractDetails')
    con.connect()
    con.setServerLogLevel(5)

    # define the contract for each leg
    # first short Leg
    shortLeg = makeOptContract("IBM", "201509", 155, "C")
    # get the contract ID for each leg
    print "order id" + str(nextOrderId)
    con.reqContractDetails(nextOrderId,shortLeg)
    contractDetails = ContractDetails()
    # wait for TWS message to come back to message handler
    sleep(2)
    shortLegContract = contractDetails.m_summary
    print shortLegContract.m_conId
    shortLeg.m_conId = shortLegContract.m_conId

    # second long leg
    longLeg = makeOptContract("IBM", "201510", 155, "C")
    print "order id" + str(nextOrderId)
    con.reqContractDetails(nextOrderId,longLeg)
    contractDetails = ContractDetails()
    # wait for TWS message to come back to message handler
    sleep(2)
    longLegContract = contractDetails.m_summary
    print longLegContract.m_conId
    longLegContract.m_conId = longLegContract.m_conId

    print shortLegContract.m_conId
    print longLegContract.m_conId

    shortConId = shortLegContract.m_conId
    longConId = longLegContract.m_conId

    # instantiate each leg
    shortLeg = makeComboLeg(shortConId, "SELL")
    longLeg = makeComboLeg(longConId, "BUY")

    # build a bag with these legs
    BagContract = makeBagContract([shortLeg, longLeg])

    # build order to buy 1 spread at $1.66
    comboOrder = makeOrder(action="BUY", qty=1,price=0.2)

    # place order
    con.placeOrder(nextOrderId, BagContract, comboOrder)

    # watch the messages for a bit
    sleep(20)


