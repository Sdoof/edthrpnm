# coding: utf-8
from ib.ext.Order import Order
from ib.ext.Contract import Contract
from ib.ext.ComboLeg import ComboLeg
from ib.opt import Connection
from time import sleep

# -- globals  ------------------------------------------------------------------
port_G = 7496
clientId_G = 678

nextOrderId = -1
contractDetail = None
contractRestoreList = None
orderIdMktReqContractDict = None


# -- message handlers  ---------------------------------------------------------
def MessageHandler(msg):
    print msg


def ErrorHandler(msg):
    print(str(msg))


def NextValidIdHandler(msg):
    global nextOrderId
    print(str(msg))
    nextOrderId = msg.orderId


def ContractDetailsHandler(msg):
    global contractDetail
    print(str(msg))
    contractDetail = msg.contractDetails


def MultiContractDetailsHandler(msg):
    global contractRestoreList
    theContractdetail = msg.contractDetails
    theContract = theContractdetail.m_summary
    print('<MultiContracts reqid %s conId %s: %s: %s: %s: %s: %s: %s>' % (
        msg.reqId, theContract.m_conId, theContract.m_localSymbol, theContract.m_right,
        theContract.m_strike, theContract.m_expiry, theContract.m_exchange, theContract.m_symbol))
    if contractRestoreList:
        contractRestoreList.append(theContract)
    else:
        contractRestoreList = [theContract]


def OrderStatusHandler(msg):
    print(str(msg))
    print('<%s:%s:%s:%s:%s:%s:%s>' % (
        msg.orderId, msg.typeName, msg.status, msg.whyHeld, msg.avgFillPrice, msg.filled, msg.remaining))

def TickPriceHandler(msg):
    # field: 1 = bid 2 = ask 4 = last 6 = high 7 = low 9 = close
    print(str(msg))


# -- functions  -----------------------------------------------------------------

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


def makeComboLeg(conId, action, ratio):
    newComboLeg = ComboLeg()
    newComboLeg.m_conId = conId
    newComboLeg.m_ratio = ratio
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


def placeSpreadOrder(Leg, BuySell, ComboRatio, LimitPrice, QTY):
    global contractDetail, con, nextOrderId
    LegContract = None

    for legitem in range(len(Leg)):
        theOrderId = nextOrderId
        con.reqContractDetails(theOrderId, Leg[legitem])
        print('ContactDetal requested ' + str(theOrderId))
        # wait for TWS message to come back to message handler
        raw_input('wait for contractDetail')
        if LegContract:
            LegContract.append(contractDetail.m_summary)
        else:
            LegContract = [contractDetail.m_summary]
        print("=> [{0} ({1})] Call/Put: {2} Strike: {3} Expiration: {4}".format(
            LegContract[legitem].m_localSymbol, LegContract[legitem].m_conId,
            LegContract[legitem].m_right, LegContract[legitem].m_strike, LegContract[legitem].m_expiry))
        Leg[legitem].m_conId = LegContract[legitem].m_conId
        raw_input('press any to continue conId: ' + str(Leg[legitem].m_conId))
    raw_input('now instantiate each leg press to continue')
    # instantiate each leg
    LegsList = None
    for legitem in range(len(Leg)):
        if LegsList:
            LegsList.append(makeComboLeg(Leg[legitem].m_conId, BuySell[legitem], ComboRatio[legitem]))
        else:
            LegsList = [makeComboLeg(Leg[legitem].m_conId, BuySell[legitem], ComboRatio[legitem])]
    # build a bag with these legs
    BagContract = makeBagContract(LegsList)
    # combination legs
    comboOrder = makeOrder(action="BUY", qty=QTY, price=LimitPrice)
    # place order
    nextOrderId = nextOrderId + 1
    theOrderId = nextOrderId
    con.placeOrder(theOrderId, BagContract, comboOrder)
    print('bag contract order placed ' + str(theOrderId))

# -- main  ---------------------------------------------------------------------
# First Leg
Leg = [makeOptContract("IBM", "201509", 155, "C"), makeOptContract("IBM", "201510", 155, "C"),
       makeOptContract("IBM", "201510", 150, "P"), makeOptContract("IBM", "201509", 150, "P")]
BuySell = ["SELL", "BUY", "SELL", "BUY"]
ComboRatio = [1, 2, 2, 1]
LimitPrice_G = 0.2
QTY_G = 1

# Second Leg
Leg2 = [makeOptContract("IBM", "201509", 160, "C"), makeOptContract("IBM", "201510", 160, "C"),
       makeOptContract("IBM", "201510", 150, "P"), makeOptContract("IBM", "201509", 148, "P")]
BuySell2 = ["BUY", "SELL", "BUY", "SELL"]
ComboRatio2 = [2, 1, 1, 2]
LimitPrice2_G = 0.03
QTY2_G = 1

if __name__ == '__main__':
    for legitem in range(len(Leg)):
        print("=> %s %s x [%s] %s Call/Put: %s Strike: %s Expiration: %s" %
              (BuySell[legitem], ComboRatio[legitem], Leg[legitem].m_secType, Leg[legitem].m_symbol,
               Leg[legitem].m_right, Leg[legitem].m_strike, Leg[legitem].m_expiry))
    raw_input('Spread leg info correct?')

    # Server Access
    con = Connection.create(port=port_G, clientId=clientId_G)
    # con.registerAll(MessageHandler)
    con.register(ErrorHandler, 'Error')
    con.register(NextValidIdHandler, 'NextValidId')
    con.register(ContractDetailsHandler, 'ContractDetails')
    con.register(OrderStatusHandler, 'OrderStatus')
    con.connect()
    con.setServerLogLevel(5)

    # get mext Order Id
    raw_input('wait for nextOrderId')
    con.reqIds(1)

    # place Spread Order
    # first spread
    placeSpreadOrder(Leg, BuySell, ComboRatio, LimitPrice_G, QTY_G)
    # second spread
    if len(Leg2) != 0 :
        raw_input('Going into next Leg press to continue')
        for legitem in range(len(Leg)):
            print("=> %s %s x [%s] %s Call/Put: %s Strike: %s Expiration: %s" %
              (BuySell2[legitem], ComboRatio2[legitem], Leg2[legitem].m_secType, Leg2[legitem].m_symbol,
               Leg2[legitem].m_right, Leg2[legitem].m_strike, Leg2[legitem].m_expiry))
        raw_input('Spread leg info correct?')
        placeSpreadOrder(Leg2, BuySell2, ComboRatio2, LimitPrice2_G, QTY2_G)
    # request Order Status
    con.reqOpenOrders()

    # Retrieve Option Chain Contract
    raw_input('getting data retrieval press any to continue')
    con.unregister(ContractDetailsHandler, 'ContractDetails')
    con.register(MultiContractDetailsHandler, 'ContractDetails')

    # contract list
    # opchainContract = makeOptContract(sym='SPX', exp='201509', strike='', right='')

    # just one contract
    opchainContract = makeOptContract(sym='SPX', exp='20150917', strike='2000', right='P')
    nextOrderId = nextOrderId + 1
    theOrderId = nextOrderId
    con.reqContractDetails(theOrderId, opchainContract)
    raw_input('wait for contractDetail')
    print('conId %s' % (contractRestoreList[0].m_conId))

    # Request Data
    con.register(TickPriceHandler, 'TickPrice')

    nextOrderId = nextOrderId + 1
    theOrderId = nextOrderId
    # In the case of snapshot, no need to store orderId to the Dict?
    # con.reqMktData(tickerId=theOrderId,contract=contractRestoreList[0],genericTickList='',snapshot=True)
    # market data streaming
    con.reqMktData(tickerId=theOrderId, contract=contractRestoreList[0], genericTickList='', snapshot=False)
    # orderIdMktReqContractDict = dict([(theOrderId,contractRestoreList[0])])
    orderIdMktReqContractDict = {theOrderId: contractRestoreList[0]}
    print('request market data [%s] for conId %s' % (theOrderId, orderIdMktReqContractDict[theOrderId].m_conId))

    # Cancel First Data
    raw_input('cancel first mktData %s press any to continue' % (orderIdMktReqContractDict.keys()[0]))
    con.cancelMktData(orderIdMktReqContractDict.keys()[0])

    # Receive the new OrderId sequence from the IB Server
    con.reqIds(1)
    sleep(2)

    # disconnect
    con.disconnect()
    sleep(3)
