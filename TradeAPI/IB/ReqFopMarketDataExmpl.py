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
    # field: 1 = bid, 2 = ask, 4 = last, 6 = high, 7 = low, 9 = close
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

# -- main  ---------------------------------------------------------------------
# First Leg

if __name__ == '__main__':
    # Server Access
    con = Connection.create(port=port_G, clientId=clientId_G)
    # con.registerAll(MessageHandler)
    con.register(ErrorHandler, 'Error')
    con.register(NextValidIdHandler, 'NextValidId')
    con.register(MultiContractDetailsHandler, 'ContractDetails')
    con.register(OrderStatusHandler, 'OrderStatus')
    con.register(TickPriceHandler, 'TickPrice')
    con.connect()
    con.setServerLogLevel(5)

    # get mext Order Id
    raw_input('wait for nextOrderId')
    con.reqIds(1)

    # Retrieve Option Chain Contract
    raw_input('getting data retrieval press any to continue')

    # option chain contract
    # opchainContract = makeOptContract(sym='SPX', exp='201509', strike='', right='')

    # just one contract
    opchainContract = makeOptContract(sym='SPX', exp='20150917', strike='2000', right='P')
    nextOrderId = nextOrderId + 1
    theOrderId = nextOrderId
    con.reqContractDetails(theOrderId, opchainContract)
    raw_input('wait for contractDetail')
    print('conId %s' % (contractRestoreList[0].m_conId))

    # Request Data
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