# coding: utf-8
from ib.ext.Contract import Contract
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

def makeFxFutContract(sym, exp, multip):
    newFutContract = Contract()
    newFutContract.m_symbol = sym
    newFutContract.m_secType = "FUT"
    newFutContract.m_expiry = exp
    newFutContract.m_multiplier = multip
    newFutContract.m_exchange = "GLOBEX"
    newFutContract.m_currency = "USD"
    return newFutContract

def makeFxFutOptContract(sym, exp, strike, right, multip):
    newOptContract = Contract()
    newOptContract.m_symbol = sym
    newOptContract.m_secType = "FOP"
    newOptContract.m_expiry = exp
    newOptContract.m_strike = strike
    newOptContract.m_right = right
    newOptContract.m_multiplier = multip
    newOptContract.m_exchange = "GLOBEX"
    newOptContract.m_currency = "USD"
    return newOptContract

# -- main  ---------------------------------------------------------------------

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
    raw_input('getting Fx Future Option Contract press any to continue')

    # Fx Futre Option Contract
    fxFutOpContract = makeFxFutOptContract(sym='JPY', exp='201511', strike='', right='', multip='')
    nextOrderId = nextOrderId + 1
    theOrderId = nextOrderId
    con.reqContractDetails(theOrderId, fxFutOpContract)
    raw_input('wait for contractDetail')
    print('conId %s' % (contractRestoreList[0].m_conId))
    raw_input('show contractList press any to continue')

    for con_item in range(len(contractRestoreList)):
        con_each = contractRestoreList[con_item]
        print('retrieved conid %s \"%s\" %s %s %s %s %s x %s on %s' %
              (con_each.m_conId, con_each.m_localSymbol, con_each.m_secType, con_each.m_right,con_each.m_strike,
               con_each.m_expiry, con_each.m_symbol, con_each.m_multiplier, con_each.m_exchange))
        # Contract member variables:
        # m_conId, m_symbol, m_secType, m_expiry, m_strike, m_right, m_multiplier, m_exchange, m_currency, m_localSymbol,
        # m_tradingClass,m_primaryExch, m_includeExpired, m_secIdType, m_secId

    # Request Data
    raw_input('requesting Fx Futre Option  press any to continue')

    for con_item in range(len(contractRestoreList)):
        nextOrderId = nextOrderId + 1
        theOrderId = nextOrderId
        con_each = contractRestoreList[con_item]
        sleep(1)
        # In the case of snapshot, no need to store orderId to the Dict?
        # con.reqMktData(tickerId=theOrderId,contract=contractRestoreList[0],genericTickList='',snapshot=True)
        # market data streaming
        if orderIdMktReqContractDict:
            orderIdMktReqContractDict = {theOrderId: con_each}
        else:
            orderIdMktReqContractDict = dict([(theOrderId,con_each)])
        con.reqMktData(tickerId=theOrderId, contract=con_each, genericTickList='', snapshot=False)
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