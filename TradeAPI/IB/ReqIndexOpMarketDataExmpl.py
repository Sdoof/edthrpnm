# coding: utf-8
from ib.ext.Contract import Contract
from ib.opt import Connection
from time import sleep
from datetime import date, datetime, timedelta
import csv

# -- globals  ------------------------------------------------------------------
port_G = 7496
clientId_G = 678

nextOrderId = -1
contractDetail = None
contractRestoreList = None
orderIdMktReqContractDict = None
priceInfoDict = {}

SPX_Strike_Max=2450
SPX_Strike_Min=1000
RUT_Strike_Max=1500
RUT_Strike_Min=700

class ContractPrice:
    def __init__(self):
        self.bid = ""
        self.ask = ""
        self.last = ""
        self.high = ""
        self.low = ""
        self.close = ""
        self.Contract = ""


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
    global orderIdMktReqContractDict
    global priceInfoDict
    # field: 1 = bid, 2 = ask, 4 = last, 6 = high, 7 = low, 9 = close
    print(str(msg))
    # print(orderIdMktReqContractDict.keys())
    contRtrvd = orderIdMktReqContractDict[msg.tickerId]
    print('Ticker reqid %s con_id %s \"%s\" %s %s' % (msg.tickerId, contRtrvd.m_conId, contRtrvd.m_localSymbol,
                                                      contRtrvd.m_strike, contRtrvd.m_expiry))
    contract_price = priceInfoDict.get(contRtrvd.m_localSymbol)
    if contract_price == None:
        contract_price = ContractPrice()
    contract_price.Contract = contRtrvd
    if msg.field == 1:
        contract_price.bid = msg.price
    if msg.field == 2:
        contract_price.ask = msg.price
    if msg.field == 4:
        contract_price.last = msg.price
    if msg.field == 9:
        contract_price.close = msg.price
    priceInfoDict[contRtrvd.m_localSymbol] = contract_price
    print('\"%s\" expDate %s bid %s ask %s last %s close %s' % (contRtrvd.m_localSymbol, contRtrvd.m_expiry,
                                                                priceInfoDict[contRtrvd.m_localSymbol].bid,
                                                                priceInfoDict[contRtrvd.m_localSymbol].ask,
                                                                priceInfoDict[contRtrvd.m_localSymbol].last,
                                                                priceInfoDict[contRtrvd.m_localSymbol].close))



# -- factory functions  -----------------------------------------------------------------

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

def makeIndexContract(sym, exchange):
    newOptContract = Contract()
    newOptContract.m_symbol = sym
    newOptContract.m_secType = "IND"
    newOptContract.m_expiry = ""
    newOptContract.m_strike = ""
    newOptContract.m_right = ""
    newOptContract.m_exchange = exchange
    newOptContract.m_currency = "USD"
    return newOptContract

def subscribeContractList(opCont,con):
    global nextOrderId
    global SPX_Strike_Max, SPX_Strike_Min, RUT_Strike_Max,RUT_Strike_Min
    nextOrderId = nextOrderId + 1
    theOrderId = nextOrderId
    con.reqContractDetails(theOrderId, opCont)
    #raw_input('wait for contractDetail press to continue')
    sleep(2.5)
    contractRemoveList = []
    for con_item in range(len(contractRestoreList)):
        con_each = contractRestoreList[con_item]
        #print('retrieved conid %s \"%s\" %s %s %s %s %s x %s on %s' %
        #      (con_each.m_conId, con_each.m_localSymbol, con_each.m_secType, con_each.m_right, con_each.m_strike,
        #       con_each.m_expiry, con_each.m_symbol, con_each.m_multiplier, con_each.m_exchange))
        ### exclude condition
        #if (con_each.m_strike % 10) != 0:
        #    contractRemoveList.append(con_each)
        if con_each.m_secType == 'OPT' and con_each.m_symbol == 'SPX' and con_each.m_strike < SPX_Strike_Min:
            contractRemoveList.append(con_each)
        elif con_each.m_secType == 'OPT' and con_each.m_symbol == 'SPX' and con_each.m_strike > SPX_Strike_Max:
            contractRemoveList.append(con_each)
        elif con_each.m_secType == 'OPT' and con_each.m_symbol == 'RUT' and con_each.m_strike < RUT_Strike_Min:
            contractRemoveList.append(con_each)
        elif con_each.m_secType == 'OPT' and con_each.m_symbol == 'RUT' and con_each.m_strike > RUT_Strike_Max:
            contractRemoveList.append(con_each)
    # Remove
    for con_rmv_item in range(len(contractRemoveList)):
        con_rmv = contractRemoveList[con_rmv_item]
        contractRestoreList.remove(con_rmv)

def subscribeDataRequest(con):
    global nextOrderId, orderIdMktReqContractDict
    global contractRestoreList, orderIdMktReqContractDict
    orderIdMktReqContractDict = {}
    for con_item in range(len(contractRestoreList)):
        nextOrderId = nextOrderId + 1
        theOrderId = nextOrderId
        con_each = contractRestoreList[con_item]
        # In the case of snapshot, no need to store orderId to the Dict?
        # con.reqMktData(tickerId=theOrderId,contract=contractRestoreList[0],genericTickList='',snapshot=True)
        # market data streaming
        orderIdMktReqContractDict[theOrderId] = con_each
        #print('dict %s %s' % (theOrderId, orderIdMktReqContractDict[theOrderId].m_conId))
        # sleep(1)
        con.reqMktData(tickerId=theOrderId, contract=con_each, genericTickList='', snapshot=False)
        # print('request market data [%s] for conId %s' % (theOrderId, orderIdMktReqContractDict[theOrderId].m_conId))
    #print('examing orderIdMktReqContractDict press any to continue')
    #for req_order_id in orderIdMktReqContractDict.iterkeys():
    #    print('req_order_id ', req_order_id)

def writeToFile(sectype,symbol):
    global priceInfoDict
    if sectype == 'OPT':
        fname = "C:/Users/kuby/edthrpnm/MarketData/" + symbol + sectype + datetime.now().strftime("%Y-%m-%d") + ".csv"
    elif sectype == 'IND':
        fname = "C:/Users/kuby/edthrpnm/MarketData/" + symbol + sectype + ".csv"
    else:
        fname = "C:/Users/kuby/edthrpnm/MarketData/" + symbol + sectype + ".csv"
    file = open(fname, mode='a')
    writer_csv = csv.writer(file, lineterminator="\n",quoting=csv.QUOTE_NONNUMERIC)
    if sectype == 'OPT':
        writer_csv.writerow(["Strike","ContactName","Last","Bid","Ask","ExpDate","TYPE"])
    for priceInfo_key in priceInfoDict.iterkeys():
        print('priceInfoKey %s' % (priceInfo_key))
        contract_price = priceInfoDict[priceInfo_key]
        contract = contract_price.Contract
        print('Strike %s ContactName %s last %s bid %s ask %s ExpDate %s TYPE_S %s' %
              (contract.m_strike, contract.m_localSymbol, contract_price.last,
               contract_price.bid, contract_price.ask, contract.m_expiry, contract.m_right))
        if sectype == 'OPT' and contract_price.last == '':
            contract_price.last = 0.0
        if sectype == 'OPT' and (contract_price.bid < 0 or contract_price.ask < 0) :
            continue
        if sectype == 'OPT' and contract.m_right == 'P':
            contract.m_right = '1'
        elif sectype == 'OPT' and contract.m_right == 'C':
             contract.m_right = '-1'
        #contract.m_expiry = datetime.datetime.strptime(contract.m_expiry,'%Y%m%d').strftime('%Y/%m/%d')
        if sectype == 'OPT' and symbol == contract.m_symbol:
            try:
                writer_csv.writerow(
                    [str(contract.m_strike), contract.m_localSymbol, str(format(contract_price.last,'.10f')), str(format(contract_price.bid,'.10f')),
                     str(format(contract_price.ask,'.10f')), datetime.strptime(contract.m_expiry,'%Y%m%d').strftime('%Y/%m/%d'), contract.m_right])
            except:
                continue
        elif sectype == 'IND' and symbol == contract.m_symbol:
            try:
                writer_csv.writerow(
                    [str(contract_price.last),str(contract_price.close),datetime.now().strftime('%Y/%m/%d %H:%M:%S')])
            except:
                continue
    file.close()

# -- main  ---------------------------------------------------------------------
#18 elements finish in 4min15s
opContractList = [
    makeOptContract(sym='SPX', exp='20160818', strike='', right='P'),
    makeOptContract(sym='SPX', exp='20160818', strike='', right='C'),
    makeOptContract(sym='SPX', exp='20160831', strike='', right='P'),
    makeOptContract(sym='SPX', exp='20160831', strike='', right='C'),
    makeOptContract(sym='SPX', exp='20160915', strike='', right='P'),
    makeOptContract(sym='SPX', exp='20160915', strike='', right='C'),
    makeOptContract(sym='SPX', exp='20160930', strike='', right='P'),
    makeOptContract(sym='SPX', exp='20160930', strike='', right='C'),
    makeOptContract(sym='SPX', exp='20161020', strike='', right='P'),
    makeOptContract(sym='SPX', exp='20161020', strike='', right='C'),
    makeOptContract(sym='SPX', exp='20161031', strike='', right='P'),
    makeOptContract(sym='SPX', exp='20161031', strike='', right='C'),
    makeOptContract(sym='SPX', exp='20161117', strike='', right='P'),
    makeOptContract(sym='SPX', exp='20161117', strike='', right='C'),
    makeOptContract(sym='SPX', exp='20161130', strike='', right='P'),
    makeOptContract(sym='SPX', exp='20161130', strike='', right='C')
]

idxContractList = [
                   makeIndexContract(sym='SPX',exchange='CBOE'),
                   #makeIndexContract(sym='RUT',exchange='RUSSELL'),
                   makeIndexContract(sym='VIX',exchange='CBOE')
                   #makeIndexContract(sym='RVX',exchange='CBOE')
                   ]

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
    #raw_input('wait for nextOrderId')
    con.reqIds(1)
    sleep(1)

    #raw_input('getting Fx Future Option Contract press any to continue')
    print('getting Fx Future Option Contract')

    #Option Contract
    for opContract_item in range(len(opContractList)):
        contractRestoreList = []
        orderIdMktReqContractDict = {}
        priceInfoDict = {}
        OpContract = opContractList[opContract_item]
        subscribeContractList(OpContract,con)
        #raw_input('requesting Fx Futre Option length press any to continue ')
        subscribeDataRequest(con)
        #raw_input('cancel mktData %s press any to continue' % (orderIdMktReqContractDict.keys()))
        sleep(12)
        for req_order_id in orderIdMktReqContractDict.iterkeys():
            con.cancelMktData(req_order_id)
        #raw_input('Price data writing to file press to continue')
        print('Price data writing to file press to continue')
        sleep(2)
        con.disconnect()
        con = Connection.create(port=port_G, clientId=clientId_G)
        # con.registerAll(MessageHandler)
        con.register(ErrorHandler, 'Error')
        con.register(NextValidIdHandler, 'NextValidId')
        con.register(MultiContractDetailsHandler, 'ContractDetails')
        con.register(OrderStatusHandler, 'OrderStatus')
        con.register(TickPriceHandler, 'TickPrice')
        con.connect()
        con.setServerLogLevel(5)
        con.reqIds(1)
        writeToFile(OpContract.m_secType,OpContract.m_symbol)

    # INDEX
    sleep(3)
    for idxContractList_item in range(len(idxContractList)):
        contractRestoreList = []
        orderIdMktReqContractDict = {}
        priceInfoDict = {}
        idxContract = idxContractList[idxContractList_item]
        subscribeContractList(idxContract,con)
        #raw_input('requesting Fx Futre Option length press any to continue ')
        subscribeDataRequest(con)
        sleep(2)
        #raw_input('cancel mktData %s press any to continue' % (orderIdMktReqContractDict.keys()))
        for req_order_id in orderIdMktReqContractDict.iterkeys():
            con.cancelMktData(req_order_id)
        #raw_input('Price data writing to file press to continue')
        sleep(2)
        con.disconnect()
        con = Connection.create(port=port_G, clientId=clientId_G)
        # con.registerAll(MessageHandler)
        con.register(ErrorHandler, 'Error')
        con.register(NextValidIdHandler, 'NextValidId')
        con.register(MultiContractDetailsHandler, 'ContractDetails')
        con.register(OrderStatusHandler, 'OrderStatus')
        con.register(TickPriceHandler, 'TickPrice')
        con.connect()
        con.setServerLogLevel(5)
        con.reqIds(1)
        writeToFile(idxContract.m_secType,idxContract.m_symbol)
    print('About to exit press any to continue')

    # Receive the new OrderId sequence from the IB Server
    con.reqIds(1)
    sleep(2)

    # disconnect
    con.disconnect()
    sleep(3)
