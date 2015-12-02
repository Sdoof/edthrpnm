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

EUR_Strike_Max=1.5
EUR_Strike_Min=0.7
JPY_Strike_Max=0.010
JPY_Strike_Min=0.006
GBP_Strike_Max=1.8
GBP_Strike_Min=1.3

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

def subscribeContractList(opCont,con):
    global nextOrderId
    global EUR_Strike_Max, EUR_Strike_Min, JPY_Strike_Max, JPY_Strike_Min, GBP_Strike_Max, GBP_Strike_Min
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
        # Need just 6J
        if 'XJ' in con_each.m_localSymbol:
            contractRemoveList.append(con_each)
        elif 'XT' in con_each.m_localSymbol:
            contractRemoveList.append(con_each)
        elif 'XB' in con_each.m_localSymbol:
            contractRemoveList.append(con_each)
        elif con_each.m_secType == 'FOP' and con_each.m_symbol == 'JPY' and con_each.m_strike < JPY_Strike_Min:
            contractRemoveList.append(con_each)
        elif con_each.m_secType == 'FOP' and con_each.m_symbol == 'JPY' and con_each.m_strike > JPY_Strike_Max:
            contractRemoveList.append(con_each)
        elif con_each.m_secType == 'FOP' and con_each.m_symbol == 'EUR' and con_each.m_strike < EUR_Strike_Min:
            contractRemoveList.append(con_each)
        elif con_each.m_secType == 'FOP' and con_each.m_symbol == 'EUR' and con_each.m_strike > EUR_Strike_Max:
            contractRemoveList.append(con_each)
        elif con_each.m_secType == 'FOP' and con_each.m_symbol == 'GBP' and con_each.m_strike < GBP_Strike_Min:
            contractRemoveList.append(con_each)
        elif con_each.m_secType == 'FOP' and con_each.m_symbol == 'GBP' and con_each.m_strike > GBP_Strike_Max:
            contractRemoveList.append(con_each)
    # Remove XJX
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
    #raw_input('examing orderIdMktReqContractDict press any to continue')
    #for req_order_id in orderIdMktReqContractDict.iterkeys():
    #    print('req_order_id ', req_order_id)

def writeToFile(sectype,symbol):
    global priceInfoDict
    if sectype == 'FOP':
        fname = "C:/Users/kuby/edthrpnm/MarketData/" + symbol + sectype + datetime.now().strftime("%Y-%m-%d") + ".csv"
    elif sectype == 'FUT':
        fname = "C:/Users/kuby/edthrpnm/MarketData/" + symbol + sectype + ".csv"
    else:
        fname = "C:/Users/kuby/edthrpnm/MarketData/" + symbol + sectype + ".csv"

    file = open(fname, mode='a')
    writer_csv = csv.writer(file, lineterminator="\n",quoting=csv.QUOTE_NONNUMERIC)
    if sectype == 'FOP':
        writer_csv.writerow(["Strike","ContactName","Last","Bid","Ask","ExpDate","TYPE"])
    #if sectype == 'FUT':
    #    writer_csv.writerow(["ContactName","Last","Bid","Ask","ExpDate","Date"])
    for priceInfo_key in priceInfoDict.iterkeys():
        print('priceInfoKey %s' % (priceInfo_key))
        contract_price = priceInfoDict[priceInfo_key]
        contract = contract_price.Contract
        print('Strike %s ContactName %s last %s bid %s ask %s ExpDate %s TYPE %s' %
              (contract.m_strike, contract.m_localSymbol, contract_price.last,
               contract_price.bid, contract_price.ask, contract.m_expiry, contract.m_right))
        if contract_price.last == '':
            contract_price.last = (contract_price.bid + contract_price.ask)/2.0
        if contract_price.bid < 0 or contract_price.ask < 0 :
            continue
        if sectype == 'FOP':
            if contract.m_right == 'P':
                contract.m_right = '1'
            elif contract.m_right == 'C':
                contract.m_right = '-1'
        #contract.m_expiry = datetime.datetime.strptime(contract.m_expiry,'%Y%m%d').strftime('%Y/%m/%d')
        if sectype == 'FOP' and symbol == contract.m_symbol:
            writer_csv.writerow(
                [str(contract.m_strike), contract.m_localSymbol, str(format(contract_price.last,'.10f')), str(format(contract_price.bid,'.10f')),
                 str(format(contract_price.ask,'.10f')), datetime.strptime(contract.m_expiry,'%Y%m%d').strftime('%Y/%m/%d'), contract.m_right])
        elif sectype == 'FUT' and symbol == contract.m_symbol:
            writer_csv.writerow(
                [contract.m_localSymbol, str(format(contract_price.last,'.10f')), str(format(contract_price.bid,'.10f')), str(format(contract_price.ask,'.10f')),
                 datetime.strptime(contract.m_expiry,'%Y%m%d').strftime('%Y/%m/%d'),(datetime.now() - timedelta(1)).strftime('%Y/%m/%d')])
    file.close()

# -- main  ---------------------------------------------------------------------
fxFutOpContractList = [
                       makeFxFutOptContract(sym='JPY', exp='20160108', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='JPY', exp='20160108', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='JPY', exp='20160108', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='EUR', exp='20160108', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='EUR', exp='20160108', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='GBP', exp='20160108', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='GBP', exp='20160108', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='JPY', exp='20160205', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='JPY', exp='20160205', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='EUR', exp='20160205', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='EUR', exp='20160205', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='GBP', exp='20160205', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='GBP', exp='20160205', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='JPY', exp='20160304', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='JPY', exp='20160304', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='EUR', exp='20160304', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='EUR', exp='20160304', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='GBP', exp='20160304', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='GBP', exp='20160304', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='JPY', exp='20160603', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='JPY', exp='20160603', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='EUR', exp='20160603', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='EUR', exp='20160603', strike='', right='P', multip=''),
                       makeFxFutOptContract(sym='GBP', exp='20160603', strike='', right='C', multip=''),
                       makeFxFutOptContract(sym='GBP', exp='20160603', strike='', right='P', multip='')
                       ]

fxFutContractList = [
                     makeFxFutContract(sym='JPY', exp='201512', multip=''),
                     makeFxFutContract(sym='EUR', exp='201512', multip=''),
                     makeFxFutContract(sym='GBP', exp='201512', multip=''),
                     makeFxFutContract(sym='JPY', exp='201603', multip=''),
                     makeFxFutContract(sym='EUR', exp='201603', multip=''),
                     makeFxFutContract(sym='GBP', exp='201603', multip=''),
                     makeFxFutContract(sym='JPY', exp='201606', multip=''),
                     makeFxFutContract(sym='EUR', exp='201606', multip=''),
                     makeFxFutContract(sym='GBP', exp='201606', multip='')
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

    # Fx Future Option Contract Data
    for fxFutOpContract_item in range(len(fxFutOpContractList)):
        contractRestoreList = []
        orderIdMktReqContractDict = {}
        priceInfoDict = {}
        fxFutOpContract = fxFutOpContractList[fxFutOpContract_item]
        subscribeContractList(fxFutOpContract,con)
        #raw_input('requesting Fx Futre Option length press any to continue ')
        subscribeDataRequest(con)
        sleep(8)
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
        writeToFile(fxFutOpContract.m_secType,fxFutOpContract.m_symbol)

    # Fx Future Contract Data
    sleep(3)
    for fxFutContract_item in range(len(fxFutContractList)):
        contractRestoreList = []
        orderIdMktReqContractDict = {}
        priceInfoDict = {}
        fxFutContract = fxFutContractList[fxFutContract_item]
        subscribeContractList(fxFutContract,con)
        #raw_input('requesting Fx Futre Option length press any to continue ')
        subscribeDataRequest(con)
        sleep(5)
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
        writeToFile(fxFutContract.m_secType,fxFutContract.m_symbol)

    print('About to exit press any to continue')

    # Receive the new OrderId sequence from the IB Server
    con.reqIds(1)
    sleep(2)

    # disconnect
    con.disconnect()
    sleep(3)
