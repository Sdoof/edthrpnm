# coding: utf-8
from ib.ext.Order import Order
from ib.ext.Contract import Contract
from ib.ext.ComboLeg import ComboLeg
from ib.opt import Connection
from time import sleep
from collections import deque
import sys

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

def makeFxFutContract(sym, exp):
    newFutContract = Contract()
    newFutContract.m_symbol = sym
    newFutContract.m_secType = "FUT"
    newFutContract.m_expiry = exp
    newFutContract.m_multiplier = ""
    newFutContract.m_exchange = "GLOBEX"
    newFutContract.m_currency = "USD"
    return newFutContract

def makeFxFutOptContract(sym, exp, strike, right):
    newOptContract = Contract()
    newOptContract.m_symbol = sym
    newOptContract.m_secType = "FOP"
    newOptContract.m_expiry = exp
    newOptContract.m_strike = strike
    newOptContract.m_right = right
    newOptContract.m_multiplier = ""
    newOptContract.m_exchange = "GLOBEX"
    newOptContract.m_currency = "USD"
    return newOptContract

def makeComboLeg(conId, action, ratio):
    newComboLeg = ComboLeg()
    newComboLeg.m_conId = conId
    newComboLeg.m_ratio = ratio
    newComboLeg.m_action = action
    newComboLeg.m_exchange = "GLOBEX"
    newComboLeg.m_openClose = 0
    newComboLeg.m_shortSaleSlot = 0
    newComboLeg.m_designatedLocation = ""
    return newComboLeg

def makeBagContract(legs,symbol):
    newBagContract = Contract()
    newBagContract.m_symbol = symbol
    newBagContract.m_secType = "BAG"
    newBagContract.m_exchange = "GLOBEX"
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

def placeSpreadOrder(Leg, BuySell, ComboRatio, LimitPrice, QTY, symbol):
    global contractDetail, con, nextOrderId
    LegContract = None
    for legitem in range(len(Leg)):
        nextOrderId = nextOrderId + 1
        theOrderId = nextOrderId
        con.reqContractDetails(theOrderId, Leg[legitem])
        print('ContactDetal requested (orderId: ' + str(theOrderId))
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
        raw_input('press any to process next leg (this leg conId: ' + str(Leg[legitem].m_conId))
    raw_input('now instantiate each leg press to continue')
    # instantiate each leg
    LegsList = None
    for legitem in range(len(Leg)):
        if LegsList:
            LegsList.append(makeComboLeg(LegContract[legitem].m_conId, BuySell[legitem], ComboRatio[legitem]))
        else:
            LegsList = [makeComboLeg(LegContract[legitem].m_conId, BuySell[legitem], ComboRatio[legitem])]
        print(LegContract[legitem].m_conId, BuySell[legitem], ComboRatio[legitem])
    # build a bag with these
    BagContract = makeBagContract(LegsList,symbol)
    # combination legs
    comboOrder = makeOrder(action="BUY", qty=QTY, price=LimitPrice)
    # place order
    nextOrderId = nextOrderId + 1
    theOrderId = nextOrderId
    con.placeOrder(theOrderId, BagContract, comboOrder)
    print('sending order ... wait for a moment')
    sleep(3)
    print('bag contract order placed (orderId: ' + str(theOrderId))

def placeEachLegOrder(Leg, BuySell, ComboRatio,lmtPrc):
    global contractDetail, con, nextOrderId
    for legitem in range(len(Leg)):
        nextOrderId = nextOrderId + 1
        theOrderId = nextOrderId
        con.reqContractDetails(theOrderId, Leg[legitem])
        print('ContactDetal requested (orderId: ' + str(theOrderId))
        # wait for TWS message to come back to message handler
        raw_input('wait for contractDetail')
        theContract = contractDetail.m_summary
        print("$$$ Individual Leg of Spread [{0} ({1})] Call/Put: {2} Strike: {3} Expiration: {4}".format(
            theContract.m_localSymbol, theContract.m_conId,
            theContract.m_right, theContract.m_strike, theContract.m_expiry))
        raw_input('press any to continue (conid: ' + str(theContract.m_conId))
        Leg[legitem].m_conId = theContract.m_conId
        theOrder =  makeOrder(action=BuySell[legitem], qty=ComboRatio[legitem], price=lmtPrc[legitem])
        # place order
        nextOrderId = nextOrderId + 1
        theOrderId = nextOrderId
        con.placeOrder(theOrderId, Leg[legitem], theOrder)
        print('sending order ... wait for a moment')
        sleep(3)
        print('option order placed (orderId: ' + str(theOrderId))


def processFopAPITicket(Symbol, Expiry, Strike, Right):
    Leg_new = None
    for legid in range(len(Symbol)):
        if Leg_new:
            Leg_new.append(makeFxFutOptContract(Symbol[legid], Expiry[legid], Strike[legid], Right[legid]))
        else:
            Leg_new = [makeFxFutOptContract(Symbol[legid], Expiry[legid], Strike[legid], Right[legid])]
    return Leg_new

def makeLmtPrice(BuySell,bid,ask):
    lmtPrc = None
    for legitem in range(len(BuySell)):
        if BuySell[legitem] == "BUY":
            value = bid
        else:
            value = ask
        if lmtPrc:
            lmtPrc.append(value)
        else:
            lmtPrc = [value]
    return lmtPrc

class TicketPackage:
    def __init__(self):
        self.symbolTicket = ""
        self.expiryTicket = ""
        self.strikeTicket = ""
        self.rightTicket = ""
        self.buySell = ""
        self.comboRatio = ""
        self.limitPrice_G = -9000
        self.qTY_G = 1

def createTicketPackage(symbolTicket, expiryTicket, strikeTicket, rightTicket, buySell, comboRatio,limitPrice_G, qTY_G ):
    theTicketPackage=TicketPackage()
    theTicketPackage.symbolTicket=SymbolTicket
    theTicketPackage.expiryTicket=ExpiryTicket
    theTicketPackage.strikeTicket=StrikeTicket
    theTicketPackage.rightTicket=RightTicket
    theTicketPackage.buySell=BuySell
    theTicketPackage.comboRatio=ComboRatio
    theTicketPackage.limitPrice_G=LimitPrice_G
    theTicketPackage.qTY_G=QTY_G
    return theTicketPackage

# -- globals  ------------------------------------------------------------------
port_G = 7496
clientId_G = 679

nextOrderId = -1
contractDetail = None

optionOrderBid=0.1
optionOrderAsk=600

# API Ticket generated by the edthrpnm MC Simulator
TicketPackages = []
#First Ticket

SymbolTicket = [ 'EUR','EUR' ]; ExpiryTicket = [ '20160108','20160108' ]; StrikeTicket = [ 1.045,1.055 ] ; RightTicket = [ 'P','P' ]; BuySell = [ 'BUY','BUY' ]; ComboRatio = [ 1,1 ] ;LimitPrice_G =  -9000  ;QTY_G =  1

theTicketPkg=createTicketPackage(SymbolTicket,ExpiryTicket,StrikeTicket,RightTicket,BuySell,ComboRatio,LimitPrice_G,QTY_G)
TicketPackages.append(theTicketPkg)
queTickets=deque(TicketPackages)

SymbolTicket = [ 'EUR','EUR' ]; ExpiryTicket = [ '20160205','20160205' ]; StrikeTicket = [ 1.01,1.035 ] ; RightTicket = [ 'P','P' ]; BuySell = [ 'SELL','SELL' ]; ComboRatio = [ 1,1 ] ;LimitPrice_G =  -9000  ;QTY_G =  1
theTicketPkg=createTicketPackage(SymbolTicket,ExpiryTicket,StrikeTicket,RightTicket,BuySell,ComboRatio,LimitPrice_G,QTY_G)
queTickets.append(theTicketPkg)

SymbolTicket = [ 'EUR','EUR','EUR' ]; ExpiryTicket = [ '20160108','20160108','20160205' ]; StrikeTicket = [ 1.095,1.1,1.12 ] ; RightTicket = [ 'C','C','C' ]; BuySell = [ 'SELL','SELL','BUY' ]; ComboRatio = [ 1,1,2 ] ;LimitPrice_G =  -9000  ;QTY_G =  1
theTicketPkg=createTicketPackage(SymbolTicket,ExpiryTicket,StrikeTicket,RightTicket,BuySell,ComboRatio,LimitPrice_G,QTY_G)
queTickets.append(theTicketPkg)

#Second Ticket


# -- main  ------------------------------------------------------------------
if __name__ == '__main__':
    #look into queTickets
    while True:
        try:
            tkpkg=queTickets.popleft()
            print(tkpkg.symbolTicket,tkpkg.expiryTicket,tkpkg.strikeTicket,tkpkg.rightTicket,
                  tkpkg.buySell,tkpkg.comboRatio,tkpkg.limitPrice_G,tkpkg.qTY_G)

            #this Ticket info
            SymbolTicket=tkpkg.symbolTicket
            ExpiryTicket=tkpkg.expiryTicket
            StrikeTicket=tkpkg.strikeTicket
            RightTicket=tkpkg.rightTicket
            BuySell=tkpkg.buySell
            ComboRatio=tkpkg.comboRatio
            LimitPrice_G=tkpkg.limitPrice_G
            QTY_G=tkpkg.qTY_G

            ## Makeing Leg Lists from APT Ticket
            Leg = processFopAPITicket(SymbolTicket, ExpiryTicket, StrikeTicket, RightTicket)
            ## Make sure each Sperad is correct
            print('------------- Spread')
            for legitem in range(len(Leg)):
                print("=> %s %s x [%s] %s Call/Put: %s Strike: %s Expiration: %s" %
                      (BuySell[legitem], ComboRatio[legitem], Leg[legitem].m_secType, Leg[legitem].m_symbol,
                       Leg[legitem].m_right, Leg[legitem].m_strike, Leg[legitem].m_expiry))
            raw_input('Spread leg info correct?')

            ## Server Access
            con = Connection.create(port=port_G, clientId=clientId_G)
            con.register(ErrorHandler, 'Error')
            con.register(NextValidIdHandler, 'NextValidId')
            con.register(ContractDetailsHandler, 'ContractDetails')
            con.register(OrderStatusHandler, 'OrderStatus')
            con.connect()
            con.setServerLogLevel(5)
            ## get mext Order Id
            raw_input('wait for nextOrderId')
            con.reqIds(1)

            ## place Spread Order
            placeSpreadOrder(Leg, BuySell, ComboRatio, LimitPrice_G, QTY_G, symbol=Leg[1].m_symbol)
            con.reqOpenOrders()
            ans = raw_input('PLACING INDIVIDUAL LEG ORDER (yes or no)?')
            if ans == "yes":
                placeEachLegOrder(Leg, BuySell, ComboRatio, makeLmtPrice(BuySell,bid=optionOrderBid,ask=optionOrderAsk))
                con.reqOpenOrders()
            ## Receive the new OrderId sequence from the IB Server
            con.reqIds(1)
            sleep(3)
            ## disconnect
            con.disconnect()
        except IndexError:
            print("IndexError")
            break
    sleep(3)
    #sys.exit(0)



