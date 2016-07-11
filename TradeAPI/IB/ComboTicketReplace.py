# coding: utf-8
from ib.ext.Order import Order
from ib.ext.Contract import Contract
from ib.ext.ComboLeg import ComboLeg
from ib.opt import Connection
from time import sleep
from collections import deque
import copy
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
    # build a bag with these legs
    BagContract = makeBagContract(LegsList)
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

def processAPITicket(Symbol, Expiry, Strike, Right):
    Leg_new = None
    for legid in range(len(Symbol)):
        if Leg_new:
            Leg_new.append(makeOptContract(Symbol[legid], Expiry[legid], Strike[legid], Right[legid]))
        else:
            Leg_new = [makeOptContract(Symbol[legid], Expiry[legid], Strike[legid], Right[legid])]
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


# API Ticket generated by the edthrpnm MC Simulator
TicketPackages = []
#First Ticket
SymbolTicket = [ 'SPX','SPX' ]; ExpiryTicket = [ '20160915','20160915' ]; StrikeTicket = [ 1860,1925 ] ; RightTicket = [ 'P','P' ]; BuySell = [ 'SELL','SELL' ]; ComboRatio = [ 1,1 ] ;LimitPrice_G =  -9000  ;QTY_G =  1
theTicketPkg=createTicketPackage(SymbolTicket,ExpiryTicket,StrikeTicket,RightTicket,BuySell,ComboRatio,LimitPrice_G,QTY_G)
TicketPackages.append(theTicketPkg)
queTickets=deque(TicketPackages)

#Second Ticket
SymbolTicket = [ 'SPX','SPX' ]; ExpiryTicket = [ '20161020','20161020' ]; StrikeTicket = [ 1825,1875 ] ; RightTicket = [ 'P','P' ]; BuySell = [ 'BUY','BUY' ]; ComboRatio = [ 1,1 ] ;LimitPrice_G =  -9000  ;QTY_G =  1
theTicketPkg=createTicketPackage(SymbolTicket,ExpiryTicket,StrikeTicket,RightTicket,BuySell,ComboRatio,LimitPrice_G,QTY_G)
queTickets.append(theTicketPkg)

SymbolTicket=None
# -- main  ------------------------------------------------------------------
if __name__ == '__main__':
    #look into queTickets
    while True:
        try:
            tkpkg=queTickets.popleft()
            #print(tkpkg.symbolTicket,tkpkg.expiryTicket,tkpkg.strikeTicket,tkpkg.rightTicket,
            #      tkpkg.buySell,tkpkg.comboRatio,tkpkg.limitPrice_G,tkpkg.qTY_G)

            ###
            ##  First Ticket info
            SymbolTicket=tkpkg.symbolTicket
            ExpiryTicket=tkpkg.expiryTicket
            StrikeTicket=tkpkg.strikeTicket
            RightTicket=tkpkg.rightTicket
            BuySell=tkpkg.buySell
            ComboRatio=tkpkg.comboRatio
            LimitPrice_G=tkpkg.limitPrice_G
            QTY_G=tkpkg.qTY_G

            ## Make sure the First Sperad is correct
            print('------------- Spread')
            ## Makeing Leg Lists from APT Ticket
            Leg = processAPITicket(SymbolTicket, ExpiryTicket, StrikeTicket, RightTicket)
            for legitem in range(len(Leg)):
                print("=> %s %s x [%s] %s Call/Put: %s Strike: %s Expiration: %s" %
                      (BuySell[legitem], ComboRatio[legitem], Leg[legitem].m_secType, Leg[legitem].m_symbol,
                       Leg[legitem].m_right, Leg[legitem].m_strike, Leg[legitem].m_expiry))
            raw_input('Spread leg info correct?')

            #deep copy for replacement
            ST_A=copy.deepcopy(SymbolTicket)
            Exp_A=copy.deepcopy(ExpiryTicket)
            Str_A=copy.deepcopy(StrikeTicket)
            Right_A=copy.deepcopy(RightTicket)
            BS_A=copy.deepcopy(BuySell)
            CmbR_A=copy.deepcopy(ComboRatio)
            LmitP_A=copy.deepcopy(LimitPrice_G)
            QTY_A=copy.deepcopy(QTY_G)



            tkpkg = queTickets.popleft()
            #print(tkpkg.symbolTicket, tkpkg.expiryTicket, tkpkg.strikeTicket, tkpkg.rightTicket,
            #      tkpkg.buySell, tkpkg.comboRatio, tkpkg.limitPrice_G, tkpkg.qTY_G)

            ###
            ##  Second Ticket info
            SymbolTicket = tkpkg.symbolTicket
            ExpiryTicket = tkpkg.expiryTicket
            StrikeTicket = tkpkg.strikeTicket
            RightTicket = tkpkg.rightTicket
            BuySell = tkpkg.buySell
            ComboRatio = tkpkg.comboRatio
            LimitPrice_G = tkpkg.limitPrice_G
            QTY_G = tkpkg.qTY_G


            ## Makeing Leg Lists from APT Ticket
            Leg = processAPITicket(SymbolTicket, ExpiryTicket, StrikeTicket, RightTicket)
            ## Make sure the First Sperad is correct
            print('------------- Spread')
            for legitem in range(len(Leg)):
                print("=> %s %s x [%s] %s Call/Put: %s Strike: %s Expiration: %s" %
                      (BuySell[legitem], ComboRatio[legitem], Leg[legitem].m_secType, Leg[legitem].m_symbol,
                       Leg[legitem].m_right, Leg[legitem].m_strike, Leg[legitem].m_expiry))
            raw_input('Spread leg info correct?')

            # deep copy for replacement
            ST_B = copy.deepcopy(SymbolTicket)
            Exp_B = copy.deepcopy(ExpiryTicket)
            Str_B = copy.deepcopy(StrikeTicket)
            Right_B = copy.deepcopy(RightTicket)
            BS_B = copy.deepcopy(BuySell)
            CmbR_B = copy.deepcopy(ComboRatio)
            LmitP_B = copy.deepcopy(LimitPrice_G)
            QTY_B = copy.deepcopy(QTY_G)

            #Original combo
            print("original")
            print(ST_A, Exp_A, Str_A, Right_A, BS_A, CmbR_A, LmitP_A, QTY_A)
            print(ST_B, Exp_B, Str_B, Right_B, BS_B, CmbR_B, LmitP_B, QTY_B)

            ##Replacing
            TMP_ST=ST_A[1]
            TMP_Exp=Exp_A[1]
            TMP_Str=Str_A[1]
            TMP_Right=Right_A[1]
            TMP_BS=BS_A[1]
            TMP_CmbR=CmbR_A[1]
            #print(TMP_ST,TMP_Exp,TMP_Str,TMP_Right,TMP_BS,TMP_CmbR)

            ST_A[1]=ST_B[0]
            Exp_A[1]=Exp_B[0]
            Str_A[1]=Str_B[0]
            Right_A[1]=Right_B[0]
            BS_A[1]=BS_B[0]
            CmbR_A[1]=CmbR_B[0]
            #print(ST_A, Exp_A, Str_A, Right_A, BS_A, CmbR_A, LmitP_A, QTY_A)

            ST_B[0]=TMP_ST
            Exp_B[0]=TMP_Exp
            Str_B[0]=TMP_Str
            Right_B[0]=TMP_Right
            BS_B[0]=TMP_BS
            CmbR_B[0]=TMP_CmbR
            #print(ST_B, Exp_B, Str_B, Right_B, BS_B, CmbR_B, LmitP_B, QTY_B)

            #showing result

            print("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                  "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" %
                  (ST_A,Exp_A,Str_A,Right_A,BS_A,CmbR_A,LmitP_A,QTY_A))

            print("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                  "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" %
                  (ST_B, Exp_B, Str_B, Right_B, BS_B, CmbR_B, LmitP_B, QTY_B))

        except IndexError:
            break
    sleep(1)
    sys.exit(0)



