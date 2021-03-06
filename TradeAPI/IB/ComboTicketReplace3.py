# coding: utf-8
from ib.ext.Contract import Contract
from time import sleep
from collections import deque
import copy
import sys

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

def processAPITicket(Symbol, Expiry, Strike, Right):
    Leg_new = None
    for legid in range(len(Symbol)):
        if Leg_new:
            Leg_new.append(makeOptContract(Symbol[legid], Expiry[legid], Strike[legid], Right[legid]))
        else:
            Leg_new = [makeOptContract(Symbol[legid], Expiry[legid], Strike[legid], Right[legid])]
    return Leg_new

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
# API Ticket generated by the edthrpnm
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
            ##
            # First Ticket
            tkpkg=queTickets.popleft()
            #print(tkpkg.symbolTicket,tkpkg.expiryTicket,tkpkg.strikeTicket,tkpkg.rightTicket,
            #      tkpkg.buySell,tkpkg.comboRatio,tkpkg.limitPrice_G,tkpkg.qTY_G)

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
                print(("=> %s %s x [%s] %s Call/Put: %s Strike: %s Expiration: %s" %
                      (BuySell[legitem], ComboRatio[legitem], Leg[legitem].m_secType, Leg[legitem].m_symbol,
                       Leg[legitem].m_right, Leg[legitem].m_strike, Leg[legitem].m_expiry)))
            input('Spread leg info correct?')

            # deep copy for replacement
            ST_A=copy.deepcopy(SymbolTicket)
            Exp_A=copy.deepcopy(ExpiryTicket)
            Str_A=copy.deepcopy(StrikeTicket)
            Right_A=copy.deepcopy(RightTicket)
            BS_A=copy.deepcopy(BuySell)
            CmbR_A=copy.deepcopy(ComboRatio)
            LmitP_A=copy.deepcopy(LimitPrice_G)
            QTY_A=copy.deepcopy(QTY_G)

            #print(ST_A, Exp_A, Str_A, Right_A, BS_A, CmbR_A, LmitP_A, QTY_A)
            if max(CmbR_A)  >= 2:
                #print(max(CmbR_A),CmbR_A.index(max(CmbR_A)),CmbR_A[0],CmbR_A[1],CmbR_A[2])

                ST_A_BRKUP=[]
                Exp_A_BRKUP=[]
                Str_A_BRKUP=[]
                Right_A_BRKUP=[]
                BS_A_BRKUP=[]
                CmbR_A_BRKUP=[]
                for i in range(len(CmbR_A)):
                    if i != CmbR_A.index(max(CmbR_A)):
                        ST_A_BRKUP.append(ST_A[i])
                        Exp_A_BRKUP.append(Exp_A[i])
                        Str_A_BRKUP.append(Str_A[i])
                        Right_A_BRKUP.append(Right_A[i])
                        BS_A_BRKUP.append(BS_A[i])
                        CmbR_A_BRKUP.append(CmbR_A[i])

                #print(ST_A_BRKUP,Exp_A_BRKUP,Str_A_BRKUP,Right_A_BRKUP,BS_A_BRKUP,CmbR_A_BRKUP)

                ST_A_BRK_RST=[]
                Exp_A_BRK_RST = []
                Str_A_BRK_RST = []
                Right_A_BRK_RST = []
                BS_A_BRK_RST = []
                CmbR_A_BRK_RST = []

                for i in range(len(ST_A_BRKUP)):
                    ST_A_BRK_RST.append(  [ST_A[ CmbR_A.index(max(CmbR_A)) ],ST_A_BRKUP[i]]  )
                    Exp_A_BRK_RST.append(  [Exp_A[ CmbR_A.index(max(CmbR_A)) ],Exp_A_BRKUP[i]] )
                    Str_A_BRK_RST.append(  [Str_A[ CmbR_A.index(max(CmbR_A)) ],Str_A_BRKUP[i]] )
                    Right_A_BRK_RST.append(  [Right_A[CmbR_A.index(max(CmbR_A))], Right_A_BRKUP[i]]  )
                    BS_A_BRK_RST.append( [BS_A[CmbR_A.index(max(CmbR_A))], BS_A_BRKUP[i]] )
                    #CmbR_A_BRK_RST.append( [CmbR_A[CmbR_A.index(max(CmbR_A))],CmbR_A_BRKUP[i]] )
                    CmbR_A_BRK_RST.append( [  1  ,CmbR_A_BRKUP[i]] )

                #print(ST_A_BRK_RST,Exp_A_BRK_RST,Str_A_BRK_RST,Right_A_BRK_RST,BS_A_BRK_RST,CmbR_A_BRK_RST)

                print("\n### Ticket Split")
                #print(ST_A_BRK_RST[0], Exp_A_BRK_RST[0], Str_A_BRK_RST[0], Right_A_BRK_RST[0], BS_A_BRK_RST[0], CmbR_A_BRK_RST[0],LmitP_A,QTY_A)
                #print(ST_A_BRK_RST[1], Exp_A_BRK_RST[1], Str_A_BRK_RST[1], Right_A_BRK_RST[1], BS_A_BRK_RST[1],CmbR_A_BRK_RST[1], LmitP_A, QTY_A)
                for i in range(len(ST_A_BRKUP)):
                    print(("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                          "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" % (
                          ST_A_BRK_RST[i], Exp_A_BRK_RST[i], Str_A_BRK_RST[i], Right_A_BRK_RST[i], BS_A_BRK_RST[i],
                          CmbR_A_BRK_RST[i], LmitP_A, QTY_A)))

                print("\n### Ticket Split Reversed")
                for i in range(len(ST_A_BRKUP)):
                    print(("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                          "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" % (
                              list(reversed(ST_A_BRK_RST[i])), list(reversed(Exp_A_BRK_RST[i])), list(reversed(Str_A_BRK_RST[i])), list(reversed(Right_A_BRK_RST[i])), list(reversed(BS_A_BRK_RST[i])),
                              list(reversed(CmbR_A_BRK_RST[i])), LmitP_A, QTY_A)))
            else:
                print("\n### Ticket Reversed")
                print(("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                  "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" %
                      (list(reversed(ST_A)), list(reversed(Exp_A)), list(reversed(Str_A)), list(reversed(Right_A)),
                       list(reversed(BS_A)), list(reversed(CmbR_A)), LmitP_A, QTY_A)))

            ##
            # Second Ticket
            tkpkg = queTickets.popleft()
            #print(tkpkg.symbolTicket, tkpkg.expiryTicket, tkpkg.strikeTicket, tkpkg.rightTicket,
            #      tkpkg.buySell, tkpkg.comboRatio, tkpkg.limitPrice_G, tkpkg.qTY_G)

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
                print(("=> %s %s x [%s] %s Call/Put: %s Strike: %s Expiration: %s" %
                      (BuySell[legitem], ComboRatio[legitem], Leg[legitem].m_secType, Leg[legitem].m_symbol,
                       Leg[legitem].m_right, Leg[legitem].m_strike, Leg[legitem].m_expiry)))
            input('Spread leg info correct?')



            # deep copy for replacement
            ST_B = copy.deepcopy(SymbolTicket)
            Exp_B = copy.deepcopy(ExpiryTicket)
            Str_B = copy.deepcopy(StrikeTicket)
            Right_B = copy.deepcopy(RightTicket)
            BS_B = copy.deepcopy(BuySell)
            CmbR_B = copy.deepcopy(ComboRatio)
            LmitP_B = copy.deepcopy(LimitPrice_G)
            QTY_B = copy.deepcopy(QTY_G)

            #Ticket Reversed
            print("\n###  Ticket Reversed")
            print(("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                  "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" %
                  (list(reversed(ST_B)), list(reversed(Exp_B)), list(reversed(Str_B)), list(reversed(Right_B)),
                   list(reversed(BS_B)), list(reversed(CmbR_B)), LmitP_B, QTY_B)))

            #Original combo
            #print("\nOriginal Ticket Elements")
            #print(ST_A, Exp_A, Str_A, Right_A, BS_A, CmbR_A, LmitP_A, QTY_A)
            #print(ST_B, Exp_B, Str_B, Right_B, BS_B, CmbR_B, LmitP_B, QTY_B)

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

            #Replaced Tickets
            print("\n###  Ticket Replaced")
            print(("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                  "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" %
                  (ST_A,Exp_A,Str_A,Right_A,BS_A,CmbR_A,LmitP_A,QTY_A)))
            print(("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                  "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" %
                  (ST_B, Exp_B, Str_B, Right_B, BS_B, CmbR_B, LmitP_B, QTY_B)))

            #Replaced Tickets Reversed
            print("\n###  Replaced Ticket Reversed")
            print(("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                  "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" %
                  (list(reversed(ST_A)),list(reversed(Exp_A)),list(reversed(Str_A)),list(reversed(Right_A)),
                   list(reversed(BS_A)),list(reversed(CmbR_A)), LmitP_A, QTY_A)))
            print(("SymbolTicket = %s; ExpiryTicket = %s; StrikeTicket = %s; RightTicket = %s; "
                  "BuySell = %s; ComboRatio = %s; LimitPrice_G = %s; QTY_G =%s" %
                  (list(reversed(ST_B)), list(reversed(Exp_B)), list(reversed(Str_B)), list(reversed(Right_B)),
                   list(reversed(BS_B)), list(reversed(CmbR_B)), LmitP_B, QTY_B)))

        except IndexError:
            break
    sleep(1)
    sys.exit(0)



