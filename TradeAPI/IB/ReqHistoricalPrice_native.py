# coding: utf-8
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.common import *
from ibapi.contract import *
from time import sleep
from datetime import datetime, timedelta
from threading import Thread
import csv

port_G = 7496
clientId_G = 780

def makeContractEURGBP():
    contract = Contract()
    contract.symbol = "EUR"
    contract.secType = "CASH"
    contract.currency = "GBP"
    contract.exchange = "IDEALPRO"
    return contract

def makeContractAUDJPY():
    contract = Contract()
    contract.symbol = "AUD"
    contract.secType = "CASH"
    contract.currency = "JPY"
    contract.exchange = "IDEALPRO"
    return contract

def makeContractSGDJPY():
    contract = Contract()
    contract.symbol = "SGD"
    contract.secType = "CASH"
    contract.currency = "JPY"
    contract.exchange = "IDEALPRO"
    return contract

def makeContractHKDJPY():
    contract = Contract()
    contract.symbol = "HKD"
    contract.secType = "CASH"
    contract.currency = "JPY"
    contract.exchange = "IDEALPRO"
    return contract

def makeContractAAPL():
    contract = Contract()
    contract.symbol = "AAPL"
    contract.secType = "STK"
    contract.exchange = "SMART"
    contract.currency = "USD"
    contract.primaryExchange = "NASDAQ"
    return contract

class TestApp(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.nextOrderId = -1
        self.insttrument = None
        self.fname = None
        self.writer_csv = None

    def error(self, reqId:TickerId, errorCode:int, errorString:str):
        print("Error,", reqId, ",", errorCode, ",",errorString)

    def contractDetails(self,reqId:int, contractDetails:ContractDetails):
        print("contractDetails,", reqId, ",", contractDetails)

    def historicalData(self, reqId: int, bar: BarData):
        print(self.insttrument,",HistoricalData,", reqId, "Date,", bar.date, "Open,", bar.open,
        "High,", bar.high, "Low,", bar.low, "Close,", bar.close, "Volume,", bar.volume,
        "Count,", bar.barCount, "WAP,", bar.average)
        self.writer_csv.writerow(
                    [datetime.strptime(bar.date, '%Y%m%d').strftime('%Y/%m/%d'),
                     str(format(bar.open, '.3f')),
                     str(format(bar.high, '.3f')),
                     str(format(bar.low, '.3f')),
                     str(format(bar.close, '.3f'))
                     ])

    def historicalDataEnd(self, reqId: int, start: str, end: str):
        super().historicalDataEnd(reqId, start, end)
        print("HistoricalDataEnd ", reqId, "from", start, "to", end)
        self.insttrument = None
        self.fname = None
        self.writer_csv = None
        self.disconnect()


    def historicalDataUpdate(self, reqId: int, bar: BarData):
        print("HistoricalDataUpdate,", reqId, "Date,", bar.date, "Open.", bar.open,
        "High,", bar.high, "Low,", bar.low, "Close,", bar.close, "Volume,", bar.volume,
        "Count,", bar.barCount, "WAP,", bar.average)

    def headTimestamp(self, reqId: int, headTimestamp: str):
        print("HeadTimestamp,", reqId, ",", headTimestamp)

    def historicalDataRequests_req(self, inst:str):
        # Requesting historical data
        queryTime = (datetime.today() -
                     timedelta(days=0)).strftime("%Y%m%d %H:%M:%S")
        self.nextOrderId = self.nextOrderId+1

        #if inst doesn't match, hist of EURGBP is given
        contract=makeContractEURGBP()

        self.insttrument = inst
        if inst == 'AUDJPY':
            contract=makeContractAUDJPY()
        elif inst == 'SGDJPY':
            contract = makeContractSGDJPY()
        elif inst == 'HKDJPY':
            contract = makeContractHKDJPY()

        self.fname = "C:/Users/kuby/edthrpnm/MarketData/" + self.insttrument + contract.secType + \
                     datetime.now().strftime("%Y-%m-%d") + ".csv"
        fd_write = open(self.fname, mode='a')
        self.writer_csv = csv.writer(fd_write, lineterminator="\n", quoting=csv.QUOTE_NONNUMERIC)


        self.reqHistoricalData(self.nextOrderId, contract, queryTime,
                              "1 Y", "1 day", "MIDPOINT", 1, 1, False, [])


    def headTimeStampRequests_req(self):
        # Requesting headTimeStamp
        self.nextOrderId = self.nextOrderId + 1
        self.reqHeadTimeStamp(self.nextOrderId, makeContractEURGBP(), "TRADES", 0, 1)

    def historicalDataRequests_cancel(self):
        # Canceling historical data requests
        self.cancelHistoricalData(self.nextOrderId)


def main():
    global port_G, clientId_G
    app=TestApp()
    app.connect("127.0.0.1",port_G,clientId_G)
    #request contract details test
    #app.reqContractDetails(10,makeContractAAPL())

    # request historical data
    #AUDJPY
    app.historicalDataRequests_req('AUDJPY')
    app.run()
    sleep(1)

    # SGDJPY
    clientId_G = clientId_G+1
    app.connect("127.0.0.1", port_G, clientId_G)
    app.historicalDataRequests_req('SGDJPY')
    app.run()
    sleep(1)

    # HKDJPY
    clientId_G = clientId_G + 1
    app.connect("127.0.0.1", port_G, clientId_G)
    app.historicalDataRequests_req('HKDJPY')
    app.run()

    sleep(2)
    print('finish getting historical price')
    sys.exit(0)


if __name__ == "__main__":
    main()
