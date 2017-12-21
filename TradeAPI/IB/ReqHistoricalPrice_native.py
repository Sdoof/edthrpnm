from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.common import *
from ibapi.contract import *
from time import sleep
from datetime import datetime, timedelta

port_G = 7496
clientId_G = 780

def makeContractEURGBP():
    contract = Contract()
    contract.symbol = "EUR"
    contract.secType = "CASH"
    contract.currency = "GBP"
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

    def error(self, reqId:TickerId, errorCode:int, errorString:str):
        print("Error:", reqId, " ", errorCode, " ",errorString)

    def contractDetails(self,reqId:int, contractDetails:ContractDetails):
        print("contractDetails:", reqId, " ", contractDetails)

    def historicalData(self, reqId: int, bar: BarData):
        print("HistoricalData. ", reqId, " Date:", bar.date, "Open:", bar.open,
        "High:", bar.high, "Low:", bar.low, "Close:", bar.close, "Volume:", bar.volume,
        "Count:", bar.barCount, "WAP:", bar.average)

    def historicalDataEnd(self, reqId: int, start: str, end: str):
        super().historicalDataEnd(reqId, start, end)
        print("HistoricalDataEnd ", reqId, "from", start, "to", end)

    def historicalDataUpdate(self, reqId: int, bar: BarData):
        print("HistoricalDataUpdate. ", reqId, " Date:", bar.date, "Open:", bar.open,
        "High:", bar.high, "Low:", bar.low, "Close:", bar.close, "Volume:", bar.volume,
        "Count:", bar.barCount, "WAP:", bar.average)

    def headTimestamp(self, reqId: int, headTimestamp: str):
        print("HeadTimestamp: ", reqId, " ", headTimestamp)

    def historicalDataRequests_req(self):
        # Requesting historical data
        queryTime = (datetime.today() -
                     timedelta(days=0)).strftime("%Y%m%d %H:%M:%S")

        self.reqHistoricalData(4001, makeContractEURGBP(), queryTime,
                               "1 Y", "1 day", "MIDPOINT", 1, 1, False, [])

    def headTimeStampRequests_req(self):
        # Requesting headTimeStamp
        self.reqHeadTimeStamp(4103, makeContractEURGBP(), "TRADES", 0, 1)

    def historicalDataRequests_cancel(self):
        # Canceling historical data requests
        self.cancelHistoricalData(4001)

    def historicalDataRequests_cancel(self):
        # Canceling historical data requests
        self.cancelHeadTimeStamp(4103)

def main():
    app=TestApp()
    app.connect("127.0.0.1",port_G,clientId_G)
    app.reqContractDetails(10,makeContractAAPL())
    app.historicalDataRequests_req()
    app.run()

    input('Get historical prices until something hit ')
    historicalDataRequests_cancel()

if __name__ == "__main__":
    main()
