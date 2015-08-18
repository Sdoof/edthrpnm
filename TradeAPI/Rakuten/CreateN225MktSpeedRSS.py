# coding: utf-8

# absolute path
# f = open('C:/Users/kuby/edthrpnm/MarketData/N255MktSpeedCode.txt',mode='r')
# relative path
f = open('../../MarketData/N255MktSpeedCode.txt',mode='r')

#Excel Column
Strike = []
TYPE=[]
Date=[]
Last=[]
Chg=[]
Bid=[]
Ask=[]
IV=[]
ContactName_JP=[]
ExpDate_Code=[]
UDLY=[]

#creating Column Data
for l in f:
    l = l.rstrip()
    print l
    func_pre_chunk = '=RSS|\''+l+'.OS\'!'
    UDLY_item = '=RSS|\'N225\'!'+u'現在値'
    print func_pre_chunk
    Strike_item = func_pre_chunk + u'権利行使価格'
    TYPE_item = func_pre_chunk + u'ＣＰ区分'
    Date_item = func_pre_chunk + u'現在日付'
    Last_item = func_pre_chunk + u'現在値'
    Chg_item = func_pre_chunk + u'前日比率'
    Bid_item = func_pre_chunk + u'最良買気配値１'
    Ask_item = func_pre_chunk + u'最良売気配値１'
    IV_item = func_pre_chunk + u'ＩＶ'
    ExpDate_Code_item = func_pre_chunk + u'限月'
    ContactName_JP_item = func_pre_chunk + u'銘柄名称'
    print(Strike_item)
    print(TYPE_item)
    print(Date_item)
    print(Last_item)
    print(Chg_item)
    print(Bid_item)
    print(Ask_item)
    print(IV_item)
    print(ExpDate_Code_item)
    print(ContactName_JP_item)
    print(UDLY_item)
    Strike.append(Strike_item)
    TYPE.append(TYPE_item)
    Date.append(Date_item)
    Last.append(Last_item)
    Chg.append(Chg_item)
    Bid.append(Bid_item)
    Ask.append(Ask_item)
    IV.append(IV_item)
    ContactName_JP.append(ContactName_JP_item)
    ExpDate_Code.append(ExpDate_Code_item)
    UDLY.append(UDLY_item)

#print(Strike)
# print(TYPE)
# print(Date)
# print(Last)
# print(Chg)
# print(Bid)
# print(Ask)
# print(IV)
# print(ContactName_JP)
# print(ExpDate_Code)
# print(UDLY)
f.close()
