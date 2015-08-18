# coding: utf-8

# absolute path
# f = open('C:/Users/hoge/edthrpnm/MarketData/N255MktSpeedCode.txt',mode='r')
# relative path
f = open('../../MarketData/N255MktSpeedCode.txt',mode='r')

#Excel Column
Strike = []
TYPE=[]
Date=[]
ContactName=[]
Last=[]
Chg=[]
Bid=[]
Ask=[]
IV=[]
ContactName_JP=[]
ExpDate_Code=[]
UDLY=[]

# creating Column Data
for l in f:
    l = l.rstrip()
    print l
    func_pre_chunk = '=RSS|\''+l+'.OS\'!'
    UDLY_item = '=RSS|\'N225\'!'+u'現在値'
    print func_pre_chunk
    Strike_item = func_pre_chunk + u'権利行使価格'
    TYPE_item = func_pre_chunk + u'ＣＰ区分'
    Date_item = func_pre_chunk + u'現在日付'
    ContactName_item = func_pre_chunk + u'銘柄コード'
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
    print(ContactName_item)
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
    ContactName.append(ContactName_item)
    Last.append(Last_item)
    Chg.append(Chg_item)
    Bid.append(Bid_item)
    Ask.append(Ask_item)
    IV.append(IV_item)
    ContactName_JP.append(ContactName_JP_item)
    ExpDate_Code.append(ExpDate_Code_item)
    UDLY.append(UDLY_item)

#print(Strike[0])
# print(TYPE)
# print(Date)
# print(ContactName)
# print(Last)
# print(Chg)
# print(Bid)
# print(Ask)
# print(IV)
# print(ContactName_JP)
# print(ExpDate_Code)
# print(UDLY)
f.close()

# writing to file

f = open('../../MarketData/oppriceN255.csv',mode='w')
f.write('Date'.encode('utf8','ignore')+','+'TYPE'.encode('utf8','ignore')+','+'Strike'.encode('utf8','ignore')+',')
f.write('ContactName'.encode('utf8','ignore')+','+'UDLY'.encode('utf8','ignore')+'Last'.encode('utf8','ignore')+',')
f.write('Chg'.encode('utf8','ignore')+','+'Bid'.encode('utf8','ignore')+'Ask'.encode('utf8','ignore')+',')
f.write('IV'.encode('utf8','ignore')+','+'ContactName_JP'.encode('utf8','ignore')+'ExpDate_Code'.encode('utf8','ignore')+'\n')
for l in range(len(Strike)):
    f.write(Date[l].encode('utf8','ignore')+','+TYPE[l].encode('utf8','ignore')+','+Strike[l].encode('utf8','ignore')+',')
    f.write(ContactName[l].encode('utf8','ignore')+','+UDLY[l].encode('utf8','ignore')+Last[l].encode('utf8','ignore')+',')
    f.write(Chg[l].encode('utf8','ignore')+','+Bid[l].encode('utf8','ignore')+Ask[l].encode('utf8','ignore')+',')
    f.write(IV[l].encode('utf8','ignore')+','+ContactName_JP[l].encode('utf8','ignore')+ExpDate_Code[l].encode('utf8','ignore')+'\n')
f.close()
