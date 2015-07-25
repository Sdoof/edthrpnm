$(Get-Content ".\\MarketData\Data\ConfigParameters.csv") -replace "edthrpnm","edthrpnm2" > .\MarketData\Data\ConfigParameters2.csv
$(Get-Content ".\\MarketData\Data\ConfigParameters.csv") -replace "edthrpnm","edthrpnm3" > .\MarketData\Data\ConfigParameters3.csv
