$(Get-Content ".\\MarketData\Data\ConfigParameters.csv") -replace "edthrpnm","edthrpnm2" | Out-File -Filepath .\MarketData\Data\ConfigParameters2.csv -Encoding default 
$(Get-Content ".\\MarketData\Data\ConfigParameters.csv") -replace "edthrpnm","edthrpnm3" | Out-File -Filepath .\MarketData\Data\ConfigParameters3.csv -Encoding default 