Get-ChildItem .\ResultData  | Where-Object {$_.Name  -match "^*-\dCb\+\dCb\+\dCb-"}�@| cat >> .\ResultData\3Cb-.csv