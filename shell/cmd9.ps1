Get-ChildItem .\ResultData. | Where-Object {$_.Name  -match "^*-3Cb.*3Cb.*-"} | cat >> .\ResultData\6Cb-.csv