 Get-ChildItem .\ResultData  | Where-Object {$_.Name  -match "^*-2Cb.*2Cb.*-"} | cat >> .\ResultData\4Cb-.csv