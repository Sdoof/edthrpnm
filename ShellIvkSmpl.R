#Get-Content
st <- "powershell.exe -Command \" Get-ChildItem . \" "
system(st) ;rm(st)

#1Cb.csv
st <- "powershell.exe .\\shell\\cmd1.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd2.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\1Cb-.csv \" "
system(st) ;rm(st)

#2Cb.csv
st <- "powershell.exe .\\shell\\cmd3.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd4.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\2Cb-.csv \" "
system(st) ;rm(st)

#3Cb.csv
st <- "powershell.exe .\\shell\\cmd5.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd6.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\3Cb-.csv \" "
system(st) ;rm(st)

#4Cb.csv
st <- "powershell.exe .\\shell\\cmd7.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd8.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\4Cb-.csv \" "
system(st) ;rm(st)

#6Cb.csv
st <- "powershell.exe .\\shell\\cmd9.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd10.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\6Cb-.csv \" "
system(st) ;rm(st)

