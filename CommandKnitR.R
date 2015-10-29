library(knitr)
library(markdown)
rm(list=ls())

out<-knit("./ResultData/PosAnalysisResult.Rmd",encoding="UTF-8")
markdownToHTML(out,output="./ResultData/PosAnalysisResult.html")

rm(list=ls())