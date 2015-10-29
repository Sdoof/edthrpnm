library(knitr)
library(markdown)
rm(list=ls())

out<-knit("./ResultData/PosAnalysisResult.Rmd",output ="./ResultData/PosAnalysisResult.md",encoding="UTF-8")

knit2html(input   = out, output  = "./ResultData/PosAnalysisResult.html")
#markdownToHTML(out,output="./ResultData/PosAnalysisResult.html",fragment.only=F)

rm(list=ls())