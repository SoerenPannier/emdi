

setwd("H:/")
getwd()

Sys.getenv("PATH") 
Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:/Program Files/MiKTeX 2.9/miktex/bin/x64",sep=";"))
system("R CMD Rd2pdf emdi")
