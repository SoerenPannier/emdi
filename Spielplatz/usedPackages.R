View(installed.packages())

instPkg <- as.data.frame(installed.packages(lib.loc = .libPaths()[1]))
instPkg <- cbind.data.frame(instPkg$Package, instPkg$Version)
nrow(instPkg) / 3
rownames(instPkg) <- NULL
linesPerTab <- ceiling(nrow(instPkg) / 3)
pkgTab <- cbind(instPkg[1:linesPerTab,], 
            instPkg[ (linesPerTab + 1):(linesPerTab * 2),],
            rbind(instPkg[ (linesPerTab * 2 + 1):nrow(instPkg),], c(NA, NA)))# rbind(c(NA, NA),c(NA, NA)

colnames(pkgTab) <- rep(c("Package", "Version"), times = 3)
save(pkgTab, file = "Spielplatz/UsedPkgs.RData")

load(file = "Spielplatz/UsedPkgs.RData")

packrat::extlib("xtable")
library(xtable)
print(xtable(pkgTab), booktabs = TRUE, include.rownames = FALSE)

packrat::snapshot()
