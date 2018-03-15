################################################################################
# All packages should be up to date for the generation of the numbers in the 
# paper. Thus, we update the packages that are imported or suggested by emdi 
# and we use the last R Version at the 14.03.2018. 
################################################################################

# See if old packages are installed
old.packages()

# Update all packages
update.packages()


# Add a new libPath that only contains dependencies of emdi and their 
# dependencies
.libPaths("./Spielplatz/RepLib")
.libPaths()
install.packages("emdi", lib = .libPaths()[1])
install.packages("readODS", lib = .libPaths()[1])
devtools::install_github("soerenpannier/emdi")
#install.packages("emdi")
library(emdi)

# The ggplot2 package is loaded additionally since we use theme_set for a better
# representation of the plots in the paper but it is otherwise not needed to 
# load the package manually. 
library(ggplot2)
# The laeken package is loaded since we estimate the poverty line in the 
# application with the weightedMedian function from this package.
library(laeken)

installed.packages()

View(installed.packages(lib = .libPaths()[1]))
View(installed.packages(lib = .libPaths()[2]))

used_packages <- installed.packages(lib = .libPaths()[1])

used_packages_adj <- used_packages[, c("Package", "Version")]
rownames(used_packages_adj) <- NULL


library("xtable", lib = "C:/R-3.4.3/libBackup")
tex_packages <- xtable(used_packages_adj)
print(tex_packages, include.rownames = FALSE)


# Split table
used_packages_adj_split <- as.data.frame(used_packages_adj)


used_packages_adj_split <- data.frame(used_packages_adj_split[1:40,],
                                      used_packages_adj_split[41:80,])



tex_packages_split <- xtable(used_packages_adj_split)
print(tex_packages_split, include.rownames = FALSE)


# Abgleich packages
used_packages2 <- installed.packages(lib = .libPaths())
used_packages2_adj_split <- as.data.frame(used_packages2)
dim(used_packages2_adj_split)

used_packages2_adj_split <- used_packages2_adj_split[, c("Package", "Version")]


compare_lib <- merge(used_packages2_adj_split, as.data.frame(used_packages_adj), by = "Package" ,
      all = FALSE)

all.equal(as.character(compare_lib$Version.x), as.character(compare_lib$Version.y))

compare_lib$Version.x[as.character(compare_lib$Version.x) != as.character(compare_lib$Version.y)]


