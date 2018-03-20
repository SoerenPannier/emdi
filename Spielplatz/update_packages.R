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
# packrat::snapshot()

install.packages("emdi", lib = .libPaths()[1])
install.packages("readODS", lib = .libPaths()[1])
devtools::install_github("soerenpannier/emdi")
#install.packages("emdi")
library(emdi)
library(xtable)


View(installed.packages())

View(installed.packages(lib = .libPaths()[1]))
View(installed.packages(lib = .libPaths()[2]))


# Packages in Spielplatz/RepLib
used_packages <- installed.packages(lib = .libPaths()[1])
used_packages <- used_packages[, c("Package", "Version")]
rownames(used_packages) <- NULL

# Get latex table for packages in Spielplatz/RepLib
# Split table
used_packages_split <- as.data.frame(used_packages)
used_packages_split <- data.frame(used_packages_split[1:40,],
                                  used_packages_split[41:80,])

tex_packages_split <- xtable(used_packages_split)
print(tex_packages_split, include.rownames = FALSE)


# Packages in C:/R-3.4.3/library
all_packages <- installed.packages(lib = .libPaths()[2])
all_packages <- as.data.frame(all_packages)
dim(all_packages)

all_packages <- all_packages[, c("Package", "Version")]


# Compare the versions of packages in Spielplatz/RepLib and C:/R-3.4.3/library
compare_lib <- merge(all_packages, used_packages, by = "Package", all = FALSE)

all.equal(as.character(compare_lib$Version.x), as.character(compare_lib$Version.y))
compare_lib$Version.x[as.character(compare_lib$Version.x) != as.character(compare_lib$Version.y)]


save("all_packages", "used_packages", file = "./Spielplatz/Packages.RData")

