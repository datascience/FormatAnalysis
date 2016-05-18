
requiredPackages <- c("plyr", "ggplot2", "minpack.lm", "propagate", "MASS", "gridExtra", "grid")
toInstall <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(toInstall)>0) {
  cat(paste("Installing ", toInstall))
  install.packages(toInstall)
} else {
  cat("Needed packages are installed")
}
