# check if notebooks load superfluous packages
knitr::purl("09-multreg.Rmd", documentation = 0)
install.packages('devtools')
library(devtools)
remotes::install_github("MichaelChirico/funchir")
funchir::stale_package_check('09-multreg.R')
