rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(EBASS)

# install.packages(
#   "https://cran.r-project.org/src/contrib/Archive/EBASS/EBASS_0.1.tar.gz", 
#   repos = NULL, type = "source"
# )

# https://rdrr.io/cran/EBASS/f/vignettes/my-vignette.Rmd

object_lambda <- create_object_lambda (50000) # maximum ICER US$50,0000

de <- 0.028 # difference of effectiveness in QALY

dc <- -167.4 # difference of cost in USD

# incremental net monetary benefit (INMB)
object_inmb <- create_object_inmb(de = de, dc=dc, object_lambda = object_lambda)
object_inmb$get_inmb()

## direct INMB input wtihout de, dc, or lambda
# object_inmb_direct <- create_object_inmb_direct(inmb = 968)

object_var_inmb <- create_object_var_inmb_direct(object_inmb$get_inmb())

object_pop <- create_object_pop(horizon = 20, discount=0.03, N_year = 10000)

object_evpi_decrease <- create_object_evpi_decrease(object_inmb, object_pop, object_var_inmb)
