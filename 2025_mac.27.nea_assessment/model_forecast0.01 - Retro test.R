

#-------------------------------------------------------------------------------
# initialise
#-------------------------------------------------------------------------------

library(icesTAF)
taf.library(stockassessment)
library(ggplot2)
#taf.bootstrap()

mkdir("output")
mkdir("output/model diags")
mkdir("output/model diags/explorations/")




# load model run
model <- "0.01"
load(paste0("./model/fit_and_diagnostics/model_",model,"_full_output.RData"))





# Config years and reference values
ay<- max(ret[[1]]$data$years)   # last year in the firstr retro peel
ady <- ay + 1
Ave.yrs <- ay - (1:3)      # no biological input data for the curreny year
Rec.yrs <- 2014:ay         # take all recruitments since 2024, incl. current year
# simulation numbers
nsim <- 10000
#ref points
MSYBtrigger <- 4119337
Blim        <- 3067017
Fmsy        <- 0.191
# catch estimate for 2024
IYC <- 954112
# current year advice    (for 2024)
Cur.Adv  <- 739386

save(ay,ady,Ave.yrs,Rec.yrs,MSYBtrigger,Blim,Fmsy,IYC,Cur.Adv,file=("./model/STFconf.RData"))


#### FC : F2025 = Fmsy
set.seed(123)
FC1ret<- forecast(
  ret[[1]],                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fmsy,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2025 = Fmsy",                                                                 #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = TRUE
  )

SSBs <- attr(FC1ret, "tab")[, "ssb:median"]




save(FC1ret , file ="./model/STF_ret.RData")

