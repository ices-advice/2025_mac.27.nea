

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

#-------------------------------------------------------------------------------
# load model runs
#-------------------------------------------------------------------------------

model <- "0.01"


load(paste0("./model/fit_and_diagnostics/model_",model,"_full_output.RData"))


set.seed(123)

# Config years
ay<- max(fit$data$years)   # current year, also final data year as we have in-year data
ady <- ay + 1
Ave.yrs <- ay - (1:3)
Rec.yrs <- 2014:ay


FC<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
# catchval = c(550000,NA,NA),                                                   #  a vector of target catches. See details.
  catchval.exact = c(750000,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,0.19,NA),                                                          #  a vector of target f values. See details.
#  nextssb = NULL,                                                               #  a vector target SSB values the following year. See details
#  landval = NULL,                                                               #  a vector of target catches. See details.
#  cwF = NULL,                                                                   #  a vector target custom weighted F values. customWeights must also be specified
  nosim = 1000,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2026 = Fmsy",                                                                 #  optional label to appear in short table
  overwriteSelYears = NULL,                                                     #  if a vector of years is specified, then the average selectivity of those years is used (not recommended)
  deterministic = FALSE,                                                        #  option to turn all process noise off (not recommended, as it will likely cause bias)
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  customWeights = NULL,                                                         #  a vector of same length as number of age groups giving custom weights (currently only used for weighted average of F calculation)
  customSel = NULL,                                                             #  supply a custom selection vector that will then be used as fixed selection in all years after the final assessment year (not recommended)
  lagR = FALSE,                                                                 #  if the second youngest age should be reported as recruits
  splitLD = FALSE,                                                              #  if TRUE the result is split in landing and discards
  addTSB = FALSE,                                                               #  if TRUE the total stock biomass (TSB) is added
  useSWmodel = (fit$conf$stockWeightModel == 1),                                #  if TRUE the catch mean weight predicted from the assessment model is used (can only be used for configurations supporting this)
  useCWmodel = (fit$conf$catchWeightModel == 1),                                #  if TRUE the catch mean weight predicted from the assessment model is used (can only be used for configurations supporting this)
  useMOmodel = (fit$conf$matureModel == 1),                                     #  if TRUE the proportion mature predicted from the assessment model is used (can only be used for configurations supporting this)
  useNMmodel = (fit$conf$mortalityModel == 1),                                  #  if TRUE the natural mortality predicted from the assessment model is used (can only be used for configurations supporting this)
  savesim = FALSE,                                                              #  save the individual simulations
  estimate = median                                                             #  the summary function used (typically mean or median)
)

SSBs <- attr(FC, "tab")[, "ssb:median"]

MSYBtrigger <- 4119337
Blim <-  3067017

Fred <- 0.19* SSBs[["2025"]] / MSYBtrigger

FC2<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
# catchval = c(550000,NA,NA),                                                   #  a vector of target catches. See details.
  catchval.exact = c(750000,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fred,NA),                                                          #  a vector of target f values. See details.
  nosim = 10000,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2026 = Fred",                                                               #  optional label to appear in short table
   savesim = T
)


FC2





####### run a sequence of Ftargat values to find the F p(SSB<Blim)=0.5


Frange  <- seq(0.08,0.19,by=0.005)
pBlim <-c()
FCs<-list()

for (i in 1:length(Frange))
{

ftarg <- Frange[i]

fc<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
# catchval = c(550000,NA,NA),                                                   #  a vector of target catches. See details.
  catchval.exact = c(750000,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,ftarg,NA),                                                          #  a vector of target f values. See details.
  nosim = 10000,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = paste("F2026 =", ftarg)  ,
     savesim = T                                                               #  optional label to appear in short table
)

FCs[[i]]<-fc

pBlim[i]<-sum(fc[[3]]$ssb < Blim)/length(fc[[3]]$ssb)
}


plot(Frange,pBlim)


