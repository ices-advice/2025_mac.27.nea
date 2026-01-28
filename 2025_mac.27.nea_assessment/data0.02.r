## Preprocess data, write TAF data tables

## Before:
## After:
 library(icesTAF)

source("data0.01.R")

# remove the 2025 SSB estimate from MEGS
surveys[["Egg-survey"]]["2025",]    <-  NA


dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn, 
                    prop.mature=mo, 
                    stock.mean.weight=sw, 
                    catch.mean.weight=cw, 
                    dis.mean.weight=dw, 
                    land.mean.weight=lw,
                    prop.f=pf, 
                    prop.m=pm, 
                    natural.mortality=nm, 
                    land.frac=lf,
                    recapture=recap)

save(dat,file=paste0("./data/",data.path,"_no2025egg.RData"))
