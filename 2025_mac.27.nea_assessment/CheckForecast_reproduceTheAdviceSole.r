# library------------------------------------------------------------

library(FLCore)
library(FLash)
library(mixfishtools)
library(tidyr)
library(ggplot2)
library(ggplotFL)
library(knitr)


#global variables------------------------------------------------------------
## general------------------------------------------------------------
yrAssess <- 2025 # final year of assessment data
yrNow <- 2025 # intermediate year
yrTAC <- 2026 # advice year
yrTACp1 <- 2027 # advice year +1 (needed to get SSB at end of yrTAC)
## stock  ------------------------------------------------------------
### general-----------
Fmsy <- 0.191

### from SSA STF to be updated------------
FyearNow <- 0.29
FyearTAC <- 0.127

RyearNow <- 4094160

load("./model/STF.RData")

Recs <- attr(FC2, "tab")[, "rec:median"]
RyearTAC <- Recs[2]


## WGBIE results to be updated-------------------------------------------------------
# Reported output from single stock headline advice
stfRef <- data.frame(
  model = "WGWIDE",
  year = 2025:2027,
  catch = c(755142, 306702, NA),
  landings = c(755142, 306702, NA),
  fbar = c(0.29, 0.127, 0.127),
  ssb = c(2735857, 2926263, 3387078) ,
  rec = c(RyearNow,RyearTAC,RyearTAC)
)


#load FLR object------------------------------------------------------------

load(file = "./output/Mac_FLStock.RData")
stk <- Mac


#extend object to be updated if different assumption about number of years/differnt years for the average and f rescaling------------------------------------------------------------
stkProj <- stf(object = window(stk,end = 2024), nyears = 3, wts.nyears = 3, 
               fbar.nyears = 1, f.rescale = TRUE, disc.nyears = 3)

#put back N2025
stock.n(stkProj)[,"2025"]  <- stock.n(stk)[,"2025"]

## control ------------------------------------------------------------
df <- as.data.frame(stkProj)
df <- subset(df, slot %in% c("landings.wt", "discards.wt", "catch.wt", "m", "mat", "harvest") & year > (yrAssess-20))
df$forecast <- df$year %in% c(yrNow, yrTAC, yrTACp1)

ggplot(df) + aes(x = year, y = data, group = age, color = forecast) +
  facet_wrap(~slot, scales = "free_y") +
  geom_line(show.legend = F) + 
  scale_color_manual(values = c(8,1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  theme_bw()


# STF to be changed if needed ------------------------------------------------------------
# stf control (Fsq, followed by 2 years at Fmsy)

ctrl <- fwdControl( 
  data.frame(
    year = c(yrNow, yrTAC, yrTACp1),
    val = c(755142, FyearTAC, FyearTAC),
    quantity = c("catch","f","f"),
    relYear = c(NA, NA, NA)                               
  )
)



# stock-recruitment model (manual input within a geometric mean model)
srPar <- FLPar(c(RyearNow, RyearTAC, RyearTAC), 
               dimnames = list(params="a", year = c(yrNow, yrTAC, yrTACp1), iter = 1))
srMod <- FLSR(model = "geomean", params = srPar)

# projection
source ("fwdF3 function.r") # special fwd that does not overwrite N2025
stkProj <- fwd( stkProj,  ctrl,  srMod)

# plot
L <- FLStocks(list(assessment = stkProj, forecast = stkProj[,ac(yrAssess:yrTACp1)]))
plot(L) + 
  # aes(linetype = stock) +
  scale_color_manual(values = c(8,1)) + 
  # scale_linetype_manual(values = c(2,1)) +
  theme_bw()

# Control ------------------------------

stfDet <- data.frame(
  model = "FLR",
  year = ac(yrAssess:yrTACp1),
  catch = c(catch(stkProj[,ac(yrAssess:yrTACp1)])),
  landings = c(landings(stkProj[,ac(yrAssess:yrTACp1)])),
  fbar = c(fbar(stkProj[,ac(yrAssess:yrTACp1)])),
  ssb = c(ssb(stkProj[,ac(yrAssess:yrTACp1)])),
  rec = c(rec(stkProj[,ac(yrAssess:yrTACp1)]))
  )
  

df <- merge(stfRef, stfDet, all = T)
df <- pivot_longer(df, cols = c(catch, landings, fbar, ssb, rec), 
                   names_to = "variable", values_to = "value")
df <- df |>
  filter(
    (variable %in% c("catch", "rec", "fbar") & year <= yrTAC) |
      (variable %in% c("ssb") & year <= yrTACp1))


df2 <- pivot_wider(df, names_from = model, values_from = value)
df2$percErr <- round((df2$FLR - df2$WGWIDE)/df2$WGWIDE * 100, 1)

ggplot(df) + aes(x = year, y = value, group = model, color = model, shape = model) +
  facet_wrap(~variable, scales = "free_y") +
  geom_line() +
  geom_point(size = 3, stroke = 1) +
  scale_shape_discrete(solid = F) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw()


kable(df2, digits = 3)


ggplot(df2) + aes(x = year, y = percErr) + 
  facet_wrap(~variable) +
  geom_col(fill = 4) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_hline(yintercept = c(-10,10), linetype = 3) + 
  theme_bw()
