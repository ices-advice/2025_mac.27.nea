####
#### a remplement to the FLR function fdw() so that, if numbers at age are specified at the start of the first
#    projection year, those do not get overwritten
#    this is important for assessments that produce estimates in the current year which are used in the single stock advice STF
#    such as North Sea cod and whiting
#    Thomas Brunel based on code from Iago Mosqueira, WGMIXFISH METHODS 2020


fwd <- function(x, ctrl, sr) {

  ages <- dimnames(m(x))$age
  year <- an(ctrl@target$year[1])     # first projection year

  # are there values for the stock abundances in the ImY?
  surv<-apply(!is.na(stock.n(x)[,ac(year)] ),2:6,any)

  # COPY stock
  y <- x
  if(surv)
  {
  # SET stock.n[1:last-1, year-1] to stock.n[2:last, year]
  stock.n(y)[ages[-length(ages)], ac(year-1)] <-
  stock.n(y)[ages[-1], ac(year)]
  # SET stock.n[last, year-1] to zero, all in age last-1
  stock.n(y)[ages[length(ages)], ac(year-1)] <- 0
  # SET m and F to almost nothing
  m(y)[, ac(year-1)] <- 1e-32
  harvest(y)[, ac(year-1)] <- 1e-32
  
  # if Stq F : need to reshape the ctrl because Fsq has now become 1e-32
  # so copy the Fsq value from the original FLStock, x, and make a control that is not using rel.year
  if (!is.na(ctrl@target[1,'rel.year'])) 
  {
  ctrl@target[1,'rel.year'] <- NA
  ctrl@target[1,'val'] <- fbar(x)[,ac(year-1)]
  ctrl@trgtArray[1,"val",] <- fbar(x)[,ac(year-1)]
  }
  }
  
  
  # FWD
  z <-  FLash::fwd(y,ctrl=ctrl,sr=sr)
  # REASSEMBLE stock
  x[, ac(year:range(x)['maxyear'])] <- z[, ac(year:range(x)['maxyear'])]
  return(x)
}

