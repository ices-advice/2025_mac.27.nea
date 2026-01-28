## Run analysis, write model results

#-------------------------------------------------------------------------------
# initialise
#-------------------------------------------------------------------------------
library(icesTAF)
taf.library(stockassessment)
source("./boot/software/utils/utility.r")
mkdir("model")
mkdir("model/fit_and_diagnostics")

# new model name
modName <- "0.00"


#-------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------


source("data0.00.R")

#-------------------------------------------------------------------------------
# Model configuration
#-------------------------------------------------------------------------------

conf<-defcon(dat)


####### Fishing mortality process
#states fully decoupled
conf$keyLogFsta[1,]  <- c(0:9,9)

# F random walk variance different for young and for old ages
conf$keyVarF[1,]    <- c( 0,0,1,1,1,1,1,1,1,2,2)

# correlation in the random walks
conf$corFlag <- 2

# Fbar age range
conf$fbarRange <- c(4,8)


####### Observation variances
conf$keyVarObs[1,]  <-c(0,0,1,1,1,1,1,1,1,2,2)             # catches
conf$keyVarObs[2,1] <-  3                                  # egg survey
conf$keyVarObs[3,]  <- c(-1,4,5,5,5,5,5,5,6,6,-1)
conf$keyVarObs[4,]  <- c(7,8,9,9,9,9,9,9,9,9,-1)
conf$keyVarObs[5,1]  <- 10
#for the downweight catches
conf$fixVarToWeight<-1

# correlation structure in errors
conf$obsCorStruct[] <- c( "ID", "ID", "AR",  "AR", "ID","ID")
conf$keyCorObs[3,]<-c( -1, 0  ,0 , 0,  0,  0,  0,   0 ,  0 , -1)       ### the first -1 was 0 in Niels fit
conf$keyCorObs[4,]<-c( 1, 1  ,1 , 1,  1,  1,  1,   1 ,  1 , -1)


####### Catchabilities

    #conf$keyLogFpar[3,2:10] <- c(1,2,3,4,5,5,5,5,5)        # was fully decoupled in Niels fit
#    conf$keyLogFpar[4,1:10]     <-  c(6:14,14)
#    conf$keyLogFpar[5,1]     <-    15
#

#- Set default starting parameters
par<-defpar(dat,conf)

#- save data file
save(surveys,cn,cw,dw,lf,lw,mo,nm,pf,pm,sw,dat,recap,conf,par,file=paste0("data/data_",modName,".RData"))

#-------------------------------------------------------------------------------
# fit  model
#-------------------------------------------------------------------------------

fit <-sam.fit(dat,conf,par)
save(fit, file=paste0("model/fit_model_",modName,".RData"))

# compare with Niels's fit
#  load( file=paste0("model/fit_model_","XplConf.08",".RData"))


#-------------------------------------------------------------------------------
# runs diagnostics
#-------------------------------------------------------------------------------

#- Run retrospectives ----------------------------------------------------------
#ret<-stockassessment::retro(fit, year=7)     
#ret5peels<-stockassessment::retro(fit, year=5)
#


R <- cbind(2023-0:6, 2023-0:6, 2024-0:6, 2022-0:6, 2024-0:6,2022-0:6)
ret<-stockassessment::retro(fit, year=R)     

R <- cbind(2023-0:4, 2023-0:4, 2024-0:4, 2022-0:4, 2024-0:4,2022-0:4)
ret5peels<-stockassessment::retro(fit, year=R)     


# leave one out ----------------------------------------------------------------
lo<-leaveout(fit, fleet=c(2,3,4,5))

# residuals --------------------------------------------------------------------
options(mc.cores=3)



dat<-fit$data
dis<-dat$fleetTypes[dat$aux[,"fleet"]]==5
res<-TMB::oneStepPredict(fit$obj, observation.name="logobs", data.term.indicator="keep", discrete=FALSE, subset=which(!dis), parallel=TRUE)
#res2<-TMB::oneStepPredict(fit$obj, observation.name="logobs", data.term.indicator="keep", discrete=TRUE, conditional=which(!dis),
#                          subset=which(dis), method ="oneStepGeneric", range=c(0,Inf), parallel=TRUE)
#
totalres<-rep(NA,nrow(dat$aux))
totalres[!dis]<-res$residual
totalres[dis]<-res2$residual
RES<-data.frame(dat$aux, residual=totalres)


class(RES)<-"samres"
attr(RES, "fleetNames")<-attr(fit$data, "fleetNames") [1:5]

RESP<-procres(fit)
 
#- Save all relevant objects
save(fit, lo,ret,ret5peels,dat,par,conf,res,RES,file=paste0("model/fit_and_diagnostics/model_",modName,"_full_output.RData"))








#########
modName <- model <- "3.00"
load((paste0("model/fit_and_diagnostics/model_",modName,"_full_output.RData")))
# look at things for STF
# create FLR objects for first model    
assess.name  <- paste0("model",model)
modname      <- model
source('./boot/software/utils/SAM2FLRtmb.r')       # this is a self coded SAM2FLR function... there is surely a more elegant way to do this
fit.stck <- Mac                     # FLStock
fit.flsam<- res

R<-as.data.frame(rec(Mac),drop=T)
R$age<-2
R$quant <- "Recruitment"

sw<-as.data.frame(stock.wt(Mac),drop=T)
sw$quant <- "stock weight"

dats<-rbind(sw,R[,names(sw)])


ggplot(dats , aes(year,data,group=age)) + 
      geom_line(aes(colour=factor(year))) + 
      facet_grid(quant~.,scales="free")  +
      theme(legend.position="none")
      
      
# recruit per spawner
rps <- as.data.frame(rec(Mac)/ssb(Mac) ,drop=T     )
rps$quant <- "Recruit Per Spawner" 
rps$age<-"none"      
dats<-rbind(dats,rps[,names(dats)])
      
      
retRec<- lapply(ret , function(x) {tail(rectable(x),1)})      
retRec <- do.call(rbind.data.frame, retRec) 
retRec$year <- rownames(retRec)
retRec <-  retRec[,c("year","Estimate")]
names(retRec)[2]  <- "Term.Year.Est"

R<-as.data.frame(rec(Mac),drop=T)
R<-merge(R,retRec,all.x=T)

geomean <- retRec
names(geomean)[2]  <- "geomean"
for (yr in an(retRec$year)) geomean$geomean[geomean$year == yr]    <-  exp(mean(log(R$data[R$year > 2013 & R$year < yr])))

R<- merge(R,geomean,,all.x=T)
names(R)[2]  <- "Recruitment" 


R<-tidyr::gather(R,key=basis,value=predictor,3:4)

ggplot(subset(R,year>2010) , aes(year,Recruitment)) + 
          geom_line()   + 
          geom_point(aes(year,predictor,col=basis)) + 
        ylim(0,NA)  + ggtitle("recruitment assumption")


R$APE <- with(R,  abs(predictor-Recruitment)/Recruitment )


aggregate(APE~basis,subset(R,year >2019),function(x) mean(x, ns.omit=T)) 

ggplot(as.data.frame(stock.wt(Mac)),aes(year,data,group=age))  + geom_line()

 
ggplot(as.data.frame(stock.wt(Mac)) , aes(year,data,group=age)) + 
      geom_line(aes(colour=factor(year))) + 
      theme(legend.position="none")  
      
      
      
      