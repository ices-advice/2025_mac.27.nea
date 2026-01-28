## Extract results of interest, write TAF output tables

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
#load(paste0("./model/fit_model_",model,".RData"))




pdf(paste0("./output/model diags/model",model,".pdf"),width=12,height=10)


#-------------------------------------------------------------------------------
# plot stock trajectory
#-------------------------------------------------------------------------------


par(mfrow=c(2,2),mar=c(4,5,1,1))
ssbplot(fit)
legend(legend = paste("model",model), lwd=2 , lty=c(1),x="top" ,bg="white")
fbarplot(fit)
recplot(fit)
catchplot(fit)


#-------------------------------------------------------------------------------
# plot fitted vs observed
#-------------------------------------------------------------------------------


fitplot(fit, fleet=1)
fitplot(fit, fleet=2, ylim=c(14, 16))
fitplot(fit, fleet=3)
fitplot(fit, fleet=4)

if(fit$data$fleetTypes[5]!=5) fitplot(fit, fleet=5)
source("./boot/software/utils/fitplot.tags.r")
fitplot.tags(fit,fleets=which(fit$data$fleetTypes==5))


#-------------------------------------------------------------------------------
# plot residuals
#-------------------------------------------------------------------------------

plot(RES)
#tgsub <- subset(TAGRES ,Type==2)
#plotby(TAGRES$year, TAGRES$RecaptureY-TAGRES$year, TAGRES$residual, by=TAGRES$age, xlab="Year", ylab="No years out"); title(paste(round(range(TAGRES$residual),1), collapse=" - "))
plot(RESP)

#plotby(tgsub$year, tgsub$RecaptureY-tgsub$year, tgsub$residual, by=tgsub$age, xlab="Year", ylab="No years out"); title(paste(round(range(tgsub$residual),1), collapse=" - "))


#-------------------------------------------------------------------------------
# plot leave one out
#-------------------------------------------------------------------------------


## compare the stock trajectories
require(RColorBrewer)
colbrew <- brewer.pal(length(lo),"Set1")
par(mfrow=c(3,1),mar=c(4,5,1,1))
ssbplot(fit,         col="black",  cicol=rgb(0,0,0,0.1),main="leave one out SSB")
for(i in 1:length(lo))
    ssbplot(lo[[i]],  add=T, col=colbrew[i],   ci=F)
ssbplot(fit, add=T,  col="black",  ci=F)
legend(legend =c("all in", names(lo)), col = c("black",colbrew) ,lwd=3 , lty=c(1),x="topleft" ,bg="white")

fbarplot(fit, partial = F ,        col="black",  cicol=rgb(0,0,0,0.1),main="leave one out Fbar")
for(i in 1:length(lo))
    fbarplot(lo[[i]], partial = F , add=T, col=colbrew[i],   ci=F)
fbarplot(fit, partial = F , add=T, col="black",  ci=F)

recplot(fit,         col="black",  cicol=rgb(0,0,0,0.1),main="leave one out Recruitment")
for(i in 1:length(lo))
    recplot(lo[[i]],  add=T, col=colbrew[i],   ci=F)
recplot(fit, add=T,  col="black",  ci=F)
 


#-------------------------------------------------------------------------------
# plot retrospective 
#-------------------------------------------------------------------------------




par(mfrow=c(3,1),mar=c(4,5,1,1))
ssbplot(ret,  main="retrospective SSB")
text(1998,13.5e6,paste("Mohn's rho =",round(mohn(ret5peels)["SSB"],3)),pos=4)
fbarplot(ret,partial=F,main="retrospective Fbar")  
text(1998,0.05,paste("Mohn's rho =",round(mohn(ret5peels,lag = 1)["Fbar(4-8)"],3)),pos=4)                                                                                                     
recplot(ret,main="retrospective Rec")
text(1998,2.8e7,paste("Mohn's rho =",round(mohn(ret5peels)["R(age 2)"],3)),pos=4)                                                                                                     


#-------------------------------------------------------------------------------
# plot parameter values
#-------------------------------------------------------------------------------

# create FLR objects for first model    
assess.name  <- paste0("model",model)
modname      <- model
source('./boot/software/utils/SAM2FLRtmb.r')       # this is a self coded SAM2FLR function... there is surely a more elegant way to do this
save(Mac, file="./output/Mac_FLStock.RData")

fit.stck <- Mac                     # FLStock
fit.flsam<- res  

st.names <- assess.name
stc <- list(fit.stck)
names(stc) <- assess.name
flsam <- list(fit.flsam)
names(flsam) <- assess.name
fit.lst <-list(fit)
names(fit.lst) <- st.names
source('./boot/software/utils/compare params Benchmark 2025.r')    # this is a self coded function to plot parameters with self explaining names. will require adapation each time the number of parameters changes

pars<-pars[,c("type","name","value","std.dev","CIlow","CIup","assess")]
write.csv(pars , paste0("./output/model diags/parameters/pars_",model,".csv"), row.names=FALSE)


print(ggplot(pars, aes(name,std.dev,fill=type)) + geom_bar(stat="identity") + coord_flip())  + facet_wrap(~type)



# obsvar versus SD

pars$name <-gsub("Residual c","C",pars$name)
ggplot2::ggplot(subset(pars,type ==  "observation variances"), aes(value,std.dev,label = name)) + 
      geom_point() + ylim(0,NA)+xlim(0,NA) + 
      geom_text(hjust=1, vjust=1) +
      xlab("parameter estimate") + ylab("Standard deviation \nof parameter estimate") + 
      scale_color_discrete(name = "Data Source")  + theme_classic()

### retro in paramters
fits<-as.list(ret)
names(fits) <- paste("retro", c(1:length(ret)-1))
source('./boot/software/utils/compare params Benchmark 2025_V2.r')    # this is a self coded function to plot parameters with self explaining names. will require adapation each time the number of parameters changes


#### retro in assessment undertainty
u<-as.data.frame(summary(fit$sdrep))
u$quant <- row.names(u)

sdssb<- u[grep("logssb",u$quant),]
sdssb$year <- seq(1998,1997+dim(sdssb)[1])
sdssb$peel <- 0
sdssb<-sdssb[,c("year","Std. Error","peel")]
sdssb$quant <- "sdLogSSB"

sdfbar<- u[grep("logfbar",u$quant),]
sdfbar$year <- seq(1998,1997+dim(sdfbar)[1])
sdfbar$peel <- 0
sdfbar<-sdfbar[,c("year","Std. Error","peel")]
sdfbar$quant <- "sdLogFbar"

resfit<-rbind(sdfbar,sdssb)


res<-list()

for ( i in 1:length(ret))
{
mod <- ret[[i]]
u<-as.data.frame(summary(mod$sdrep))
u$quant <- row.names(u)

sdssb<- u[grep("logssb",u$quant),]
sdssb$year <- seq(1998,1997+dim(sdssb)[1])
sdssb$peel <- i
sdssb<-sdssb[,c("year","Std. Error","peel")]
sdssb$quant <- "sdLogSSB"

sdfbar<- u[grep("logfbar",u$quant),]
sdfbar$year <- seq(1998,1997+dim(sdfbar)[1])
sdfbar$peel <- i
sdfbar<-sdfbar[,c("year","Std. Error","peel")]
sdfbar$quant <- "sdLogFbar"

res[[i]]<-rbind(sdfbar,sdssb)
}

res <- do.call("rbind",res)
res<-rbind(resfit,res)


names(res)[2] <- "Std.Error"

ggplot2::ggplot(subset(res,peel<6),aes(year,Std.Error,group=peel)) +
        geom_line(aes(colour = factor(peel))) +
        facet_grid(.~quant)


#### correlation    in the errors for the IESSNS
par(mfrow=c(1,2))
cc<- fit$rep$obsCov[[3]]
ccc<-cc
for (i in 1:9)
{
for (j in 1:9)
{
ccc[i,j]  <-  cc[i,j] /  (sqrt(cc[i,i]) * sqrt(cc[j,j]))
}
}

row.names(ccc)   <- paste("Age", 3:11)
 ccc<-t(ccc)
 row.names(ccc)   <- paste("Age", 3:11)

source('./boot/software/utils/correlation plot.r')
my.plotcorr(ccc,upper.panel = c( "number"))                                  

#stockassessment::corplot(fit) 


#### correlation    in the errors for the IESSNS

cc<- fit$rep$obsCov[[3]]
ccc<-cc
for (i in seq(dim(cc)[1]))
{
for (j in seq(dim(cc)[1]))
{
ccc[i,j]  <-  cc[i,j] /  (sqrt(cc[i,i]) * sqrt(cc[j,j]))
}
}

row.names(ccc)   <- paste("Age", 3:11)
 ccc<-t(ccc)
 row.names(ccc)   <- paste("Age", 3:11)

source('./boot/software/utils/correlation plot.r')
my.plotcorr(ccc,upper.panel = c( "number"))         


cc<- fit$rep$obsCov[[4]]
ccc<-cc
for (i in seq(dim(cc)[1]))
{
for (j in seq(dim(cc)[1]))
{
ccc[i,j]  <-  cc[i,j] /  (sqrt(cc[i,i]) * sqrt(cc[j,j]))
}
}

row.names(ccc)   <- paste("Age", 2:11)
 ccc<-t(ccc)
 row.names(ccc)   <- paste("Age", 2:11)

source('./boot/software/utils/correlation plot.r')
my.plotcorr(ccc,upper.panel = c( "number"))         









restab<-data.frame(year  = RES$year, 
                   fleet = RES$fleet, 
                   age   = RES$age,
                   RecaptureY = RES$RecaptureY,
                   Yearclass = RES$Yearclass,
                   Nscan = RES$Nscan,
                   R = RES$R,
                   Type = RES$Type,
                   residual = RES$residual)
                   

myres <- subset(restab,fleet %in% 1 & year > 2000)
class(myres)<-"samres"
attr(myres, "fleetNames")<-attr(fit$data, "fleetNames") [c(1)]
empirobscorrplot(myres)     


myres <- subset(restab,fleet %in% c(3) & year > 2000)
class(myres)<-"samres"
attr(myres, "fleetNames")<-attr(fit$data, "fleetNames") [c(3)]
empirobscorrplot(myres)


myres <- subset(restab,fleet %in% c(4) & year > 2000)
class(myres)<-"samres"
attr(myres, "fleetNames")<-attr(fit$data, "fleetNames") [c(4)]
empirobscorrplot(myres)







### NB need to modify the object RES for this to work
                            # perhaps making one res per survey suing the code in model run 
                            # would be easiest


#-------------------------------------------------------------------------------
# plot parameter correlations
#-------------------------------------------------------------------------------

cov<-fit$sdrep$cov.fixed

cor <- cov

for (i in 1:dim(cov)[1])
for (j in 1: dim(cov)[2])
{
cor[i,j] <-    cov[i,j] / (cov[i,i]^0.5 * cov[j,j]^0.5)
}


  class(cor)

pars$lab<- pars$type
pars$lab[pars$type=="scaling parameters"] <- "Q"
pars$lab[pars$type=="process variances"] <- "procVar"
pars$lab[pars$type=="observation variances"] <- "obsVar"

idx<-which(colnames(cor)== "transfIRARdist")

colnames(cor)[-c(idx)]   <- rownames(cor)[-c(idx)] <- paste(pars$lab,pars$name)

diag(cor)  <- NA

library(lattice)
cols2 <- colorRampPalette(c("blue","white","red"))(256)
levelplot(cor,col.regions=cols2 , at=seq(-1, 1, .01),scales=list(x=list(rot=90)))


#-------------------------------------------------------------------------------
# plot process error realisation
#-------------------------------------------------------------------------------

# devtools::install_github("einarhjorleifsson/fishvice")
library(fishvice)


Mac<-fit.stck
Mac@discards.wt<-Mac@catch.wt
Mac@discards.n[]<-0
mac.sam<-flstock_to_rbya(Mac)
mac.pe1<-sam_process_error (mac.sam, plus_group=TRUE, plot_it=F)

pe<- mac.pe1$rby
pe$var<- assess.name
 
ggplot(pe,   aes( year,b,fill=var ))  + 
    geom_bar(, position = "dodge", stat="identity") + 
    ggtitle("biomass accumulated process error")+     # pe for age 1  is given age 0 by the sam_process_error function so correct for that  
    scale_fill_manual("assessment", values = c( "black",  "orange")) +
    theme(legend.position = "bottom")

pe<- mac.pe1$rbya
pe$var<- assess.name

 
ggplot(subset(pe , year > 2000 & age < 11),  aes( year,n.d,fill=var ))  + 
                                  geom_bar(, position = "dodge", stat="identity") + 
                                  ggtitle("process error (deviation in numbers)")  + 
                                  facet_wrap(~c(age+1) , scale = "free")               +     # pe for age 1  is given age 0 by the sam_process_error function so correct for that  
                                  scale_fill_manual("assessment", values = c( "black",  "orange")) +
                                  theme(legend.position = "bottom")



#-------------------------------------------------------------------------------
# plot election pattern
#-------------------------------------------------------------------------------

sel<-sweep(fit.stck@harvest,c(2:6),FUN="/",quantMeans(fit.stck@harvest[as.character(4:8),]))


xyplot(data ~ age|sprintf("%i's",floor((year)/5)*5),sel,
       groups=year,type="l",as.table=TRUE,
       scale=list(alternating=FALSE),
       main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar")



xyplot(data~factor(year),groups=age,data=sel[,as.character(1998:2024)],
                type="l",
               # auto.key=list(columns = 2,text=c("age 0","age 1","age 2","age 3","age 4","age 5","age 6","age 7-12+")),
                xlab="years",
                ylab="Selection Pattern" )
                
                
wireframe(data~age+year,ylim=c(2000,2024), xlim=c(0,10) ,
                            data=sel,
                            main="Exploitation pattern (2000-2024)")                
                

sel <- as.data.frame(sel[1:10,as.character(1998:2024)],drop=T)
sel$age <- ac(sel$age)
sel$age[sel$age=="11"] <- "11 and older"


print(ggplot( sel , aes(year,data,group=age,colour=age)) + geom_line() + ylab("exploitation pattern") + theme_classic())





#-------------------------------------------------------------------------------
# plot exploitable biomass
#-------------------------------------------------------------------------------
 

Fmax <- apply(harvest(fit.stck),c(2:6),max)

sel <- sweep(harvest(fit.stck),c(2:6),Fmax,"/")

ExBiom <- as.data.frame(quantSums(stock.n(fit.stck) * sel * catch.wt(fit.stck)))
ExBiom$type<- "Exploitable Stock Biomass"

SSB <- as.data.frame(ssb(fit.stck))
SSB$type <- "Spawning Stock Biomass"

TSB <- as.data.frame(tsb(fit.stck))
TSB$type  <- "Total Stock Biomass"

dats <- do.call(rbind , list(ExBiom,SSB,TSB))


ggplot(subset(dats , year > 2000) , aes(year,data,colour = type)) + geom_line()  + ylab("tonnes") 



dev.off()

