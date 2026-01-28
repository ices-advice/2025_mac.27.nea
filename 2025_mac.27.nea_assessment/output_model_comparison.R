## Extract results of interest, write TAF output tables

#-------------------------------------------------------------------------------
# initialise
#-------------------------------------------------------------------------------

library(icesTAF)
#  taf.bootstrap()
taf.library(stockassessment)
library(ggplot2)


mkdir("output")
mkdir("output/model comparisons")


#-------------------------------------------------------------------------------
# load model runs
#-------------------------------------------------------------------------------

model1 <- "0.00"
model2 <- "0.01"

load(paste0("./model/fit_model_",model1,".RData"))

#load(paste0("./model/fit_model_",model2,".RData"))
#fit2<-fit
 rm(ret)
load(paste0("./model/fit_and_diagnostics/model_",model1,"_full_output.RData"))
fit1<-fit
ret1<-ret
ret5peels1<-ret5peels
load(paste0("./model/fit_and_diagnostics/model_",model2,"_full_output.RData"))
fit2 <- fit
ret2<-ret
ret5peels2<-ret5peels

 ######  check if they converged
 
fit1
fit2

#-------------------------------------------------------------------------------
# compare stock trajectories
#-------------------------------------------------------------------------------

pdf(paste0("./output/model comparisons/comp",model1,"and",model2,".pdf"),width=12,height=10)


## compare the stock trajectories
par(mfrow=c(2,2),mar=c(4,5,1,1))
ssbplot(fit1,col="blue",cicol=rgb(0,0,1,0.1),main="SSB")
ssbplot(fit2,add=T,col="red",cicol=rgb(1,0,0,0.1))


legend(legend = c(paste("model",model1),paste("model",model2)), col = c("blue","red") ,lwd=2 , lty=c(1),x="top" ,bg="white")
legend(legend = c(paste("Benchmark2025"),paste("WGWIDE2025")), col = c("blue","red") ,lwd=2 , lty=c(1),x="top" ,bg="white")

fbarplot(fit1,partial=F,col="blue",cicol=rgb(0,0,1,0.1),main="Fbar")
fbarplot(fit2,add=T,partial=F,col="red",cicol=rgb(1,0,0,0.1))

recplot(fit1,col="blue",cicol=rgb(0,0,1,0.1),main = "Recruitment" , las=0, drop=1)
recplot (fit2,add=T,col="red",cicol=rgb(1,0,0,0.1),las=0, drop=1)

catchplot( fit1,col="blue",cicol=rgb(1,0,1,0.1) , main = "Catch")
catchplot( fit2,add=T,col="red",cicol=rgb(1,0,0,0.1))




#-------------------------------------------------------------------------------
# compare retrospective plots
#-------------------------------------------------------------------------------


par(mfrow=c(2,3),mar=c(4,5,1,1))
ssbplot(ret1,  main=paste0("retrospective SSB mod",model1))
text(1995,15e6,paste("Mohn's rho (5yrs)=",round(mohn(ret5peels1)["SSB"],3)),pos=4)
fbarplot(ret1,partial=F,main=paste0("retrospective Fbar mod",model1))  
text(1995,0.05,paste("Mohn's rho (5yrs)=",round(mohn(ret5peels1)["Fbar(4-8)"],3)),pos=4)                                                                                                     
recplot(ret1,partial=F,main=paste0("Rec mod",model1))  
text(1995,0.05,paste("Mohn's rho (5yrs)=",round(mohn(ret5peels1)["R(age 2)"],3)),pos=4)                                                                                                     



ssbplot(ret2,  main=paste0("retrospective SSB mod",model2))
text(1995,15e6,paste("Mohn's rho (5yrs)=",round(mohn(ret5peels2)["SSB"],3)),pos=4)
fbarplot(ret2,partial=F,main=paste0("retrospective Fbar mod",model2))
text(1995,0.05,paste("Mohn's rho (5yrs)=",round(mohn(ret5peels2)["Fbar(4-8)"],3)),pos=4)                                                                                                     

recplot(ret2,partial=F,main=paste0("Rec mod",model2))  
text(1995,0.05,paste("Mohn's rho (5yrs)=",round(mohn(ret5peels2)["R(age 2)"],3)),pos=4)                                                                                                     


#-------------------------------------------------------------------------------
# compare parameter values
#-------------------------------------------------------------------------------
fits<- list(fit1,fit2)
names(fits) <- c(model1,model2)
#names(fits) <- c("Benchmark2025","WGWIDE2025")
class(fits) <- "samset"
source('./boot/software/utils/compare params Benchmark 2025_V2.r')    # this is a self coded function to plot parameters with self explaining names. will require adapation each time the number of parameters changes



# create FLR objects for first model    
assess.name  <- paste("model",model1)
modname      <- model1
fit <- fit1
source('./boot/software/utils/SAM2FLRtmb.r')       # this is a self coded SAM2FLR function... there is surely a more elegant way to do this
fit1.stck <- Mac                     # FLStock
fit1.flsam<- res                     # FLSAM

# create FLR objects for second model    
assess.name  <- paste("model",model2)
modname      <- model2
fit <- fit2
source('./boot/software/utils/SAM2FLRtmb.r')   
fit2.stck <- Mac
fit2.flsam<- res
rm(fit)

st.names <- c(paste("model",model1),paste("model",model2))
stc <- list(fit1.stck,fit2.stck)
names(stc) <- st.names
flsam <- list(fit1.flsam,fit2.flsam)
names(flsam) <- st.names
fit.lst <-list(fit1,fit2)
names(fit.lst) <- st.names
source('./boot/software/utils/compare params Benchmark 2025.r')    # this is a self coded function to plot parameters with self explaining names. will require adapation each time the number of parameters changes 


#pars$assess[pars$assess == "model 0.00"] <- "Benchmark2025"
#pars$assess[pars$assess == "model 0.01"] <- "WGWIDE2025"
#
ggplot2::ggplot(subset(pars,type ==  "observation variances"), aes(value,std.dev,label = name)) + 
      geom_point(aes(colour=assess)) + ylim(0,NA)+xlim(0,NA) + 
      geom_text(aes(colour=assess),hjust=-0.1, vjust=1) +
      xlab("parameter estimate") + ylab("Standard deviation \nof parameter estimate") + 
      scale_color_discrete(name = "Data Source")  + theme_classic()



#-------------------------------------------------------------------------------
# compare model uncertainty on SSB and Fbar
#-------------------------------------------------------------------------------

slotNames(fit2.flsam)

yr  <- an(dimnames(m(stc[[1]]))$year)
yr2 <- an(dimnames(m(stc[[2]]))$year)

head(fit1.flsam@params)
pars<-fit1.flsam@params
logssbsd1 <- pars[pars$name == "logssb" , 3]
logFbarsd1<- pars[pars$name == "logfbar" , 3]


head(fit2.flsam@params)
pars<-fit2.flsam@params
logssbsd2 <- pars[pars$name == "logssb" , 3]
logFbarsd2 <- pars[pars$name == "logfbar" , 3]



dats1  <-   data.frame( years = rep(yr,2)  , val = c(logssbsd1 , logFbarsd1) , var=c(rep("sd log ssb",length(yr)) ,  rep("sd log fbar",length(yr)))      , assess = rep (paste("model",model1) ,2*length(yr)))
dats2  <-   data.frame( years = rep(yr2,2) , val = c(logssbsd2 , logFbarsd2) , var=c(rep("sd log ssb",length(yr2)),  rep("sd log fbar",length(yr2)))     , assess = rep (paste("model",model2) ,2*length(yr2)))

dats1$year2s  <-      dats1$years  -max(dats1$years)
dats2$year2s  <-      dats2$years  -max(dats2$years)


dats <- rbind(dats1,dats2)

dats$assess[dats$assess == "model 0.00"] <- "Benchmark2025"
dats$assess[dats$assess == "model 0.01"] <- "WGWIDE2025"

g <-  ggplot(subset(dats,years>1990) ,aes (x=years , y= val , colour= assess))
g <- g + geom_line()
g <- g + facet_grid (~var)  +  theme(legend.position="bottom") + xlab("years")
g


g <-  ggplot(subset(dats,years>1990) ,aes (x=year2s , y= val , colour= assess))
g <- g + geom_line()
g <- g + facet_grid (~var)  +  theme(legend.position="bottom") + xlab("years  (to the last data year)")
g



#-------------------------------------------------------------------------------
# compare model estimates of N@age
#-------------------------------------------------------------------------------

#### plot differences in N@age
#
#
Ns1 <-as.data.frame(stock.n(fit1.stck))
Ns2 <-as.data.frame(stock.n(fit2.stck))
Ns1$assess  <- paste("model",model1)
Ns2$assess  <- paste("model",model2)
Ns <- rbind(Ns1,Ns2)
#
#
 ggplot(subset(as.data.frame(Ns) , year>2010)  , aes(age , data, color = assess) )  + 
        geom_line()+ facet_wrap(~ year)   + 
        ggtitle("difference in N@age")   +  theme(legend.position="bottom")
# 
Fs1 <-as.data.frame(harvest(fit1.stck))
Fs2 <-as.data.frame(harvest(fit2.stck))
Fs1$assess  <- paste("model",model1)
Fs2$assess  <- paste("model",model2)
Fs <- rbind(Fs1,Fs2)
#
#
 ggplot(subset(as.data.frame(Fs) , year>2000)  , aes(age , data, color = assess) )  + geom_line()+ facet_wrap(~ year)   + ggtitle("difference in F@age")   +  theme(legend.position="bottom")
# 



#-------------------------------------------------------------------------------
# compare model process error
#-------------------------------------------------------------------------------
 
###### process error

# devtools::install_github("einarhjorleifsson/fishvice")
library(fishvice)


#Mac<-trim(fit1.stck,year=1998:2023)
Mac<-fit1.stck
Mac@discards.wt<-Mac@catch.wt
Mac@discards.n[]<-0
mac.sam<-flstock_to_rbya(Mac)
mac.pe1<-sam_process_error (mac.sam, plus_group=TRUE, plot_it=F)

#Mac<-trim(fit2.stck,year=1998:2023)
Mac<-fit2.stck
Mac@discards.wt<-Mac@catch.wt
Mac@discards.n[]<-0
mac.sam<-flstock_to_rbya(Mac)
mac.pe2<-sam_process_error (mac.sam, plus_group=TRUE, plot_it=F)
 
 
pe1<- mac.pe1$rby
pe1$var<- paste("model",model1)
pe1$var<- paste("Benchmark2025" )
pe2<- mac.pe2$rby
pe2$var<- paste("model",model2)
pe2$var<- paste("WGWIDE2025" )
pe<-rbind(pe1,pe2) 
 

 
ggplot(pe,   aes( year,b,fill=var ))  + 
    geom_bar(, position = "dodge", stat="identity") + 
    ggtitle("biomass accumulated process error")+     # pe for age 1  is given age 0 by the sam_process_error function so correct for that  
    scale_fill_manual("assessment", values = c( "black",  "orange")) +
    theme(legend.position = "bottom")



pe1<- mac.pe1$rbya
pe1$var<- paste("model",model1)
pe1$var<- paste("Benchmark2025" )
pe2<- mac.pe2$rbya
pe2$var<- paste("model",model2)
pe2$var<- paste("WGWIDE2025" )

pe<-rbind(pe1,pe2) 


ggplot(subset(pe , year > 2000 & age < 11),  aes( year,n.d,fill=var ))  + 
                                  geom_bar(, position = "dodge", stat="identity") + 
                                  ggtitle("process error (deviation in numbers)")  + 
                                  facet_wrap(~c(age+1) , scale = "free")               +     # pe for age 1  is given age 0 by the sam_process_error function so correct for that  
                                  scale_fill_manual("assessment", values = c( "black",  "orange")) +
                                  theme(legend.position = "bottom")


ggplot(subset(pe , year > 2000 & age < 11),  aes( year,z.d,fill=var ))  + 
                                  geom_bar(, position = "dodge", stat="identity") + 
                                  ggtitle("process error (deviation in mortalit)")  + 
                                  facet_wrap(~c(age+1) , scale = "free")               +     # pe for age 1  is given age 0 by the sam_process_error function so correct for that  
                                  scale_fill_manual("assessment", values = c( "black",  "orange")) +
                                  theme(legend.position = "bottom")







dev.off()




