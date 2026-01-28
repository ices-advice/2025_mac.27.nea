

#-------------------------------------------------------------------------------
# initialise
#-------------------------------------------------------------------------------

library(icesTAF)
taf.library(stockassessment)
library(ggplot2)     
#taf.bootstrap()
library(FLCore)



mkdir("output")
mkdir("output/model diags")
mkdir("output/model diags/explorations/")

set.seed(123)


# load model run
model <- "0.01"
load(paste0("./model/fit_and_diagnostics/model_",model,"_full_output.RData"))
# load config
load("./model/STFconf.RData")
# load STF results
load("./model/STF.RData")
FCs <- list(FC1,FC2,FC3,FC4,FC5,FC6,FC7,FC8)




tab <- lapply(FCs , function(fc) 
{
res <- attr(fc,"shorttab")
res<-as.data.frame(res)
pBlim <- c()
for (fy in 1:3)  pBlim[fy] <- sum(fc[[fy]]$ssb < Blim)/length(fc[[fy]]$ssb)
res[paste(attr(fc,"label"),"pBlim",sep=":"),] <-  pBlim
return(res)
})

tab <- do.call("rbind",tab)
ref <- do.call("rbind",strsplit(row.names(tab),":"))
ref<-as.data.frame(ref)
names(ref) <- c("option","quant")

tab<-cbind(ref,tab)
row.names(tab) <- NULL



# table for intermediate year
imy <- subset(tab[,1:3] , option == unique(option)[[1]])
R <- rectable(fit)[,1]
R <- R[as.character(2014:(ay))]
R_geoMean <- exp(mean(log(R))) # geomean is reported in advice sheet

imy$'2025' [imy$quant == "rec"]  <- R_geoMean


# catch option table
  opts             <- unique(tab$option)
  catch2026        <- subset(tab, quant == "catch" )$'2026'
  fbar20262027     <- subset(tab, quant == "fbar" )$'2026'
  ssb2026          <- subset(tab, quant == "ssb" )$'2026'
  ssb2027          <- subset(tab, quant == "ssb" )$'2027'
  chgssb2026_27    <- round( 100 *(ssb2027 - ssb2026) / ssb2026,2 )
  chcatch          <- round( 100 *(catch2026 - IYC) / IYC, 2 )
  chAdv            <- round( 100 *(catch2026 - Cur.Adv) / Cur.Adv, 2 )
  pblim            <- subset(tab, quant == "pBlim" )$'2027'
  
  
options <- data.frame(options = opts, 
                      'Total Catch 2026'= catch2026,
                      'Fage4-8'=    fbar20262027,
                      'SSB 2026'= ssb2026,
                      'SSB 2027'= ssb2027,
                      '% change SSB'=   chgssb2026_27,
                      '% change Catch'= chcatch ,
                      '% change Advice' = chAdv,
                      'p(SSB(2027)<Blim)' = pblim)
                      
write.csv(options , file = "./output/catchoptions.csv") 





# look at stability
st <- tab[,1:3]
st <- tidyr::spread(st,key = quant , value = '2025')
st <- st[,-5]
                     
R2026 <- data.frame(year = 2026 , R= FCs[[1]][[2]]$rec  )
R2027 <- data.frame(year = 2027 , R= FCs[[1]][[3]]$rec )
  
Rs<-rbind(R2026,R2027)  

library(ggplot2)
ggplot(Rs , aes(R)) + geom_density(aes(colour = factor(year)))





##  look at the age composition of SSB and catch in advice

fc <- FCs[[2]]

sims<-lapply(as.list(1:3), function(y)

{ # year to extract
dim(fc[[y]]$sim) # all iterations logN and logF at age with rows=iterations, columns=logN and then logF
fit <- attr(fc, "fit") # the fit of the model was saved as attribute, can be used to get the model configuration
idx <- (fit$conf$minAge: fit$conf$maxAge)-1 # ages to extract logN
logN <- fc[[y]]$sim[,idx] # ages 0 to 8+
logF <- fc[[y]]$sim[,(max(idx)+1):ncol(fc[[y]]$sim)] # ages 0 to 7+
# note only 8 ages for logF because the last 2 ages were configured to have the same F:
fit$conf$keyLogFsta[1,]
# natural scale :
caa <- fc[[y]]$catchatage

logN<-as.data.frame(logN)
names(logN)<-2:12
N<-exp(logN)
N<-apply(N,2,median)

logF<-as.data.frame(logF)
names(logF)<-2:11
F.<-exp(logF)
F.<-apply(F.,2,median)

Caa <- apply(caa,1,median)

res <- list(N=N,F.=F.,caa=Caa)

})


Ns<-do.call(function (x) rbind(x$N), sims)
Ns <- rbind(sims[[1]]$N,rbind(sims[[2]]$N,sims[[3]]$N))
rownames(Ns) <- as.character(2025:2027)
Ns<- FLQuant(t(Ns),dimnames=list(year= 2025:2027,age=2:12))


Fs<-do.call(function (x) rbind(x$F.), sims)
Fs <- rbind(sims[[1]]$F.,rbind(sims[[2]]$F.,sims[[3]]$F.))
Fs <- cbind(Fs, Fs[,10])
rownames(Fs) <- as.character(2025:2027)
Fs<- FLQuant(t(Fs),dimnames=list(year= 2025:2027,age=2:12))


caa   <- rbind(sims[[1]]$caa,rbind(sims[[2]]$caa,sims[[3]]$caa))
rownames(caa) <- as.character(2025:2027)
caa<- FLQuant(t(caa),dimnames=list(year= 2025:2027,age=2:12))


load("./output/Mac_FLStock.RData")
library(FLash)
library(FLAssess)

Mac.stf <- stf(window(Mac,end = 2024), nyears=3,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE,fbar.nyears=5)
stock.n(Mac.stf)[,ac(2025:2027)] <- Ns
harvest(Mac.stf)[,ac(2025:2027)] <- Fs
catch.n(Mac.stf)[,ac(2025:2027)] <- caa
Mac.stf<-window(Mac.stf, start = 2022)



SSBatage <- stock.wt(Mac.stf) * stock.n(Mac.stf) * exp(- m.spwn(Mac.stf) * m(Mac.stf) - harvest.spwn(Mac.stf)*harvest(Mac.stf)) * mat(Mac.stf)
Catage  <- catch.n(Mac.stf) * catch.wt(Mac.stf)


SSBatage <- as.data.frame(SSBatage,drop = T)
SSBatage$quant <- "SSB"
Catage<-  as.data.frame(Catage, drop=T)
Catage$quant <- "Catches"
res<-rbind(SSBatage,Catage)
res$source <- "SAM"
res$source[res$year == "2026" & res$age == 2] <- "R2026"
res$source[res$year == "2027" & res$age == 3] <- "R2026"
res$source[res$year == "2027" & res$age == 2] <- "R2027"


#catch 2026 plot 
c2026 <- subset(res , quant == "Catches" & year == 2026)

c2026$p <- 100*c2026$data/sum(c2026$data)
c2026$ypos <-  cumsum((c2026$p))  -0.5 * c2026$p
c2026$plab <- paste0("age",c2026$age,": ",round(c2026$p,1),"%")

 

ggplot(c2026 , aes(x="" ,y= p , fill = source)) + 
                      geom_bar(stat= "identity",width = 1 , color= "white") + 
                      coord_polar( "y", start=0 )  +
                      theme_void() + 
                      theme(plot.title = element_text(hjust = 0.5, vjust = -2)) +                       
                      geom_text(aes(label = plab), color = "black", size=3,position = position_stack(vjust = .4))   +
#                      geom_text(aes(y = ypos, label = plab), color = "black", size=3)   +
                      ggtitle("2026 Catches")
                      
#SSB 2027 plot 

ssb27 <- subset(res , quant == "SSB" & year == 2027)

ssb27$p <- 100*ssb27$data/sum(ssb27$data)
ssb27$ypos <-  cumsum((ssb27$p)) - 0.5 * ssb27$p
ssb27$plab <- paste0("age",ssb27$age,": ",round(ssb27$p,1),"%")

ggplot(ssb27 , aes(x="" ,y= p , fill = source)) + 
                      geom_bar(stat= "identity",width = 1 , color= "white") + 
                      coord_polar( "y", start= 0 )  +
                      theme_void() +   
                      theme(plot.title = element_text(hjust = 0.5, vjust = -2)) + 
                      geom_text(aes( label = plab), color = "black", size=3,position = position_stack(vjust = .4))   +
                      ggtitle("2027 SSB")                      
                      
                      