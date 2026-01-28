

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


# FC2 basis for advice 
res<- as.data.frame(attr(FC2,"shorttab"))
res2024<- c(summary(fit)["2024",] [c(7,1,4)]  , # same order as in shorttab
            catchtable(fit)["2024","Estimate"]  )
res<-cbind(res2024,res)
names(res)[1] <- 2024
res$quant <- c("Fbar","rec","SSB","catch")
res$run <- "WGWIDE2025"
res1 <- tidyr::gather(res , key=year,value=data,1:4)


# load STF results  from previous forecast
load("./model/STF_ret.RData")

res<- as.data.frame(attr(FC1ret,"shorttab"))
res$quant <- c("Fbar","rec","SSB","catch")
res$run <- "Benchmark2025"
res2 <- tidyr::gather(res , key=year,value=data,1:3)

 res<-rbind(res1,res2)



library(ggplot2)
ggplot(res , aes(year,data,grou=run)) + 
              geom_point(aes(colour=run))  + 
              geom_line(aes(colour=run))  +
              facet_grid(quant~. ,scales ="free")
              
              
              
              
res<-tidyr::spread(res , key=run , value = data)              



