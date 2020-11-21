require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(ggplot2)

args = commandArgs(TRUE)
# args[1] is the directory for observation files
# args[2] is the directory for simulation files
# args[3] is output directory of graphs
# args[4] is the number of iterations
#args1 = "obs_file/SPRUCE_obs.txt"
#args2 = "graphoutput/Simu_dailyflux.txt"
#args3 = "graphoutput"

args1 = "obs_file/cpool.txt"
args2 = "obs_file/cflux.txt"
args3 = args[1]

cpool_obs <- read.table(args1,header=TRUE,sep="\t",na.strings = -9999)
flux_obs <- read.table(args2,header=TRUE,sep="\t",na.strings = -9999)

dates <- (as.numeric(format(Sys.Date(), "%Y")) - 2011)*365
firstday <- dates + as.numeric(as.Date(Sys.Date(), "%Y-%m-%d") - as.Date(paste(01,01,format(Sys.Date(), "%Y"),sep="-"),"%m-%d-%Y"))
lastday = firstday+7
xticklab <- seq(as.Date(Sys.Date()),as.Date(Sys.Date()+7), length.out = 8)

setwd(args3)
#load files
files <- dir(pattern = "Simu*")

data <- files %>%
  map(read_csv) %>%    
  reduce(cbind)

sdoy <- data[,grepl("^sdoy", names(data))]
gpp <- data[,grepl("^GPP_d", names(data))]
nee <- data[,grepl("^NEE_d", names(data))]
er <- data[,grepl("^Reco_d", names(data))]
foliage <- data[,grepl("^QC1", names(data))]
wood <- data[,grepl("^QC2", names(data))]
root <- data[,grepl("^QC3", names(data))]
soil <- data[,grepl("^QC678", names(data))]


common_theme <- list(theme_bw(),
                     theme(axis.text = element_text(size=23,color = "black"),
                           axis.title=element_text(size=26, color= "black"),
                           panel.grid = element_blank()),
                     geom_ribbon(aes(ymin=lowb,ymax=highb),fill='#7fc57f'),
                     geom_line(color="darkslategrey",size=1.5),
                     scale_x_continuous(breaks=firstday:lastday,labels=xticklab))


png(height=1200, width=1400,pointsize=40, file="gpp_forecast_weekly.png")
mean1 = rowMeans(gpp[firstday:lastday,])
std1 = apply(gpp[firstday:lastday,],1,sd)
lowb = mean1-std1
highb = mean1+std1
data.gpp.weekly<-data.frame(days=(firstday:lastday),gpp=mean1)

ggplot(data.gpp.weekly,aes(x=days,y=gpp)) + 
  labs(y=expression("GPP (g "~m^-2~day^-1~")"), x="Date")+
  common_theme
dev.off()

png(height=1200, width=1400,pointsize=40, file="er_forecast_weekly.png")
mean1 = rowMeans(er[firstday:lastday,])
std1 = apply(er[firstday:lastday,],1,sd)
lowb = mean1-std1
highb = mean1+std1
data.gpp.weekly<-data.frame(days=(firstday:lastday),er=mean1)
ggplot(data.gpp.weekly,aes(x=days,y=er)) + 
  labs(y=expression("ER (g "~m^-2~day^-1~")"), x="Date")+
  common_theme
dev.off()

gpp.melt <- reshape::melt(cbind("sdoy"=sdoy[,1],gpp),id="sdoy")
er.melt <- reshape::melt(cbind("sdoy"=sdoy[,1],er),id="sdoy")
foliage.melt <- reshape::melt(cbind("sdoy"=sdoy[,1],foliage),id="sdoy")
wood.melt <- reshape::melt(cbind("sdoy"=sdoy[,1],wood),id="sdoy")
root.melt <- reshape::melt(cbind("sdoy"=sdoy[,1],root),id="sdoy")
soil.melt <- reshape::melt(cbind("sdoy"=sdoy[,1],soil),id="sdoy")


common_theme_flux <- list(theme_bw(),
                          theme(panel.grid = element_blank(),
                                axis.text = element_text(size=23,color = "black"),
                                axis.title=element_text(size=26, color= "black")),
                          scale_color_manual(values = rep("black",length(colnames(gpp)))),
                          geom_line(show.legend = F),
                          scale_x_continuous(limits = c(1,length(sdoy[,1])), breaks=c(seq(1,length(sdoy[,1]),730),length(sdoy[,1])),labels=seq(2011,2025,length.out = 8)))

png(height=1200, width=1400,pointsize=40, file="gpp_forecast.png")
ggplot(data=gpp.melt, aes(sdoy, value, color = variable)) + 
  labs(y=expression("GPP (g "~m^-2~day^-1~")"), x="Years")+
  common_theme_flux +
  scale_y_continuous(limits = c(0,max(gpp)*1.4))+
  geom_point(data=flux_obs, aes(sdoy, GPP), size=7,color = "red")
dev.off()

png(height=1200, width=1400,pointsize=40, file="er_forecast.png")
ggplot(data=er.melt, aes(sdoy, value, color = variable)) + 
  labs(y=expression("ER (g "~m^-2~day^-1~")"), x="Years")+
  common_theme_flux +
  scale_y_continuous(limits = c(0,max(er)*1.04))+
  geom_point(data=flux_obs, aes(sdoy, Reco), size=7,color = "red")
dev.off()

common_theme_cpool <- list(theme_bw(),
                           theme(axis.text = element_text(size=23,color = "black"),
                                 axis.title=element_text(size=26, color= "black"),
                                 panel.grid = element_blank()),
                           geom_ribbon(aes(ymin=lowb,ymax=highb),fill='#7fc57f'),
                           geom_line(color="darkslategrey",size=1.5),
                           #scale_y_continuous(limits = c(0,1000)),
                           scale_x_continuous(limits = c(1,length(sdoy[,1])), breaks=c(seq(1,length(sdoy[,1]),730),length(sdoy[,1])),labels=seq(2011,2025,length.out = 8)))

png(height=1200, width=1400,pointsize=40, file="foliage_forecast.png")
mean1 = rowMeans(foliage)
std1 = apply(foliage,1,sd)
lowb = mean1-std1
highb = mean1+std1
data.foliage <- data.frame(days=sdoy[,1],foliage=mean1)
ggplot(data=data.foliage, aes(days, foliage)) + 
  labs(y=expression("Foliage C "~"("~g~m^-2~")"), x="Years")+
  common_theme_cpool +
  scale_y_continuous(limits = c(190,520)) +
  geom_point(data=cpool_obs, aes(sdoy, Foliage), size=7,color = "red")
dev.off()

png(height=1200, width=1400,pointsize=40, file="wood_forecast.png")
mean1 = rowMeans(wood)
std1 = apply(wood,1,sd)
lowb = mean1-std1
highb = mean1+std1
data.wood <- data.frame(days=sdoy[,1],wood=mean1)
ggplot(data=data.wood, aes(days, wood)) + 
  labs(y=expression("Wood C "~"("~g~m^-2~")"), x="Years")+
  geom_ribbon(aes(ymin=lowb,ymax=highb),fill='#7fc57f')+
  common_theme_cpool +
  scale_y_continuous(limits = c(360,910)) +
  geom_point(data=cpool_obs, aes(sdoy, Wood), size=7,color = "red")
dev.off()

png(height=1200, width=1400,pointsize=40, file="root_forecast.png")
mean1 = rowMeans(root)
std1 = apply(root,1,sd)
lowb = mean1-std1
highb = mean1+std1
data.root <- data.frame(days=sdoy[,1],root=mean1)
ggplot(data=data.root, aes(days, root)) + 
  labs(y=expression("Root C "~"("~g~m^-2~")"), x="Years")+
  geom_ribbon(aes(ymin=lowb,ymax=highb),fill='#7fc57f')+
  common_theme_cpool +
  scale_y_continuous(limits = c(195,670)) +
  geom_point(data=cpool_obs, aes(sdoy, Root), size=7,color = "red")
dev.off()

png(height=1200, width=1400,pointsize=40, file="soil_forecast.png")
mean1 = rowMeans(soil)
std1 = apply(soil,1,sd)
lowb = mean1-std1
highb = mean1+std1
data.soil <- data.frame(days=sdoy[,1],soil=mean1)
ggplot(data=data.soil, aes(days, soil)) + 
  labs(y=expression("Soil C "~"("~g~m^-2~")"), x="Years")+
  geom_ribbon(aes(ymin=lowb,ymax=highb),fill='#7fc57f')+
  common_theme_cpool +
  scale_y_continuous(limits = c(157800,158500))
#geom_point(data=cpool_obs, aes(sdoy, SoilC), size=7,color = "red")
dev.off()
