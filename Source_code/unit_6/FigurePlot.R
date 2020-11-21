library(ggplot2)
library(gridExtra)
## change to your own workingPath where the .R script is saved
#setwd('/Users/xinhuang/Documents/Work/Training2020/Unit6/Practice_U6/Code/')
args = commandArgs(T)
#text <- readLines("namelist.txt") ## read all lines from a 'namelist.txt' file
#line1<-text[1] ## get the first line
#ninput<-unlist(strsplit(line1,'!'))[1] ## get "ninput=1" or "ninput=2"
#ninput<-unlist(strsplit(ninput,'='))[2] ## get '1 ' or '2 '
#ninput<-as.numeric(ninput) ## convert character value to numerical value. 1: ambient CO2; 2: elevated CO2
ninput <- as.numeric(args[1])
#line2<-text[2] ## get the third line
#paramRange<-unlist(strsplit(line2,'!'))[1] ## split string by '!' and get the first part, then convert it from list value to string value
#paramRange<-unlist(strsplit(paramRange,'='))[2] ## split string by '=' and get the second part, then convert it from list value to string value
#paramRange<-substr(paramRange,1,nchar(paramRange)-1) ## remove the last character '/n'
paramRange = paste0("./Source_code/unit_6/input/", args[2])
param<-read.table(paramRange)
cmin<-param[1,]
cmax<-param[2,]
## Read output files
read_table = paste(args[3], "param_accepted.txt", sep = "/")
cframe<-data.frame(t(read.table(read_table)))
colnames(cframe)<-c('c1','c2','c3','c4','c5','c6','c7')
cnum<-dim(cframe)[1]
cframe$samplingTimes<-c(1:cnum)
cRangeframe<-data.frame(cmin=as.numeric(cmin),cmax=as.numeric(cmax))

## generate plots. Fig 3/4(left) in Xu et al., 2006
## the changes of paramter values in Metropolis-Hasting sampling 
png(filename = paste(args[3], 'figures/param-sampling.png', sep = "/"),width=1200,height=900) # save figures
chart1<-ggplot()+geom_line(data=cframe,aes(samplingTimes,c1))+
  theme_bw()+scale_y_continuous(limits = c(cRangeframe$cmin[1],cRangeframe$cmax[1]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart2<-ggplot()+geom_line(data=cframe,aes(samplingTimes,c2))+
  theme_bw()+scale_y_continuous(limits = c(cRangeframe$cmin[2],cRangeframe$cmax[2]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart3<-ggplot()+geom_line(data=cframe,aes(samplingTimes,c3))+
  theme_bw()+scale_y_continuous(limits = c(cRangeframe$cmin[3],cRangeframe$cmax[3]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart4<-ggplot()+geom_line(data=cframe,aes(samplingTimes,c4))+
  theme_bw()+scale_y_continuous(limits = c(cRangeframe$cmin[4],cRangeframe$cmax[4]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart5<-ggplot()+geom_line(data=cframe,aes(samplingTimes,c5))+
  theme_bw()+scale_y_continuous(limits = c(cRangeframe$cmin[5],cRangeframe$cmax[5]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart6<-ggplot()+geom_line(data=cframe,aes(samplingTimes,c6))+
  theme_bw()+scale_y_continuous(limits = c(cRangeframe$cmin[6],cRangeframe$cmax[6]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart7<-ggplot()+geom_line(data=cframe,aes(samplingTimes,c7))+
  theme_bw()+scale_y_continuous(limits = c(cRangeframe$cmin[7],cRangeframe$cmax[7]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
grid.arrange(chart1,chart2,chart3,chart4,chart5,chart6,chart7,nrow=3)
dev.off()

## generate plots. Fig 3/4(middle) in Xu et al., 2006
#Histogram for 7 parameters
png(filename = paste(args[3], 'figures/param-histogram.png', sep = "/"),width=1200,height=900) # save figures
chart1<-ggplot()+geom_histogram(data=cframe,aes(x=c1),color='white',fill='gray60',bins=100)+
  scale_x_continuous(limits = c(cRangeframe$cmin[1],cRangeframe$cmax[1]))+
  ylab('Frequency')+theme_bw()+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart2<-ggplot()+geom_histogram(data=cframe,aes(c2),color='white',fill='gray60',bins=100)+
  theme_bw()+ylab('Frequency')+
  scale_x_continuous(limits = c(cRangeframe$cmin[2],cRangeframe$cmax[2]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart3<-ggplot()+geom_histogram(data=cframe,aes(c3),color='white',fill='gray60',bins=100)+
  theme_bw()+ylab('Frequency')+
  scale_x_continuous(limits = c(cRangeframe$cmin[3],cRangeframe$cmax[3]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart4<-ggplot()+geom_histogram(data=cframe,aes(c4),color='white',fill='gray60',bins=100)+
  theme_bw()+ylab('Frequency')+
  scale_x_continuous(limits = c(cRangeframe$cmin[4],cRangeframe$cmax[4]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart5<-ggplot()+geom_histogram(data=cframe,aes(c5),color='white',fill='gray60',bins=100)+
  theme_bw()+ylab('Frequency')+
  scale_x_continuous(limits = c(cRangeframe$cmin[5],cRangeframe$cmax[5]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart6<-ggplot()+geom_histogram(data=cframe,aes(c6),color='white',fill='gray60',bins=100)+
  theme_bw()+ylab('Frequency')+
  scale_x_continuous(limits = c(cRangeframe$cmin[6],cRangeframe$cmax[6]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart7<-ggplot()+geom_histogram(data=cframe,aes(c7),color='white',fill='gray60',bins=100)+
  theme_bw()+ylab('Frequency')+
  scale_x_continuous(limits = c(cRangeframe$cmin[7],cRangeframe$cmax[7]))+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
grid.arrange(chart1,chart2,chart3,chart4,chart5,chart6,chart7,nrow=3)
dev.off()

##The changes of mismatch between observations and simulation outputs in DA
#J<-read.table(paste0(args[3], 'mismatch_accepted.txt')
#colnames(J)<-'mismatch'
#jframe<-data.frame(J)
#jframe$samplingTimes<-c(1:dim(J)[1])
#png(filename = 'figures/accepted_mismatch.png',width=1200,height=900) # save figures
#ggplot(jframe,aes(samplingTimes,mismatch))+geom_line()+theme_bw()+
#  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
#        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))+
#  labs(x='Sampling Times',y='Mismatch in Cost Function')
#dev.off()

# comparison between best modelled values and observed data sets
woodyObs<-t(read.table('./Source_code/unit_6/input/Woody.txt')[ninput+1,])
foliageObs<-t(read.table('./Source_code/unit_6/input/Foliage.txt')[ninput+1,])
litterObs<-t(read.table('./Source_code/unit_6/input/Litterfall.txt')[ninput+1,])
forestObs<-t(read.table('./Source_code/unit_6/input/ForestFloor.txt')[ninput+1,])
mineralObs<-t(read.table('./Source_code/unit_6/input/ForestMineral.txt')[ninput+1,])
soilResObs<-t(read.table('./Source_code/unit_6/input/SoilRespiration.txt')[ninput+1,])
woodySimu<-read.table(paste0(args[3], 'Woody_bestSimu.txt'))
foliageSimu<-read.table(paste0(args[3], 'Foliage_bestSimu.txt'))
litterSimu<-read.table(paste0(args[3], 'Litterfall_bestSimu.txt'))
forestSimu<-read.table(paste0(args[3], 'Forestfloor_bestSimu.txt'))
mineralSimu<-read.table(paste0(args[3], 'ForestMineral_bestSimu.txt'))
soilResSimu<-read.table(paste0(args[3], 'SoilResp_bestSimu.txt'))
wframe<-data.frame(woodyObs,woodySimu)
folframe<-data.frame(foliageObs,foliageSimu)
lframe<-data.frame(litterObs,litterSimu)
forframe<-data.frame(forestObs,forestSimu)
mframe<-data.frame(mineralObs,mineralSimu)
sframe<-data.frame(soilResObs,soilResSimu)
colnames(wframe)<-c('woodyObs','woodySimu')
colnames(folframe)<-c('foliageObs','foliageSimu')
colnames(lframe)<-c('litterObs','litterSimu')
colnames(forframe)<-c('forestObs','forestSimu')
colnames(mframe)<-c('mineralObs','mineralSimu')
colnames(sframe)<-c('soilResObs','soilResSimu')
## generate plots. Fig 8 in Xu et al., 2006
png(filename = paste(args[3], 'figures/comp_Obs_Simu.png', sep = "/"),width=1200,height=900) # save figures
chart1<-ggplot(sframe,aes(soilResObs,soilResSimu))+geom_point()+geom_abline(colour="grey")+
  theme_bw()+labs(x='Observed Soil Respiration',y='Simulated')+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart2<-ggplot(folframe,aes(foliageObs,foliageSimu))+geom_point()+geom_abline(colour="grey")+
  theme_bw()+labs(x='Observed Foliage Biomass',y='Simulated')+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart3<-ggplot(lframe,aes(litterObs,litterSimu))+geom_point()+geom_abline(colour="grey")+
  theme_bw()+labs(x='Observed Litterfall',y='Simulated')+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart4<-ggplot(mframe,aes(mineralObs,mineralSimu))+geom_point()+geom_abline(colour="grey")+
  theme_bw()+labs(x='Observed Mineral Carbon',y='Simulated')+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart5<-ggplot(wframe,aes(woodyObs,woodySimu))+geom_point()+geom_abline(colour="grey")+
  theme_bw()+labs(x='Observed Woody Biomass',y='Simulated')+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
chart6<-ggplot(forframe,aes(forestObs,forestSimu))+geom_point()+geom_abline(colour="grey")+
  theme_bw()+labs(x='Observed Forest Floor Carbon',y='Simulated')+
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
grid.arrange(chart1,chart2,chart3,chart4,chart5,chart6,nrow=3)
dev.off()
