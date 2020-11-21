# this script plot simulated daily Carbon pools and fluxes

# # -------  install these packages if you haven't -----------
# 
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("stringr")
# install.packages("readxl")
# install.packages("Hmisc")
# install.packages("grDevices")
# 
# # ----------------------------------------------------------

library(data.table)
library(ggplot2)
library(stringr)
library(readxl)
library(Hmisc)
library(grDevices)

  a = dirname(rstudioapi::getActiveDocumentContext()$path)
  smwd=paste(a,"/output/SPRUCE/results_example",sep = "")  # 'sep' set for connection
  print(smwd)   

  sowd=paste(a,"/input/SPRUCE",sep = "")
  print(sowd)

  ###  main code begins ####
  ###  c pool and flux ####
  setwd(smwd)
  #### set a empty n*m matrix  ############
  
  tot_para<-20

  GPP<-matrix(NA,2190,tot_para)
  NEE<-matrix(NA,2190,tot_para)
  Reco<-matrix(NA,2190,tot_para)
  QC1<-matrix(NA,2190,tot_para)
  GL<-matrix(NA,2190,tot_para)
  QC2<-matrix(NA,2190,tot_para)
  GW<-matrix(NA,2190,tot_para)
  QC3<-matrix(NA,2190,tot_para)
  GR<-matrix(NA,2190,tot_para)
  QC678<-matrix(NA,2190,tot_para)
  pheno<-matrix(NA,2190,tot_para)
  LAI<-matrix(NA,2190,tot_para)

  #### read 001-500 files in a for loop ####
  for (m in 1:tot_para)
  {
    
    openfile_0<-paste("Simu_dailyflux",str_pad(m, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    # openfile_0<-paste("Simu_dailyflux",str_pad(100, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    d_0 = read.table(openfile_0,strip.white=TRUE,header=T,sep=",")

    GPP[,m]<-d_0[,3]
    NEE[,m]<-d_0[,4]
    Reco[,m]<-d_0[,5]
    QC1[,m]<-d_0[,6]
    GL[,m]<-d_0[,7]
    QC2[,m]<-d_0[,8]
    GW[,m]<-d_0[,9]
    QC3[,m]<-d_0[,10]
    GR[,m]<-d_0[,11]
    QC678[,m]<-d_0[,12]
    pheno[,m]<-d_0[,13]
    LAI[,m]<-d_0[,14]
    
    # GPP[,m]<-d_0[,2]
    # NEE[,m]<-d_0[,3]
    # Reco[,m]<-d_0[,4]
    # QC1[,m]<-d_0[,5]
    # GL[,m]<-d_0[,6]
    # QC2[,m]<-d_0[,7]
    # GW[,m]<-d_0[,8]
    # QC3[,m]<-d_0[,9]
    # GR[,m]<-d_0[,10]
    # QC678[,m]<-d_0[,11]
    # pheno[,m]<-d_0[,12]
    # LAI[,m]<-d_0[,13]
    
  }
  
  #### cal mean and sd for the 500 files from different para sets ####
  GPP_mean1<-rowMeans(GPP, na.rm =TRUE, dims = 1)
  NEE_mean1<-rowMeans(NEE, na.rm =TRUE, dims = 1)
  Reco_mean1<-rowMeans(Reco, na.rm =TRUE, dims = 1)
  QC1_mean1<-rowMeans(QC1, na.rm =TRUE, dims = 1)
  GL_mean1<-rowMeans(GL, na.rm =TRUE, dims = 1)
  QC2_mean1<-rowMeans(QC2, na.rm =TRUE, dims = 1)
  GW_mean1<-rowMeans(GW, na.rm =TRUE, dims = 1)
  QC3_mean1<-rowMeans(QC3, na.rm =TRUE, dims = 1)
  GR_mean1<-rowMeans(GR, na.rm =TRUE, dims = 1)
  QC678_mean1<-rowMeans(QC678, na.rm =TRUE, dims = 1)
  pheno_mean1<-rowMeans(pheno, na.rm =TRUE, dims = 1)
  LAI_mean1<-rowMeans(LAI, na.rm =TRUE, dims = 1)

  
  GPP_sd1<-apply(GPP, 1, sd) #1 is apply the function sd to all the elements of each row
  NEE_sd1<-apply(NEE, 1, sd) #2 is apply the function sd to all the elements of each column
  Reco_sd1<-apply(Reco, 1, sd)
  QC1_sd1<-apply(QC1, 1, sd)
  QC2_sd1<-apply(QC2, 1, sd)
  QC3_sd1<-apply(QC3, 1, sd)
  QC678_sd1<-apply(QC678, 1, sd)
  GL_sd1<-apply(GL, 1, sd)
  GW_sd1<-apply(GW, 1, sd)
  GR_sd1<-apply(GR, 1, sd)
  pheno_sd1<-apply(pheno, 1, sd)
  LAI_sd1<-apply(LAI, 1, sd)

  
  # dat.carbonpf_CWE<-read.csv("Simu_dailyflux14001.csv", header=T,sep=",")
  # DOY.date<-as.Date(dat.carbonpf_CWE$sdoy,origin='2010-12-31')
  
  ###   bread obs c pool
  setwd(sowd)
  # dat.obsbiomass<-read.table("obs_cpool.txt", header=T)
  
  leafbmdates <- c(as.Date(271,origin='2010-12-31'),as.Date(636,origin='2010-12-31'),as.Date(1002,origin='2010-12-31'),as.Date(1367,origin='2010-12-31'),as.Date(1732,origin='2010-12-31'),as.Date(2097,origin='2010-12-31'),as.Date(2462,origin='2010-12-31'))
  woodbmdates <- c(as.Date(271,origin='2010-12-31'),as.Date(636,origin='2010-12-31'),as.Date(1002,origin='2010-12-31'),as.Date(1367,origin='2010-12-31'),as.Date(1732,origin='2010-12-31'),as.Date(2097,origin='2010-12-31'),as.Date(2462,origin='2010-12-31'))
  # rootbmdates <- c(as.Date(944,origin='2010-12-31'))
  rootbmdates <- c(as.Date(579,origin='2010-12-31')) # updated date according to Iverson: 2012 data
  # # leafbiomass <- c(562.896672, 550.055296, 624.228,640.818,665.218,705.1518188) # old plor area tauleaf 1.2 tauwood 20. tauroot 1.2
  # leafbiomass <- dat.obsbiomass$Foliage #c(296.9, 361.3,360.0,411.0,407.2,436.02,465.5) # updated moss leave C mass, in accordance with 2016-2017 data
  # # woodbiomass <- c(736.410528,776.354304,865.212,912.002,920.502,982.2305612)
  # woodbiomass <- dat.obsbiomass$Wood #c(453,470,540,560,551,601,615)

  leafbiomass <- c(296.9, 361.3,360.0,411.0,407.2,436.02,465.5) # updated moss leave C mass, in accordance with 2016-2017 data
  woodbiomass <- c(453,470,540,560,551,601,615)
  rootbiomass <- 377.55
  
  # start to plot
  
  linewid=1
  alphavalue=0.5
  lt1=1
  lt2=3
  text_size=1.
  axis_size=1.
  fontsize=1.
  daystartplot<-as.Date(1,origin='2010-12-31')
  dayendplot = as.Date(2190,origin='2010-12-31')
  
  DOY.date<-c(as.Date(1:2190,origin='2010-12-31'))
  par(cex.axis=fontsize, cex.lab=fontsize, cex.main=fontsize, cex.sub=fontsize)
  par(mfrow=c(3,3),mar=c(2, 4.5, 2, 1))
  #create color palette:
  library(RColorBrewer)
  coul = brewer.pal(6, "RdYlGn")
  colline='#0571b0'
  colline2='#008837'
  colline3='#7b3294'
  colpoint = 'black'
  
  #### leaf pool
  # plot(1,2)
  plot(DOY.date, QC1_mean1,type="l", ylim=c(0,1000),xlim=c(daystartplot,dayendplot),col=coul[6],axes = FALSE,
       xaxs = "i",yaxs = "i",
       ylab='Leafpool (g m-2)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( QC1_mean1-2*QC1_sd1,
                                       rev( QC1_mean1+2*QC1_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  par(lty=lt1)
  lines(DOY.date, QC1_mean1,type="l",col=coul[6] )
  points(leafbmdates,leafbiomass,col = 'red',pch = 16)
  
  box()

  # #### wood pool
  plot(DOY.date, QC2_mean1,type="l", ylim=c(0,1000),xlim=c(daystartplot,dayendplot),col=coul[6],axes = FALSE,
       xaxs = "i",yaxs = "i",
       ylab='Woodpool (g m-2)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( QC2_mean1-2*QC2_sd1,
                                       rev( QC2_mean1+2*QC2_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  par(lty=lt1)
  lines(DOY.date, QC2_mean1,type="l",col=coul[6] )
  points(woodbmdates,woodbiomass,col = 'red',pch = 16)
  
  box()
  # points(woodbmdates,woodbiomass,col = 'RED',pch = 16)
  # #### root pool
  plot(DOY.date, QC3_mean1,type="l", ylim=c(0,1000),xlim=c(daystartplot,dayendplot),col=coul[6],axes = FALSE,
       xaxs = "i",yaxs = "i",
       ylab='Rootpool (g m-2)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( QC3_mean1-2*QC3_sd1,
                                       rev( QC3_mean1+2*QC3_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  par(lty=lt1)
  lines(DOY.date, QC3_mean1,type="l",col=coul[6] )
  points(rootbmdates,rootbiomass,col = 'red',pch = 16)
  box()

  ## leaf growth pool
  plot(DOY.date, GL_mean1,type="l", ylim=c(0,300),xlim=c(daystartplot,dayendplot),col=coul[6],axes = FALSE,
       xaxs = "i",yaxs = "i",
       ylab='LeafGpool (g m-2 d-1)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( GL_mean1-2*GL_sd1,
                                       rev( GL_mean1+2*GL_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  par(lty=lt1)
  lines(DOY.date, GL_mean1,type="l",col=coul[6] )
  box()
  
  ## wood growth pool
  plot(DOY.date, GW_mean1,type="l", ylim=c(0,300),xlim=c(daystartplot,dayendplot),col=coul[6],axes = FALSE,
       xaxs = "i",yaxs = "i",
       ylab='woodGpool (g m-2 d-1)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( GW_mean1-2*GW_sd1,
                                       rev( GW_mean1+2*GW_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  par(lty=lt1)
  lines(DOY.date, GW_mean1,type="l",col=coul[6] )
  box()
  
  ## root growth pool
  plot(DOY.date, GR_mean1,type="l", ylim=c(0,300),xlim=c(daystartplot,dayendplot),col=coul[6],axes = FALSE,
       xaxs = "i",yaxs = "i",
       ylab='rootGpool (g m-2 d-1)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( GR_mean1-2*GR_sd1,
                                       rev( GR_mean1+2*GR_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  par(lty=lt1)
  lines(DOY.date, GR_mean1,type="l",col=coul[6] )
  
  box()
  
  
  # ## QC678 soil C biomass
  # plot(DOY.date, QC678_mean1,type="l", ylim=c(155000,160000),xlim=c(daystartplot,dayendplot),col=coul[6],axes = FALSE,
  #      xaxs = "i",yaxs = "i",
  #      ylab='SoilCpool (g m-2 d-1)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  # axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  # ##
  # polygon(c(DOY.date,rev(DOY.date)),c( QC678_mean1-2*QC678_sd1,
  #                                      rev( QC678_mean1+2*QC678_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  # par(lty=lt1)
  # lines(DOY.date, QC678_mean1,type="l",col=coul[6] )
  # points(leafbmdates,leafbiomass,col = 'red',pch = 16)
  # 
  # box()
  
  ### read obs c flux
  setwd(sowd)
  dat.obsflux<-read.table("obs_cflux.txt", header=T)

  DOYobs.date<-as.Date(dat.obsflux$days,origin='2010-12-31')

  # msGPP = data.frame(x = DOYobs.date, y = dat.obsflux$GPP, sd = dat.obsflux$GPP_sd)
  # msNEE = data.frame(x = DOYobs.date, y = dat.obsflux$NEE, sd = dat.obsflux$NEE_sd)
  # msReco = data.frame(x = DOYobs.date, y = dat.obsflux$Reco, sd = dat.obsflux$Rec_sd)
  msGPP = data.frame(x = DOYobs.date, y = dat.obsflux$GPP, sd = dat.obsflux$GPP*0.2)
  msNEE = data.frame(x = DOYobs.date, y = dat.obsflux$NEE, sd = dat.obsflux$NEE*0.2)
  msReco = data.frame(x = DOYobs.date, y = dat.obsflux$Reco, sd = dat.obsflux$Reco*0.2)
  
  #GPP
  plot(DOY.date,GPP_mean1,col=coul[6],axes = FALSE,type='l',xlab=NA,ylab='GPP (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(0,20.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)

  polygon(c(DOY.date,rev(DOY.date)),c( GPP_mean1-2*GPP_sd1,
                                       rev( GPP_mean1+2*GPP_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  par(lty=lt1)
  lines(DOY.date, GPP_mean1,type="l",col=coul[6] )
  
  points(msGPP$x,msGPP$y,col='red',lwd=5,main='GPP',xlab = 'Date',
         ylab='GPP (gC m-2 d-1)')
  with(data = msGPP, expr = errbar(x,y,y+sd,y-sd,add=T,pch=0,cap=.01))

  # # NEE
  plot(DOY.date,NEE_mean1,col=coul[6],axes = FALSE,type='l',xlab=NA,ylab='NEE (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(-20,20.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c( NEE_mean1-2*NEE_sd1,
                                       rev( NEE_mean1+2*NEE_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  par(lty=lt1)
  lines(DOY.date, NEE_mean1,type="l",col=coul[6] )
  
  points(msNEE$x,msNEE$y,col='red',lwd=5,main='NEE',xlab = 'Date',
         ylab='NEE (gC m-2 d-1)')
  with(data = msNEE, expr = errbar(x,y,y+sd,y-sd,add=T,pch=0,cap=.01))
  
  # #ER
  plot(DOY.date,Reco_mean1,col=coul[6],axes = FALSE,type='l',xlab=NA,ylab='Reco (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(0,20.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c( Reco_mean1-2*Reco_sd1,
                                       rev( Reco_mean1+2*Reco_sd1)),col=adjustcolor(coul[6],alpha.f = alphavalue),border=NA)
  par(lty=lt1)
  lines(DOY.date, Reco_mean1,type="l",col=coul[6] )
  
  points(msReco$x,msReco$y,col='red',lwd=5,main='Reco',xlab = 'Date',
         ylab='Reco (gC m-2 d-1)')
  with(data = msReco, expr = errbar(x,y,y+sd,y-sd,add=T,pch=0,cap=.01))
  
  # # Rh
  # plot(DOY.date,dat.carbonpf_CWE$Rh_d,col="black",main='Rh',xlab = 'Date',
  #      ylab='Rh (gC m-2 d-1)',cex.main=fontsize,cex.lab=fontsize,ylim = c(0,4.0),lwd=linewid,type='l')
  # 
  # # Ra
  # plot(DOY.date,dat.carbonpf_CWE$Ra_d,col="black",main='Ra',xlab = 'Date',
  #      ylab='Ra (gC m-2 d-1)',cex.main=fontsize,cex.lab=fontsize,ylim = c(0,20.0),lwd=linewid,type='l')
  # 
  # # NPP
  # plot(DOY.date,dat.carbonpf_CWE$NPP_d,col="black",main='NPP',xlab = 'Date',
  #      ylab='NPP (gC m-2 d-1)',cex.main=fontsize,cex.lab=fontsize,ylim = c(0,10.0),lwd=linewid,type='l')
