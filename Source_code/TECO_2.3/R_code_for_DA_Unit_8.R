rm(list=ls()) # clear memory
while (!is.null(dev.list()))  dev.off()

#  "run" equal to 1/2 ----------------------------------
# 0 for all data assimilation options
# 1 for data assimilation witout any measurement
# 2 for data assimilation with C pool measurements
# 3 for data assimilation with C fluxes measurements
# 4 for data assimilation with both C pool measurements and C fluxes measurements
# 4 for data assimilation with different measurements and model uncertainties

step = 0     # step for DA Unit 3

args = commandArgs(TRUE)

# read arguments from command line
step = args[1]                       # step
task_folder = args[2]                 # task folder
spruce_data_folder = args[3]          # spruce data folder

library(Metrics)
library(reshape)
library(ggplot2)
library(ggpubr)
library(stringr)

getwd()

# dir.create("plot")
# dir.create(paste0(task_folder, "/plot/DAUnit3")
# plot_folder = paste0(task_folder, "/plot/DAUnit3")

dateseq0 = seq(as.Date("2011-01-01"), as.Date("2016-12-31"), by = "day")
dateseq = dateseq0[dateseq0!="2012-02-29"&dateseq0!="2016-02-29"]
datelen = length(dateseq)

linewidth = 0.4
removen = 1000
tot_para = 50

paraname = c("simun","updatedn","SLA","GLmax","GRmax","Gsmax","Vcmx0", "Tau_Leaf","Tau_Wood","Tau_Root","Tau_F","Tau_C","Tau_Micro",
             "Tau_SlowSOM","Tau_Passive","gddonset","Q10","Rl0","Rs0","Rr0")
parn = length(paraname) - 2
sim0 = read.csv(paste0(task_folder, "/output/SPRUCE/Simu_dailyflux001.csv"))
sim0$Date = dateseq
names(sim0)
colnames(sim0)[c(2:5,7,9)]=c("GPP","NEE","Reco","Foliage","Wood","Root")
sim1 = melt(sim0[,c(2:5,7,9,14)], id = "Date")

cpool0 = read.csv(paste0(spruce_data_folder, "/input/SPRUCE_cpool.txt"), sep = "")
cpool0$Date = as.Date(cpool0$days, origin="2010-12-31")
names(cpool0)
cpool_mean = melt(cpool0[,c(3,7,11,19)],id = "Date")
cpool_sd = melt(cpool0[,c(4,8,12,19)],id = "Date")
cpool = data.frame(cpool_mean,cpool_sd$value)
colnames(cpool)[3:4] = c("cpool","cpool_sd")
cpool = cpool[cpool$cpool>0,]

cflux0 = read.csv(paste0(spruce_data_folder, "/input/SPRUCE_cflux.txt"), sep = "")
cflux0$Date = as.Date(cflux0$days, origin="2010-12-31")
names(cflux0)
cflux_mean = melt(cflux0[,c(3,5,7,9)],id = "Date")
cflux_sd = melt(cflux0[,c(4,6,8,9)],id = "Date")
cflux = data.frame(cflux_mean,cflux_sd$value)
colnames(cflux)[3:4] = c("cflux","cflux_sd")
cflux = cflux[cflux$cflux>0,]

  
  para<-read.table(paste0(task_folder, "/output/DA_nomeasure/Paraest.txt"),sep=",")
  paran = length(paraname); para = para[,-(paran+1)]; colnames(para)<-paraname; rown<-nrow(para)
  par_nomeas0 = para[-c(1:removen),]
  
  para<-read.table(paste0(task_folder, "/output/DA_cflux/Paraest.txt"),sep=",")
  paran = length(paraname); para = para[,-(paran+1)]; colnames(para)<-paraname; rown<-nrow(para)
  par_cflux0 = para[-c(1:removen),]
  
  para<-read.table(paste0(task_folder, "/output/DA_cpool/Paraest.txt"),sep=",")
  paran = length(paraname);   para = para[,-(paran+1)]; colnames(para)<-paraname; rown<-nrow(para)
  par_cpool0 = para[-c(1:removen),]
  
  para<-read.table(paste0(task_folder, "/output/DA_cpool_cflux/Paraest.txt"),sep=",")
  paran = length(paraname); para = para[,-(paran+1)]; colnames(para)<-paraname; rown<-nrow(para)
  par_cpool_cflux0 = para[-c(1:removen),]
  
  min1 = min(nrow(par_nomeas0),nrow(par_cflux0),nrow(par_cpool0),nrow(par_cpool_cflux0))
  par_nomeas2 = melt(par_nomeas0[1:min1,], id = c("simun","updatedn"))
  par_cpool2 = melt(par_cpool0[1:min1,], id = c("simun","updatedn"))
  par_cflux2 = melt(par_cflux0[1:min1,], id = c("simun","updatedn"))
  par_cpool_cflux2 = melt(par_cpool_cflux0[1:min1,], id = c("simun","updatedn"))
  
  par_nomeas2$Group = "No measurement";   par_cpool2$Group = "C pools"
  par_cflux2$Group = "C fluxes";   par_cpool_cflux2$Group = "C pools and fluxes"
  
  par_all = rbind(par_nomeas2,par_cpool2,par_cflux2,par_cpool_cflux2)
  binn = 200
  
  H0 = data.frame(Parameter=paraname[3:(parn+2)],H1 = NA, H2 = NA, H3 = NA, H4 = NA)
  groupname = c("No measurement","C pools","C fluxes","C pools and fluxes")
  groupn = length(groupname)
  
  for(c in 1:groupn) {
    # c = 2
    groupname1 = groupname[c]
  for(a in 1:parn) {

    # a = 1
    parametername = paraname[a+2]
    d1=subset(par_all, Group==groupname1&variable==parametername)
    
    n1 = nrow(d1)
    max1 = max(d1$value)
    min1 = min(d1$value)
    step1 = (max1-min1)/binn
    binv1 = seq(min1,max1,step1)
    
    h0 = 0
    for(b in 1:binn) {
      # b = 2
      d2 = subset(d1,value>=binv1[b]&value<binv1[b+1])
      p1 = nrow(d2)/n1
      if(p1 > 0) {
        h1 = p1 * log2(p1)
      } else {
        h1 = 0
      }
      h0 = h0 + h1
    }
    H0[a,c+1] = -h0
  
  }
  }
  
  I0 = data.frame(Parameter=paraname[3:(parn+2)],I1 = NA, I2 = NA, I3 = NA, I4 = NA)
  I0$I1 = log2(binn) - H0$H1
  I0$I2 = log2(binn) - H0$H2
  I0$I3 = log2(binn) - H0$H3
  I0$I4 = log2(binn) - H0$H4
  
  I0$Parameter = factor(I0$Parameter,levels = paraname[3:(parn+2)])
  I1 = melt(I0,id="Parameter")
  I1$Group = factor(I1$variable,levels = c("I1","I2","I3","I4"),labels = c("No measurement","C pools","C fluxes","C pools and fluxes"))
    
  p_d1 = ggplot() +
    geom_density(data=subset(par_all, Group=="No measurement"),aes(x=value, col = Group))+
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red", "C fluxes" = "green", "C pools and fluxes" = "blue"),
                       limits = c("No measurement")) +
    facet_wrap(~variable, scales = "free", ncol = 5) + theme_test() + theme(legend.position = c(0.8,0.1))
  p_I1 = ggplot() +
    geom_bar(data=subset(I1,Group=="No measurement"),aes(x=Parameter,y=value,col=Group),fill="white",stat="identity",position = position_dodge() ) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red", "C fluxes" = "green", "C pools and fluxes" = "blue"),
                       limits = c("No measurement")) +
    ylab("Information gain (bit)") + xlab(NULL) +
    theme_test(base_size = 12) + theme(axis.text.x = element_text(angle = 30, vjust = 0.5),legend.position = "none")
  if(step==0|step==1) {
    p1 = ggarrange(p_d1, p_I1,nrow = 2,ncol = 1,heights = c(2,1))
    # ggsave(paste0(task_folder, "/plot/DAUnit3/1 Density_no_measurement.tiff", p1,width = 16, height =16, units = "cm", scale = 1.5, dpi = 300)
    png(filename = paste0(task_folder, "/plot/DAUnit3/1 Density_no_measurement.png"), width = 20, height = 20, units = "cm", res=800,pointsize = 8)
      print(p1)
    dev.off()
  }
  
  p_d2 = ggplot() +
    geom_density(data=subset(par_all, Group=="No measurement"|Group=="C pools"),aes(x=value, col = Group))+
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red", "C fluxes" = "green", "C pools and fluxes" = "blue"),
                       limits = c("No measurement","C pools")) +
    facet_wrap(~variable, scales = "free", ncol = 5) + theme_test() + theme(legend.position = c(0.8,0.1))
  p_I2 = ggplot() +
    geom_bar(data=subset(I1,Group=="No measurement"|Group=="C pools"),aes(x=Parameter,y=value,col=Group),fill="white",stat="identity",position = position_dodge() ) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red", "C fluxes" = "green", "C pools and fluxes" = "blue"),
                       limits = c("No measurement","C pools")) +
    ylab("Information gain (bit)") + xlab(NULL) +
    theme_test(base_size = 12) + theme(axis.text.x = element_text(angle = 30, vjust = 0.5),legend.position = "none")
  if(step==0|step==2) {
    p2 = ggarrange(p_d2, p_I2,nrow = 2,ncol = 1,heights = c(2,1))
    # ggsave(paste0(task_folder, "/plot/DAUnit3/2 Density_C_pools.tiff", p2,width = 16, height =16, units = "cm", scale = 1.5, dpi = 300)
    png(filename = paste0(task_folder, "/plot/DAUnit3/2 Density_C_pools.png"), width = 20, height = 20, units = "cm", res=800,pointsize = 8)
      print(p2)
    dev.off()
  }
  
  p_d3 = ggplot() +
    geom_density(data=subset(par_all, Group=="No measurement"|Group=="C pools"|Group=="C fluxes"),aes(x=value, col = Group))+
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red", "C fluxes" = "green", "C pools and fluxes" = "blue"),
                       limits = c("No measurement","C pools","C fluxes")) +
    facet_wrap(~variable, scales = "free", ncol = 5) + theme_test() + theme(legend.position = c(0.8,0.1))
  p_I3 = ggplot() +
    geom_bar(data=subset(I1,Group=="No measurement"|Group=="C pools"|Group=="C fluxes"),aes(x=Parameter,y=value,col=Group),fill="white",stat="identity",position = position_dodge() ) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red", "C fluxes" = "green", "C pools and fluxes" = "blue"),
                       limits = c("No measurement","C pools","C fluxes")) +
    ylab("Information gain (bit)") + xlab(NULL) +
    theme_test(base_size = 12) + theme(axis.text.x = element_text(angle = 30, vjust = 0.5),legend.position = "none")
  if(step==0|step==3) {
    p3 = ggarrange(p_d3, p_I3,nrow = 2,ncol = 1,heights = c(2,1))
    # ggsave(paste0(task_folder, "/plot/DAUnit3/3 Density_C_fluxes.tiff", p3,width = 16, height =16, units = "cm", scale = 1.5, dpi = 300)
    png(filename = paste0(task_folder, "/plot/DAUnit3/3 Density_C_fluxes.png"), width = 20, height = 20, units = "cm", res=800,pointsize = 8)
      print(p3)
    dev.off()
  }
  
  p_d4 = ggplot() +
    geom_density(data=par_all,aes(x=value, col = Group))+
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red", "C fluxes" = "green", "C pools and fluxes" = "blue"),
                       limits = c("No measurement","C pools","C fluxes","C pools and fluxes")) +
    facet_wrap(~variable, scales = "free", ncol = 5) + theme_test() +
    theme(legend.position = c(0.8,0.1))
  p_I4 = ggplot() +
    geom_bar(data=I1,aes(x=Parameter,y=value,col=Group),fill="white",stat="identity",position = position_dodge() ) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red", "C fluxes" = "green", "C pools and fluxes" = "blue"),
                       limits = c("No measurement","C pools","C fluxes","C pools and fluxes")) +
    ylab("Information gain (bit)") + xlab(NULL) +
    theme_test(base_size = 12) + theme(axis.text.x = element_text(angle = 30, vjust = 0.5),legend.position = "none")
  if(step==0|step == 4) {
    p4 = ggarrange(p_d4, p_I4,nrow = 2,ncol = 1,heights = c(2,1))
    # ggsave(paste0(task_folder, "/plot/DAUnit3/4 Density_all.tiff", p4, width = 16, height =16, units = "cm", scale = 1.5, dpi = 300)
    png(filename = paste0(task_folder, "/plot/DAUnit3/4 Density_all.png"), width = 20, height = 20, units = "cm", res=800,pointsize = 8)
      print(p4)
    dev.off()
  }
  
  GPP_nomeas<-matrix(NA,datelen,tot_para); NEE_nomeas<-matrix(NA,datelen,tot_para); Reco_nomeas<-matrix(NA,datelen,tot_para)
  Foliage_nomeas<-matrix(NA,datelen,tot_para); Wood_nomeas<-matrix(NA,datelen,tot_para); Root_nomeas<-matrix(NA,datelen,tot_para)
  
  GPP_cflux<-matrix(NA,datelen,tot_para); NEE_cflux<-matrix(NA,datelen,tot_para); Reco_cflux<-matrix(NA,datelen,tot_para)
  Foliage_cflux<-matrix(NA,datelen,tot_para); Wood_cflux<-matrix(NA,datelen,tot_para); Root_cflux<-matrix(NA,datelen,tot_para)
  
  GPP_cpool<-matrix(NA,datelen,tot_para); NEE_cpool<-matrix(NA,datelen,tot_para); Reco_cpool<-matrix(NA,datelen,tot_para)
  Foliage_cpool<-matrix(NA,datelen,tot_para); Wood_cpool<-matrix(NA,datelen,tot_para); Root_cpool<-matrix(NA,datelen,tot_para)
  
  GPP_cpool_cflux<-matrix(NA,datelen,tot_para); NEE_cpool_cflux<-matrix(NA,datelen,tot_para); Reco_cpool_cflux<-matrix(NA,datelen,tot_para)
  Foliage_cpool_cflux<-matrix(NA,datelen,tot_para); Wood_cpool_cflux<-matrix(NA,datelen,tot_para); Root_cpool_cflux<-matrix(NA,datelen,tot_para)
  
  #### read 001-500 files in a for loop ####
  for (m in 1:tot_para) {
    
    # m = 1
    print(paste(m,"in",tot_para))
    openfile_0<-paste(paste0(task_folder, "/output/DA_nomeasure/Simu_dailyflux"),str_pad(m, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    d_0 = read.table(openfile_0,strip.white=TRUE,header=T,sep=",")
    names(d_0)
    GPP_nomeas[,m]<-d_0[,2]     
    NEE_nomeas[,m]<-d_0[,3]
    Reco_nomeas[,m]<-d_0[,4]
    Foliage_nomeas[,m]<-d_0[,5]     
    Wood_nomeas[,m]<-d_0[,7]
    Root_nomeas[,m]<-d_0[,9]
    
    openfile_0<-paste(paste0(task_folder, "/output/DA_cflux/Simu_dailyflux"),str_pad(m, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    d_0 = read.table(openfile_0,strip.white=TRUE,header=T,sep=",")
    names(d_0)
    GPP_cflux[,m]<-d_0[,2]     
    NEE_cflux[,m]<-d_0[,3]
    Reco_cflux[,m]<-d_0[,4]
    Foliage_cflux[,m]<-d_0[,5]     
    Wood_cflux[,m]<-d_0[,7]
    Root_cflux[,m]<-d_0[,9]
    
    
    openfile_0<-paste(paste0(task_folder, "/output/DA_cpool/Simu_dailyflux"),str_pad(m, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    d_0 = read.table(openfile_0,strip.white=TRUE,header=T,sep=",")
    names(d_0)
    GPP_cpool[,m]<-d_0[,2]     
    NEE_cpool[,m]<-d_0[,3]
    Reco_cpool[,m]<-d_0[,4]
    Foliage_cpool[,m]<-d_0[,5]     
    Wood_cpool[,m]<-d_0[,7]
    Root_cpool[,m]<-d_0[,9]
    
    
    openfile_0<-paste(paste0(task_folder, "/output/DA_cpool_cflux/Simu_dailyflux"),str_pad(m, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    d_0 = read.table(openfile_0,strip.white=TRUE,header=T,sep=",")
    names(d_0)
    GPP_cpool_cflux[,m]<-d_0[,2]     
    NEE_cpool_cflux[,m]<-d_0[,3]
    Reco_cpool_cflux[,m]<-d_0[,4]
    Foliage_cpool_cflux[,m]<-d_0[,5]     
    Wood_cpool_cflux[,m]<-d_0[,7]
    Root_cpool_cflux[,m]<-d_0[,9]
    
  }
  
  GPP_nomeas_quan = data.frame(Mean=rep(NA,datelen)); GPP_cflux_quan = data.frame(Mean=rep(NA,datelen))
  GPP_cpool_quan = data.frame(Mean=rep(NA,datelen)); GPP_cpool_cflux_quan = data.frame(Mean=rep(NA,datelen))
  
  GPP_nomeas_quan2<-as.data.frame(t(apply(GPP_nomeas,1,quantile))); GPP_nomeas_quan2$Date<-dateseq
  GPP_cpool_quan2<-as.data.frame(t(apply(GPP_cpool,1,quantile))); GPP_cpool_quan2$Date<-dateseq
  GPP_cflux_quan2<-as.data.frame(t(apply(GPP_cflux,1,quantile))); GPP_cflux_quan2$Date<-dateseq
  GPP_cpool_cflux_quan2<-as.data.frame(t(apply(GPP_cpool_cflux,1,quantile))); GPP_cpool_cflux_quan2$Date<-dateseq
  
  GPP_nomeas_quan$Mean<-rowMeans(GPP_nomeas, na.rm =TRUE, dims = 1); GPP_nomeas_quan$Date<-dateseq
  GPP_cflux_quan$Mean<-rowMeans(GPP_cflux, na.rm =TRUE, dims = 1);   GPP_cflux_quan$Date<-dateseq
  GPP_cpool_quan$Mean<-rowMeans(GPP_cpool, na.rm =TRUE, dims = 1);   GPP_cpool_quan$Date<-dateseq
  GPP_cpool_cflux_quan$Mean<-rowMeans(GPP_cpool_cflux, na.rm =TRUE, dims = 1);   GPP_cpool_cflux_quan$Date<-dateseq
  GPP_nomeas_quan$Group = "No measurement"; GPP_cflux_quan$Group = "C fluxes"
  GPP_cpool_quan$Group = "C pools"; GPP_cpool_cflux_quan$Group = "C pools and fluxes"
  GPP4 = rbind(GPP_nomeas_quan,GPP_cflux_quan,GPP_cpool_quan,GPP_cpool_cflux_quan)
  pointsize1 = 3
  
  rmsev = data.frame(Group=c("No measurement","C pools","C fluxes","C pools and fluxes"),
                     GPP=NA,Reco=NA,NEE=NA,Foliage=NA,Wood=NA,Root=NA)
  GPPC1 = merge(subset(cflux,variable=="GPP"),subset(GPP4,Group=="No measurement"),by="Date",all.x=TRUE)
  GPPC2 = merge(subset(cflux,variable=="GPP"),subset(GPP4,Group=="C pools"),by="Date",all.x=TRUE)
  GPPC3 = merge(subset(cflux,variable=="GPP"),subset(GPP4,Group=="C fluxes"),by="Date",all.x=TRUE)
  GPPC4 = merge(subset(cflux,variable=="GPP"),subset(GPP4,Group=="C pools and fluxes"),by="Date",all.x=TRUE)
  rmsev$GPP[1] = rmse(GPPC1$cflux,GPPC1$Mean); rmsev$GPP[2] = rmse(GPPC2$cflux,GPPC2$Mean)
  rmsev$GPP[3] = rmse(GPPC3$cflux,GPPC3$Mean); rmsev$GPP[4] = rmse(GPPC4$cflux,GPPC4$Mean)
  
  p_gpp1 = ggplot() +
    # geom_line(data=subset(sim1,variable=="GPP"),aes(Date,value),col="black", size=linewidth) +
    geom_ribbon(data=GPP_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80")+
    geom_ribbon(data=GPP_nomeas_quan2, aes(x=Date, ymin=`25%`, ymax=`75%`),fill="gray50")+
    geom_line(data=subset(GPP4,Group=="No measurement"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement")) +
    geom_point(data=subset(cflux,variable=="GPP"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "gray50" ) +
    geom_errorbar(data=subset(cflux,variable=="GPP"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("GPP (g m"^-2, " d"^-1, ")",sep = "")))+theme_test()+xlab(NULL)+theme(legend.position = "top",legend.title = element_blank(),legend.text=element_text(size=11)) +
    annotate("text",x=as.Date("2011-07-01"),y=35,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[1],1)))),parse=T,col="gray50")
  
  p_gpp2 = ggplot() + 
    geom_line(data=subset(GPP4,Group=="No measurement"|Group=="C pools"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools")) +
    geom_point(data=subset(cflux,variable=="GPP"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "gray50" ) +
    geom_errorbar(data=subset(cflux,variable=="GPP"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("GPP (g m"^-2, " d"^-1, ")",sep = "")))+theme_test()+xlab(NULL)+theme(legend.position = "top",legend.title = element_blank(),legend.text=element_text(size=11)) +
    annotate("text",x=as.Date("2011-07-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[2],1)))),parse=T,col="red")
  
  p_gpp2_2 = ggplot() +
    geom_ribbon(data=GPP_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80",alpha=0.5)+
    geom_ribbon(data=GPP_cpool_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="red",alpha=0.5)+
    geom_line(data=subset(GPP4,Group=="No measurement"|Group=="C pools"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools")) +
    geom_point(data=subset(cflux,variable=="GPP"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "gray50" ) +
    geom_errorbar(data=subset(cflux,variable=="GPP"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("GPP (g m"^-2, " d"^-1, ")",sep = "")))+theme_test()+xlab(NULL)+theme(legend.position = "top",legend.title = element_blank(),legend.text=element_text(size=11)) +
    annotate("text",x=as.Date("2011-07-01"),y=40,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=40,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[2],1)))),parse=T,col="red") +
    annotate("text",x=as.Date("2011-07-01"),y=35,label=deparse(bquote("Range = "~.(round(mean(GPP_nomeas_quan2$`100%`-GPP_nomeas_quan2$`0%`),1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=35,label=deparse(bquote("Range = "~.(round(mean(GPP_cpool_quan2$`100%`-GPP_cpool_quan2$`0%`),1)))),parse=T,col="red")
  
  p_gpp3 = ggplot() + 
    geom_line(data=subset(GPP4,Group=="No measurement"|Group=="C pools"|Group=="C fluxes"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes")) +
    geom_point(data=subset(cflux,variable=="GPP"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cflux,variable=="GPP"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("GPP (g m"^-2, " d"^-1, ")",sep = "")))+theme_test()+xlab(NULL)+theme(legend.position = "top",legend.title = element_blank(),legend.text=element_text(size=11)) +
    annotate("text",x=as.Date("2011-07-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[3],1)))),parse=T,col="green")
  
  p_gpp3_2 = ggplot() +
    geom_ribbon(data=GPP_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80",alpha=0.5)+
    geom_ribbon(data=GPP_cflux_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="green",alpha=0.5)+
    geom_line(data=subset(GPP4,Group=="No measurement"|Group=="C fluxes"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C fluxes")) +
    geom_point(data=subset(cflux,variable=="GPP"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cflux,variable=="GPP"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("GPP (g m"^-2, " d"^-1, ")",sep = "")))+theme_test()+xlab(NULL)+theme(legend.position = "top",legend.title = element_blank(),legend.text=element_text(size=11)) +
    annotate("text",x=as.Date("2011-07-01"),y=40,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=40,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[3],1)))),parse=T,col="green") +
    annotate("text",x=as.Date("2011-07-01"),y=35,label=deparse(bquote("Range = "~.(round(mean(GPP_nomeas_quan2$`100%`-GPP_nomeas_quan2$`0%`),1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=35,label=deparse(bquote("Range = "~.(round(mean(GPP_cflux_quan2$`100%`-GPP_cflux_quan2$`0%`),1)))),parse=T,col="green")
  
  p_gpp4 = ggplot() + 
    geom_line(data=GPP4,aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes","C pools and fluxes")) +
    geom_point(data=subset(cflux,variable=="GPP"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cflux,variable=="GPP"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("GPP (g m"^-2, " d"^-1, ")",sep = "")))+theme_test()+xlab(NULL)+theme(legend.position = "top",legend.title = element_blank(),legend.text=element_text(size=11)) +
    annotate("text",x=as.Date("2011-07-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[3],1)))),parse=T,col="green")+
    annotate("text",x=as.Date("2016-01-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[4],1)))),parse=T,col="blue")
  
  p_gpp4_2 = ggplot() +
    geom_ribbon(data=GPP_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80",alpha=0.5)+
    geom_ribbon(data=GPP_cpool_cflux_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="blue",alpha=0.5)+
    geom_line(data=subset(GPP4,Group=="No measurement"|Group=="C pools and fluxes"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools and fluxes")) +
    geom_point(data=subset(cflux,variable=="GPP"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cflux,variable=="GPP"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("GPP (g m"^-2, " d"^-1, ")",sep = "")))+theme_test()+xlab(NULL)+theme(legend.position = "top",legend.title = element_blank(),legend.text=element_text(size=11)) +
    annotate("text",x=as.Date("2011-07-01"),y=40,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=40,label=deparse(bquote("RMSE = "~.(round(rmsev$GPP[4],1)))),parse=T,col="blue") +
    annotate("text",x=as.Date("2011-07-01"),y=35,label=deparse(bquote("Range = "~.(round(mean(GPP_nomeas_quan2$`100%`-GPP_nomeas_quan2$`0%`),1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=35,label=deparse(bquote("Range = "~.(round(mean(GPP_cpool_cflux_quan2$`100%`-GPP_cpool_cflux_quan2$`0%`),1)))),parse=T,col="blue")
  
  NEE_nomeas_quan = data.frame(Mean=rep(NA,datelen)); NEE_cflux_quan = data.frame(Mean=rep(NA,datelen))
  NEE_cpool_quan = data.frame(Mean=rep(NA,datelen)); NEE_cpool_cflux_quan = data.frame(Mean=rep(NA,datelen))
  
  NEE_nomeas_quan2<-as.data.frame(t(apply(NEE_nomeas,1,quantile))); NEE_nomeas_quan2$Date<-dateseq
  
  NEE_nomeas_quan$Mean<-rowMeans(NEE_nomeas, na.rm =TRUE, dims = 1); NEE_nomeas_quan$Date<-dateseq
  NEE_cflux_quan$Mean<-rowMeans(NEE_cflux, na.rm =TRUE, dims = 1); NEE_cflux_quan$Date<-dateseq
  NEE_cpool_quan$Mean<-rowMeans(NEE_cpool, na.rm =TRUE, dims = 1); NEE_cpool_quan$Date<-dateseq
  NEE_cpool_cflux_quan$Mean<-rowMeans(NEE_cpool_cflux, na.rm =TRUE, dims = 1); NEE_cpool_cflux_quan$Date<-dateseq
  NEE_nomeas_quan$Group = "No measurement"; NEE_cflux_quan$Group = "C fluxes"
  NEE_cpool_quan$Group = "C pools"; NEE_cpool_cflux_quan$Group = "C pools and fluxes"
  NEE4 = rbind(NEE_nomeas_quan,NEE_cflux_quan,NEE_cpool_quan,NEE_cpool_cflux_quan)
  
  NEEC1 = merge(subset(cflux,variable=="NEE"),subset(NEE4,Group=="No measurement"),by="Date",all.x=TRUE)
  NEEC2 = merge(subset(cflux,variable=="NEE"),subset(NEE4,Group=="C pools"),by="Date",all.x=TRUE)
  NEEC3 = merge(subset(cflux,variable=="NEE"),subset(NEE4,Group=="C fluxes"),by="Date",all.x=TRUE)
  NEEC4 = merge(subset(cflux,variable=="NEE"),subset(NEE4,Group=="C pools and fluxes"),by="Date",all.x=TRUE)
  rmsev$NEE[1] = rmse(NEEC1$cflux,NEEC1$Mean); rmsev$NEE[2] = rmse(NEEC2$cflux,NEEC2$Mean)
  rmsev$NEE[3] = rmse(NEEC3$cflux,NEEC3$Mean); rmsev$NEE[4] = rmse(NEEC4$cflux,NEEC4$Mean)
  
  p_nee1 = ggplot() +
    # geom_line(data=subset(sim1,variable=="NEE"),aes(Date,value),col="gray70", size=linewidth) +
    geom_ribbon(data=NEE_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80")+
    geom_ribbon(data=NEE_nomeas_quan2, aes(x=Date, ymin=`25%`, ymax=`75%`),fill="gray50")+
    geom_line(data=subset(NEE4,Group=="No measurement"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement")) +
    geom_point(data=subset(cflux,variable=="NEE"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "gray50") +
    geom_errorbar(data=subset(cflux,variable=="NEE"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("NEE (g m"^-2, " d"^-1, ")",sep = ""))) + theme_test() +  xlab(NULL) + theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[1],1)))),parse=T,col="gray50") 
  
  p_nee2 = ggplot() +
    geom_line(data=subset(NEE4,Group=="No measurement"|Group=="C pools"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools")) +
    geom_point(data=subset(cflux,variable=="NEE"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "gray50" ) +
    geom_errorbar(data=subset(cflux,variable=="NEE"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("NEE (g m"^-2, " d"^-1, ")",sep = ""))) + theme_test() +  xlab(NULL) + theme(legend.position = "none",legend.title = element_blank()) +
    annotate("text",x=as.Date("2011-07-01"),y=12,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=12,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[2],1)))),parse=T,col="red")
  
  p_nee3 = ggplot() +
    geom_line(data=subset(NEE4,Group=="No measurement"|Group=="C pools"|Group=="C fluxes"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes")) +
    geom_point(data=subset(cflux,variable=="NEE"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cflux,variable=="NEE"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("NEE (g m"^-2, " d"^-1, ")",sep = ""))) + theme_test() +  xlab(NULL) + theme(legend.position = "none",legend.title = element_blank()) +
    annotate("text",x=as.Date("2011-07-01"),y=12,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=12,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=12,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[3],1)))),parse=T,col="green")
  
  p_nee4 = ggplot() +
    geom_line(data=NEE4,aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes", "C pools and fluxes")) +
    geom_point(data=subset(cflux,variable=="NEE"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cflux,variable=="NEE"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("NEE (g m"^-2, " d"^-1, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank()) +
    annotate("text",x=as.Date("2011-07-01"),y=12,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=12,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=12,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[3],1)))),parse=T,col="green")+
    annotate("text",x=as.Date("2016-01-01"),y=12,label=deparse(bquote("RMSE = "~.(round(rmsev$NEE[4],1)))),parse=T,col="blue")
  
  
  Reco_nomeas_quan = data.frame(Mean=rep(NA,datelen)); Reco_cflux_quan = data.frame(Mean=rep(NA,datelen))
  Reco_cpool_quan = data.frame(Mean=rep(NA,datelen));  Reco_cpool_cflux_quan = data.frame(Mean=rep(NA,datelen))
  
  Reco_nomeas_quan2<-as.data.frame(t(apply(Reco_nomeas,1,quantile))); Reco_nomeas_quan2$Date<-dateseq
  
  Reco_nomeas_quan$Mean<-rowMeans(Reco_nomeas, na.rm =TRUE, dims = 1); Reco_nomeas_quan$Date<-dateseq
  Reco_cflux_quan$Mean<-rowMeans(Reco_cflux, na.rm =TRUE, dims = 1);  Reco_cflux_quan$Date<-dateseq
  Reco_cpool_quan$Mean<-rowMeans(Reco_cpool, na.rm =TRUE, dims = 1);  Reco_cpool_quan$Date<-dateseq
  Reco_cpool_cflux_quan$Mean<-rowMeans(Reco_cpool_cflux, na.rm =TRUE, dims = 1); Reco_cpool_cflux_quan$Date<-dateseq
  Reco_nomeas_quan$Group = "No measurement";  Reco_cflux_quan$Group = "C fluxes"
  Reco_cpool_quan$Group = "C pools"; Reco_cpool_cflux_quan$Group = "C pools and fluxes"
  Reco4 = rbind(Reco_nomeas_quan,Reco_cflux_quan,Reco_cpool_quan,Reco_cpool_cflux_quan)
  
  RecoC1 = merge(subset(cflux,variable=="Reco"),subset(Reco4,Group=="No measurement"),by="Date",all.x=TRUE)
  RecoC2 = merge(subset(cflux,variable=="Reco"),subset(Reco4,Group=="C pools"),by="Date",all.x=TRUE)
  RecoC3 = merge(subset(cflux,variable=="Reco"),subset(Reco4,Group=="C fluxes"),by="Date",all.x=TRUE)
  RecoC4 = merge(subset(cflux,variable=="Reco"),subset(Reco4,Group=="C pools and fluxes"),by="Date",all.x=TRUE)
  rmsev$Reco[1] = rmse(RecoC1$cflux,RecoC1$Mean); rmsev$Reco[2] = rmse(RecoC2$cflux,RecoC2$Mean)
  rmsev$Reco[3] = rmse(RecoC3$cflux,RecoC3$Mean); rmsev$Reco[4] = rmse(RecoC4$cflux,RecoC4$Mean)
  
  p_reco1 = ggplot() + 
    geom_ribbon(data=Reco_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80")+
    geom_ribbon(data=Reco_nomeas_quan2, aes(x=Date, ymin=`25%`, ymax=`75%`),fill="gray50")+
    geom_line(data=subset(Reco4,Group=="No measurement"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement")) +
    geom_point(data=subset(cflux,variable=="Reco"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "gray50") +
    geom_errorbar(data=subset(cflux,variable=="Reco"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("Reco (g m"^-2, " d"^-1, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank()) +
    annotate("text",x=as.Date("2011-07-01"),y=25,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[1],1)))),parse=T,col="gray50") 
  
  p_reco2 = ggplot() + 
    geom_line(data=subset(Reco4,Group=="No measurement"|Group=="C pools"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools")) +
    geom_point(data=subset(cflux,variable=="Reco"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "gray50" ) +
    geom_errorbar(data=subset(cflux,variable=="Reco"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("Reco (g m"^-2, " d"^-1, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank()) +
    annotate("text",x=as.Date("2011-07-01"),y=15,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=15,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[2],1)))),parse=T,col="red")
  
  p_reco3 = ggplot() + 
    geom_line(data=subset(Reco4,Group=="No measurement"|Group=="C pools"|Group=="C fluxes"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes")) +
    geom_point(data=subset(cflux,variable=="Reco"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cflux,variable=="Reco"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("Reco (g m"^-2, " d"^-1, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank()) +
    annotate("text",x=as.Date("2011-07-01"),y=15,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=15,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=15,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[3],1)))),parse=T,col="green")
  
  p_reco4 = ggplot() + 
    geom_line(data=Reco4,aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes", "C pools and fluxes")) +
    geom_point(data=subset(cflux,variable=="Reco"),aes(Date,cflux),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cflux,variable=="Reco"),aes(x=Date,y=cflux,ymin=cflux-cflux_sd,ymax=cflux+cflux_sd),col="black", width = 1) +
    ylab(expression(paste("Reco (g m"^-2, " d"^-1, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank()) +
    annotate("text",x=as.Date("2011-07-01"),y=15,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=15,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=15,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[3],1)))),parse=T,col="green")+
    annotate("text",x=as.Date("2016-01-01"),y=15,label=deparse(bquote("RMSE = "~.(round(rmsev$Reco[4],1)))),parse=T,col="blue")
  
  
  Foliage_nomeas_quan = data.frame(Mean=rep(NA,datelen)); Foliage_cflux_quan = data.frame(Mean=rep(NA,datelen))
  Foliage_cpool_quan = data.frame(Mean=rep(NA,datelen));  Foliage_cpool_cflux_quan = data.frame(Mean=rep(NA,datelen))

  Foliage_nomeas_quan2<-as.data.frame(t(apply(Foliage_nomeas,1,quantile))); Foliage_nomeas_quan2$Date<-dateseq
  Foliage_cpool_quan2<-as.data.frame(t(apply(Foliage_cpool,1,quantile))); Foliage_cpool_quan2$Date<-dateseq
  Foliage_cflux_quan2<-as.data.frame(t(apply(Foliage_cflux,1,quantile))); Foliage_cflux_quan2$Date<-dateseq
  Foliage_cpool_cflux_quan2<-as.data.frame(t(apply(Foliage_cpool_cflux,1,quantile))); Foliage_cpool_cflux_quan2$Date<-dateseq
  
  Foliage_nomeas_quan$Mean<-rowMeans(Foliage_nomeas, na.rm =TRUE, dims = 1);   Foliage_nomeas_quan$Date<-dateseq
  Foliage_cflux_quan$Mean<-rowMeans(Foliage_cflux, na.rm =TRUE, dims = 1);   Foliage_cflux_quan$Date<-dateseq
  Foliage_cpool_quan$Mean<-rowMeans(Foliage_cpool, na.rm =TRUE, dims = 1);   Foliage_cpool_quan$Date<-dateseq
  Foliage_cpool_cflux_quan$Mean<-rowMeans(Foliage_cpool_cflux, na.rm =TRUE, dims = 1);   Foliage_cpool_cflux_quan$Date<-dateseq
  Foliage_nomeas_quan$Group = "No measurement";   Foliage_cflux_quan$Group = "C fluxes"
  Foliage_cpool_quan$Group = "C pools";  Foliage_cpool_cflux_quan$Group = "C pools and fluxes"
  Foliage4 = rbind(Foliage_nomeas_quan,Foliage_cflux_quan,Foliage_cpool_quan,Foliage_cpool_cflux_quan)
  
  FoliageC1 = merge(subset(cpool,variable=="Foliage"),subset(Foliage4,Group=="No measurement"),by="Date",all.x=TRUE)
  FoliageC2 = merge(subset(cpool,variable=="Foliage"),subset(Foliage4,Group=="C pools"),by="Date",all.x=TRUE)
  FoliageC3 = merge(subset(cpool,variable=="Foliage"),subset(Foliage4,Group=="C fluxes"),by="Date",all.x=TRUE)
  FoliageC4 = merge(subset(cpool,variable=="Foliage"),subset(Foliage4,Group=="C pools and fluxes"),by="Date",all.x=TRUE)
  rmsev$Foliage[1] = rmse(FoliageC1$cpool,FoliageC1$Mean); rmsev$Foliage[2] = rmse(FoliageC2$cpool,FoliageC2$Mean)
  rmsev$Foliage[3] = rmse(FoliageC3$cpool,FoliageC3$Mean); rmsev$Foliage[4] = rmse(FoliageC4$cpool,FoliageC4$Mean)
  
  p_foliage1 = ggplot() + 
    geom_ribbon(data=Foliage_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80",alpha=0.5)+
    geom_ribbon(data=Foliage_nomeas_quan2, aes(x=Date, ymin=`25%`, ymax=`75%`),fill="gray50",alpha=0.5)+
    geom_line(data=subset(Foliage4,Group=="No measurement"),aes(Date,Mean,col=Group),size=linewidth) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement")) +
    geom_point(data=subset(cpool,variable=="Foliage"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "gray50") +
    geom_errorbar(data=subset(cpool,variable=="Foliage"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Foliage (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) +  theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[1],1)))),parse=T,col="gray50") 
  
  p_foliage2 = ggplot() + 
    geom_line(data=subset(Foliage4,Group=="No measurement"|Group=="C pools"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools")) +
    geom_point(data=subset(cpool,variable=="Foliage"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Foliage"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Foliage (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) +  theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[2],1)))),parse=T,col="red")
  
  p_foliage2_2 = ggplot() +
    geom_ribbon(data=Foliage_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80",alpha=0.5)+
    geom_ribbon(data=Foliage_cpool_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="red",alpha=0.5)+
    geom_line(data=subset(Foliage4,Group=="No measurement"|Group=="C pools"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools")) +
    geom_point(data=subset(cpool,variable=="Foliage"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Foliage"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Foliage (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) +  theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2011-07-01"),y=650,label=deparse(bquote("Range = "~.(round(mean(Foliage_nomeas_quan2$`100%`-Foliage_nomeas_quan2$`0%`),1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=650,label=deparse(bquote("Range = "~.(round(mean(Foliage_cpool_quan2$`100%`-Foliage_cpool_quan2$`0%`),1)))),parse=T,col="red")
  
  p_foliage2_2 = ggplot() +
    geom_ribbon(data=Foliage_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80",alpha=0.5)+
    geom_ribbon(data=Foliage_cpool_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="red",alpha=0.5)+
    geom_line(data=subset(Foliage4,Group=="No measurement"|Group=="C pools"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools")) +
    geom_point(data=subset(cpool,variable=="Foliage"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Foliage"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Foliage (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) +  theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2011-07-01"),y=650,label=deparse(bquote("Range = "~.(round(mean(Foliage_nomeas_quan2$`100%`-Foliage_nomeas_quan2$`0%`),1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=650,label=deparse(bquote("Range = "~.(round(mean(Foliage_cpool_quan2$`100%`-Foliage_cpool_quan2$`0%`),1)))),parse=T,col="red")
  
  
  p_foliage3 = ggplot() + 
    geom_line(data=subset(Foliage4,Group=="No measurement"|Group=="C pools"|Group=="C fluxes"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools", "C fluxes")) +
    geom_point(data=subset(cpool,variable=="Foliage"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Foliage"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Foliage (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) +  theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[1],1)))),parse=T,col="black")+
    annotate("text",x=as.Date("2013-01-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[3],1)))),parse=T,col="green")
  
  p_foliage3_2 = ggplot() +
    geom_ribbon(data=Foliage_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80",alpha=0.5)+
    geom_ribbon(data=Foliage_cflux_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="green",alpha=0.5)+
    geom_line(data=subset(Foliage4,Group=="No measurement"|Group=="C fluxes"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C fluxes")) +
    geom_point(data=subset(cpool,variable=="Foliage"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "gray50" ) +
    geom_errorbar(data=subset(cpool,variable=="Foliage"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Foliage (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) +  theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=820,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=820,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[3],1)))),parse=T,col="green")+
    annotate("text",x=as.Date("2011-07-01"),y=750,label=deparse(bquote("Range = "~.(round(mean(Foliage_nomeas_quan2$`100%`-Foliage_nomeas_quan2$`0%`),1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=750,label=deparse(bquote("Range = "~.(round(mean(Foliage_cflux_quan2$`100%`-Foliage_cflux_quan2$`0%`),1)))),parse=T,col="green")
  
  p_foliage4 = ggplot() + 
    geom_line(data=Foliage4,aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools", "C fluxes","C pools and fluxes")) +
    geom_point(data=subset(cpool,variable=="Foliage"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Foliage"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Foliage (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) +  theme(legend.position = "none",legend.title = element_blank()) +
    annotate("text",x=as.Date("2011-07-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[3],1)))),parse=T,col="green")+
    annotate("text",x=as.Date("2016-01-01"),y=650,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[4],1)))),parse=T,col="blue")
  
  p_foliage4_2 = ggplot() +
    geom_ribbon(data=Foliage_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80",alpha=0.5)+
    geom_ribbon(data=Foliage_cpool_cflux_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="blue",alpha=0.5)+
    geom_line(data=subset(Foliage4,Group=="No measurement"|Group=="C pools and fluxes"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools and fluxes")) +
    geom_point(data=subset(cpool,variable=="Foliage"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Foliage"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Foliage (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) +  theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=820,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=820,label=deparse(bquote("RMSE = "~.(round(rmsev$Foliage[4],1)))),parse=T,col="blue")+
    annotate("text",x=as.Date("2011-07-01"),y=750,label=deparse(bquote("Range = "~.(round(mean(Foliage_nomeas_quan2$`100%`-Foliage_nomeas_quan2$`0%`),1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=750,label=deparse(bquote("Range = "~.(round(mean(Foliage_cpool_cflux_quan2$`100%`-Foliage_cpool_cflux_quan2$`0%`),1)))),parse=T,col="blue")
  
  
  Wood_nomeas_quan = data.frame(Mean=rep(NA,datelen)); Wood_cflux_quan = data.frame(Mean=rep(NA,datelen))
  Wood_cpool_quan = data.frame(Mean=rep(NA,datelen)); Wood_cpool_cflux_quan = data.frame(Mean=rep(NA,datelen))
  
  Wood_nomeas_quan2<-as.data.frame(t(apply(Wood_nomeas,1,quantile))); Wood_nomeas_quan2$Date<-dateseq
  
  Wood_nomeas_quan$Mean<-rowMeans(Wood_nomeas, na.rm =TRUE, dims = 1);  Wood_nomeas_quan$Date<-dateseq
  Wood_cflux_quan$Mean<-rowMeans(Wood_cflux, na.rm =TRUE, dims = 1);  Wood_cflux_quan$Date<-dateseq
  Wood_cpool_quan$Mean<-rowMeans(Wood_cpool, na.rm =TRUE, dims = 1); Wood_cpool_quan$Date<-dateseq
  Wood_cpool_cflux_quan$Mean<-rowMeans(Wood_cpool_cflux, na.rm =TRUE, dims = 1); Wood_cpool_cflux_quan$Date<-dateseq
  Wood_nomeas_quan$Group = "No measurement"; Wood_cflux_quan$Group = "C fluxes"
  Wood_cpool_quan$Group = "C pools"; Wood_cpool_cflux_quan$Group = "C pools and fluxes"
  Wood4 = rbind(Wood_nomeas_quan,Wood_cflux_quan,Wood_cpool_quan,Wood_cpool_cflux_quan)
  
  WoodC1 = merge(subset(cpool,variable=="Wood"),subset(Wood4,Group=="No measurement"),by="Date",all.x=TRUE)
  WoodC2 = merge(subset(cpool,variable=="Wood"),subset(Wood4,Group=="C pools"),by="Date",all.x=TRUE)
  WoodC3 = merge(subset(cpool,variable=="Wood"),subset(Wood4,Group=="C fluxes"),by="Date",all.x=TRUE)
  WoodC4 = merge(subset(cpool,variable=="Wood"),subset(Wood4,Group=="C pools and fluxes"),by="Date",all.x=TRUE)
  rmsev$Wood[1] = rmse(WoodC1$cpool,WoodC1$Mean); rmsev$Wood[2] = rmse(WoodC2$cpool,WoodC2$Mean)
  rmsev$Wood[3] = rmse(WoodC3$cpool,WoodC3$Mean); rmsev$Wood[4] = rmse(WoodC4$cpool,WoodC4$Mean)
  
  p_wood1 = ggplot() + 
    # geom_line(data=subset(sim1,variable=="Wood"),aes(Date,value),col="gray70") +
    geom_ribbon(data=Wood_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80")+
    geom_ribbon(data=Wood_nomeas_quan2, aes(x=Date, ymin=`25%`, ymax=`75%`),fill="gray50")+
    geom_line(data=subset(Wood4,Group=="No measurement"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement")) +
    geom_point(data=subset(cpool,variable=="Wood"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "gray50") +
    geom_errorbar(data=subset(cpool,variable=="Wood"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Wood (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=1300,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[1],1)))),parse=T,col="gray50") 
  
  p_wood2 = ggplot() + 
    geom_line(data=subset(Wood4,Group=="No measurement"|Group=="C pools"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement", "C pools")) +
    geom_point(data=subset(cpool,variable=="Wood"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Wood"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Wood (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[2],1)))),parse=T,col="red")
  
  p_wood3 = ggplot() + 
    geom_line(data=subset(Wood4,Group=="No measurement"|Group=="C pools"|Group=="C fluxes"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes")) +
    geom_point(data=subset(cpool,variable=="Wood"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Wood"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Wood (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[3],1)))),parse=T,col="green")
  
  p_wood4 = ggplot() + 
    geom_line(data=Wood4,aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes","C pools and fluxes")) +
    geom_point(data=subset(cpool,variable=="Wood"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Wood"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Wood (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[3],1)))),parse=T,col="green")+
    annotate("text",x=as.Date("2016-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Wood[4],1)))),parse=T,col="blue")
  
  
  Root_nomeas_quan = data.frame(Mean=rep(NA,datelen)); Root_cflux_quan = data.frame(Mean=rep(NA,datelen))
  Root_cpool_quan = data.frame(Mean=rep(NA,datelen)); Root_cpool_cflux_quan = data.frame(Mean=rep(NA,datelen))
  
  Root_nomeas_quan2<-as.data.frame(t(apply(Root_nomeas,1,quantile))); Root_nomeas_quan2$Date<-dateseq
  
  Root_nomeas_quan$Mean<-rowMeans(Root_nomeas, na.rm =TRUE, dims = 1); Root_nomeas_quan$Date<-dateseq
  Root_cflux_quan$Mean<-rowMeans(Root_cflux, na.rm =TRUE, dims = 1); Root_cflux_quan$Date<-dateseq
  Root_cpool_quan$Mean<-rowMeans(Root_cpool, na.rm =TRUE, dims = 1); Root_cpool_quan$Date<-dateseq
  Root_cpool_cflux_quan$Mean<-rowMeans(Root_cpool_cflux, na.rm =TRUE, dims = 1); Root_cpool_cflux_quan$Date<-dateseq
  Root_nomeas_quan$Group = "No measurement"; Root_cflux_quan$Group = "C fluxes"
  Root_cpool_quan$Group = "C pools"; Root_cpool_cflux_quan$Group = "C pools and fluxes"
  Root4 = rbind(Root_nomeas_quan,Root_cflux_quan,Root_cpool_quan,Root_cpool_cflux_quan)
  
  RootC1 = merge(subset(cpool,variable=="Root"),subset(Root4,Group=="No measurement"),by="Date",all.x=TRUE)
  RootC2 = merge(subset(cpool,variable=="Root"),subset(Root4,Group=="C pools"),by="Date",all.x=TRUE)
  RootC3 = merge(subset(cpool,variable=="Root"),subset(Root4,Group=="C fluxes"),by="Date",all.x=TRUE)
  RootC4 = merge(subset(cpool,variable=="Root"),subset(Root4,Group=="C pools and fluxes"),by="Date",all.x=TRUE)
  rmsev$Root[1] = rmse(RootC1$cpool,RootC1$Mean); rmsev$Root[2] = rmse(RootC2$cpool,RootC2$Mean)
  rmsev$Root[3] = rmse(RootC3$cpool,RootC3$Mean); rmsev$Root[4] = rmse(RootC4$cpool,RootC4$Mean)
  
  p_root1 =   ggplot() + 
    geom_ribbon(data=Root_nomeas_quan2, aes(x=Date, ymin=`0%`, ymax=`100%`),fill="gray80")+
    geom_ribbon(data=Root_nomeas_quan2, aes(x=Date, ymin=`25%`, ymax=`75%`),fill="gray50")+
    geom_line(data=subset(Root4,Group=="No measurement"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement")) +
    geom_point(data=subset(cpool,variable=="Root"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "gray50") +
    geom_errorbar(data=subset(cpool,variable=="Root"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Root (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[1],1)))),parse=T,col="gray50") 

  p_root2 =   ggplot() + 
    geom_line(data=subset(Root4,Group=="No measurement"|Group=="C pools"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools")) +
    geom_point(data=subset(cpool,variable=="Root"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Root"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Root (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[2],1)))),parse=T,col="red")
  
  p_root3 =   ggplot() + 
    geom_line(data=subset(Root4,Group=="No measurement"|Group=="C pools"|Group=="C fluxes"),aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes")) +
    geom_point(data=subset(cpool,variable=="Root"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Root"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Root (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[3],1)))),parse=T,col="green")
  
  p_root4 =   ggplot() + 
    geom_line(data=Root4,aes(Date,Mean,col=Group)) +
    scale_color_manual(values = c("No measurement" = "black","C pools" = "red","C fluxes" = "green","C pools and fluxes"="blue"),
                       limits = c("No measurement","C pools","C fluxes","C pools and fluxes")) +
    geom_point(data=subset(cpool,variable=="Root"),aes(Date,cpool),color="black",size=pointsize1, shape=21, fill = "black" ) +
    geom_errorbar(data=subset(cpool,variable=="Root"),aes(x=Date,y=cpool,ymin=cpool-cpool_sd,ymax=cpool+cpool_sd),col="black", width = 1) +
    ylab(expression(paste("Root (g m"^-2, ")",sep = ""))) + theme_test() + xlab(NULL) + theme(legend.position = "none",legend.title = element_blank())+
    annotate("text",x=as.Date("2011-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[1],1)))),parse=T,col="gray50")+
    annotate("text",x=as.Date("2013-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[2],1)))),parse=T,col="red")+
    annotate("text",x=as.Date("2014-07-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[3],1)))),parse=T,col="green")+
    annotate("text",x=as.Date("2016-01-01"),y=700,label=deparse(bquote("RMSE = "~.(round(rmsev$Root[4],1)))),parse=T,col="blue")
  
  if(step==0|step==1) {
    p_comb1 = ggarrange(p_gpp1,p_reco1,p_nee1,p_foliage1,p_wood1,p_root1, ncol = 1, heights = c(1.2,1,1,1,1,1))
    # ggsave(paste0(task_folder, "/plot/DAUnit3/1 Simulation_no_measurement.tiff", p_comb1,width = 15, height = 24, units = "cm", scale = 1, dpi = 300)
    png(filename = paste0(task_folder, "/plot/DAUnit3/1 Simulation_no_measurement.png"), width = 20, height = 32, units = "cm", res=800,pointsize = 10)
      print(p_comb1)
    dev.off()
  }
  
  if(step==0|step==2) {
    p_comb2 = ggarrange(p_gpp2,p_reco2,p_nee2,p_foliage2,p_wood2,p_root2, ncol = 1, heights = c(1.2,1,1,1,1,1))
    # ggsave(paste0(task_folder, "/plot/DAUnit3/2 Simulation_C_pools.tiff", p_comb2,width = 15, height = 24, units = "cm", scale = 1, dpi = 300)
    png(filename = paste0(task_folder, "/plot/DAUnit3/2 Simulation_C_pools.png"), width = 20, height = 32, units = "cm", res=800,pointsize = 10)
      print(p_comb2)
    dev.off()
    
    p_comb2_2 = ggarrange(p_gpp2_2,p_foliage2_2,ncol = 1, heights = c(1.1,1))
    png(filename = paste0(task_folder, "/plot/DAUnit3/2_1 Simulation_C_pools_uncertainty.png"), width = 20, height = 20, units = "cm", res=800,pointsize = 14)
      print(p_comb2_2)
    dev.off()
    
  }
  
  if(step==0|step==3) {
    p_comb3 = ggarrange(p_gpp3,p_reco3,p_nee3,p_foliage3,p_wood3,p_root3, ncol = 1, heights = c(1.2,1,1,1,1,1))
    # ggsave(paste0(task_folder, "/plot/DAUnit3/3 Simulation_C_fluxes.tiff", p_comb3,width = 15, height = 24, units = "cm", scale = 1, dpi = 300)
    png(filename = paste0(task_folder, "/plot/DAUnit3/3 Simulation_C_fluxes.png"), width = 20, height = 32, units = "cm", res=800,pointsize = 10)
      print(p_comb3)
    dev.off()

    p_comb3_2 = ggarrange(p_gpp3_2,p_foliage3_2,ncol = 1, heights = c(1.1,1))
    png(filename = paste0(task_folder, "/plot/DAUnit3/3_2 Simulation_C_fluxes_uncertainty.png"), width = 20, height = 20, units = "cm", res=800,pointsize = 14)
      print(p_comb3_2)
    dev.off()
    
  }
  
  if(step==0|step == 4) {
    p_comb4 = ggarrange(p_gpp4,p_reco4,p_nee4,p_foliage4,p_wood4,p_root4, ncol = 1, heights = c(1.2,1,1,1,1,1))
    # ggsave(paste0(task_folder, "/plot/DAUnit3/4 Simulation_all.tiff", p_comb4,width = 15, height = 24, units = "cm", scale = 1, dpi = 300)
    png(filename = paste0(task_folder, "/plot/DAUnit3/4 Simulation_all.png"), width = 20, height = 32, units = "cm", res=800,pointsize = 10)
      print(p_comb4)
    dev.off()
    
    p_comb4_2 = ggarrange(p_gpp4_2,p_foliage4_2,ncol = 1, heights = c(1.1,1))
    png(filename = paste0(task_folder, "/plot/DAUnit3/4_3 Simulation_C_pools_and_fluxes_uncertainty.png"), width = 20, height = 20, units = "cm", res=800,pointsize = 14)
      print(p_comb4_2)
    dev.off()
    
  }
  
