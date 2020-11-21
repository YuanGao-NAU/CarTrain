## Packages
library(R.matlab)
library(maps)
library(ggplot2)
library(interp)
library(gridExtra)
library(grid)
library(ModelMetrics)
library(R.matlab)
# library(colorplaner)
library(cowplot)
library(plotrix)
library(viridis)
library(jcolors)

rm(list=ls()) # clear memory
while (!is.null(dev.list()))  dev.off()

args = commandArgs(TRUE)

folder_dir = args[1]

#################################################################################
# Load Projected SOC
#################################################################################
## Neural NetWorking
GridInfo_NN = readMat(paste(folder_dir, 'output_data/GlobalGridResults_NN_clm_cen.mat', sep = ''))
GridInfo_NN = GridInfo_NN$GlobalGridResults

Grid_Loc_NN = read.table(paste(folder_dir, 'output_data/valid_grid_loc_clm_cen.csv', sep = ''))
Grid_Loc_NN = Grid_Loc_NN$V1

GlobalResults_Mean_NN = Re(GridInfo_NN[[2]])
GridInfo_NN = GridInfo_NN[[1]]

Loc_Invalid_Grid_NN = grep('NaN', GlobalResults_Mean_NN[ , 1])
GlobalResults_Mean_NN = GlobalResults_Mean_NN[-Loc_Invalid_Grid_NN, ]
GridInfo_NN = GridInfo_NN[-Loc_Invalid_Grid_NN, ]
colnames(GridInfo_NN) = c('Lon', 'Lat', 'NPP_Mean', 'NPP_Max', 'NPP_Min')

## one batch
GridInfo_OB = readMat(paste(folder_dir, 'output_data/GlobalGridResults_OB_clm_cen.mat', sep = ''))
GridInfo_OB = GridInfo_OB$GlobalGridResults

GlobalResults_Mean_OB = Re(GridInfo_OB[[2]])
GridInfo_OB = GridInfo_OB[[1]]

# GlobalResults_Mean_OB = GlobalResults_Mean_OB[Grid_Loc_NN, ]
# GridInfo_OB = GridInfo_OB[Grid_Loc_NN, ]

Loc_Invalid_Grid_OB = grep('NaN', GlobalResults_Mean_OB[ , 1])
GlobalResults_Mean_OB = GlobalResults_Mean_OB[-Loc_Invalid_Grid_OB, ]
GridInfo_OB = GridInfo_OB[-Loc_Invalid_Grid_OB, ]
colnames(GridInfo_OB) = c('Lon', 'Lat', 'NPP_Mean', 'NPP_Max', 'NPP_Min')

#################################################################################
# Change Unit from g/m3 to g/m2
#################################################################################

# soil layer depth
zf_soil = c(0.0175128180000000, 0.0450917870000000, 0.0905618200000000, 0.165529231000000, 0.289129597000000, 0.492912148000000, 0.828892774000000, 1.38283117900000, 2.29612121100000, 3.80188191200000)
dz = array(NA, dim = c(1, length(zf_soil)))
for (i in 1:length(zf_soil))
{
  if (i == 1)
  {
    dz[i] = zf_soil[i]
  }
  else
  {
    dz[i] = zf_soil[i] - zf_soil[i-1]
  }
}
# change the unit from gC/m3 to gC/m2
## NN
for (ilayer in 1:10) {
  GlobalResults_Mean_NN[ , ilayer] = GlobalResults_Mean_NN[ , ilayer]*dz[ilayer]
}


SOC_30cm_NN = rowSums(GlobalResults_Mean_NN[ , 1:5]) + GlobalResults_Mean_NN[ , 6]*(0.3 - zf_soil[5])/(zf_soil[6] - zf_soil[5])

SOC_100cm_NN = rowSums(GlobalResults_Mean_NN[ , 1:7]) + GlobalResults_Mean_NN[ , 8]*(1 - zf_soil[7])/(zf_soil[8] - zf_soil[7])

SOC_200cm_NN = rowSums(GlobalResults_Mean_NN[ , 1:8]) + GlobalResults_Mean_NN[ , 9]*(2 - zf_soil[8])/(zf_soil[9] - zf_soil[8])

## one batch 
for (ilayer in 1:10) {
  GlobalResults_Mean_OB[ , ilayer] = GlobalResults_Mean_OB[ , ilayer]*dz[ilayer]
}


SOC_30cm_OB = rowSums(GlobalResults_Mean_OB[ , 1:5]) + GlobalResults_Mean_OB[ , 6]*(0.3 - zf_soil[5])/(zf_soil[6] - zf_soil[5])

SOC_100cm_OB = rowSums(GlobalResults_Mean_OB[ , 1:7]) + GlobalResults_Mean_OB[ , 8]*(1 - zf_soil[7])/(zf_soil[8] - zf_soil[7])

SOC_200cm_OB = rowSums(GlobalResults_Mean_OB[ , 1:8]) + GlobalResults_Mean_OB[ , 9]*(2 - zf_soil[8])/(zf_soil[9] - zf_soil[8])

##############################################################################
# Global Grid info (different layer)
##############################################################################
## Jet colorbar function
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# plot grid world map
DepthName = c('Total', '0-20cm', '20-75cm', '75-150cm', '150-300cm', 'Below 300cm')
# can be changed to state or world to have US and world map
Map.Using = map_data('state')
Lat_Limit_Upper = 50
Lat_Limit_Lower = 25
# Specify the border of the map
## NN
Valid_Grid_Loc_NN = c()
LonUS = seq(from = -125.75, to = -64.75, by = 0.5)
for (iLon in 1:length(LonUS))
{
  MinLon = LonUS[iLon]
  MaxLon = LonUS[iLon] + 0.5
  LatRange = Map.Using$lat[Map.Using$long > MinLon & Map.Using$long < MaxLon]
  if (length(LatRange) != 0)
  {
    MinLat = min(LatRange)
    MaxLat = max(LatRange)
    Valid_Grid_Loc_NN = c(Valid_Grid_Loc_NN, which(GridInfo_NN[, 'Lon'] >= MinLon & GridInfo_NN[, 'Lon'] <= MaxLon & GridInfo_NN[, 'Lat'] >= MinLat & GridInfo_NN[, 'Lat'] <= MaxLat & GridInfo_NN[, 'Lat'] >= Lat_Limit_Lower & GridInfo_NN[, 'Lat'] <= Lat_Limit_Upper))
  }
}

## one batch
Valid_Grid_Loc_OB = c()
LonUS = seq(from = -125.75, to = -64.75, by = 0.5)
for (iLon in 1:length(LonUS))
{
  MinLon = LonUS[iLon]
  MaxLon = LonUS[iLon] + 0.5
  LatRange = Map.Using$lat[Map.Using$long > MinLon & Map.Using$long < MaxLon]
  if (length(LatRange) != 0)
  {
    MinLat = min(LatRange)
    MaxLat = max(LatRange)
    Valid_Grid_Loc_OB = c(Valid_Grid_Loc_OB, which(GridInfo_OB[, 'Lon'] >= MinLon & GridInfo_OB[, 'Lon'] <= MaxLon & GridInfo_OB[, 'Lat'] >= MinLat & GridInfo_OB[, 'Lat'] <= MaxLat & GridInfo_OB[, 'Lat'] >= Lat_Limit_Lower & GridInfo_OB[, 'Lat'] <= Lat_Limit_Upper))
  }
}


#############################################################################
# Calculation of SOC storage
#############################################################################
Radius = 6371008.8
Resolution = 0.5

## NN
Length_Top = (2*pi*Radius*cos(abs(GridInfo_NN[ , 'Lat']+Resolution/2)/180*pi)/360)*Resolution
Length_Down = (2*pi*Radius*cos(abs(GridInfo_NN[ , 'Lat']-Resolution/2)/180*pi)/360)*Resolution
Height = (pi*Radius/180)*Resolution
Area = (Length_Top + Length_Down)*Height/2
Store_SOC_30cm_NN = sum(Area[Valid_Grid_Loc_NN]*SOC_30cm_NN[Valid_Grid_Loc_NN]*10^(-15))
Store_SOC_100cm_NN = sum(Area[Valid_Grid_Loc_NN]*SOC_100cm_NN[Valid_Grid_Loc_NN]*10^(-15))
Store_SOC_200cm_NN = sum(Area[Valid_Grid_Loc_NN]*SOC_200cm_NN[Valid_Grid_Loc_NN]*10^(-15))

## one batch
Length_Top = (2*pi*Radius*cos(abs(GridInfo_OB[ , 'Lat']+Resolution/2)/180*pi)/360)*Resolution
Length_Down = (2*pi*Radius*cos(abs(GridInfo_OB[ , 'Lat']-Resolution/2)/180*pi)/360)*Resolution
Height = (pi*Radius/180)*Resolution
Area = (Length_Top + Length_Down)*Height/2
Store_SOC_30cm_OB = sum(Area[Valid_Grid_Loc_OB]*SOC_30cm_OB[Valid_Grid_Loc_OB]*10^(-15))
Store_SOC_100cm_OB = sum(Area[Valid_Grid_Loc_OB]*SOC_100cm_OB[Valid_Grid_Loc_OB]*10^(-15))
Store_SOC_200cm_OB = sum(Area[Valid_Grid_Loc_OB]*SOC_200cm_OB[Valid_Grid_Loc_OB]*10^(-15))


#############################################################################
# Spatial distribution of SOC
#############################################################################
## Jet colorbar function
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# plot grid world map
# can be changed to state or world to have US and world map
Map.Using = map_data('state')
bar_limit = c(35, 35, 35)

## NN
CurrentData_Total = data.frame(GridInfo_NN[Valid_Grid_Loc_NN, 1:2], SOC_30cm_NN[Valid_Grid_Loc_NN]/1000, SOC_100cm_NN[Valid_Grid_Loc_NN]/1000, SOC_200cm_NN[Valid_Grid_Loc_NN]/1000)

colnames(CurrentData_Total) = c('Lon', 'Lat', 'Project_30cm', 'Project_100cm', 'Project_200cm')

fig_name = c('Neural Networking 0 - 30cm', 'Neural Networking 0 - 100cm', 'Neural Networking 0 - 200cm')
## Projected SOC 
for (ifig in 1:3){
  CurrentData = CurrentData_Total[ , c(1:2, 2+ifig)]
  colnames(CurrentData) = c('Lon', 'Lat', 'Project')
  p = ggplot(data = CurrentData, aes(x = Lon, y = Lat, fill = Project)) +
    geom_tile(na.rm = TRUE) + 
    scale_fill_gradientn(name = expression(paste('SOC (kg C m'^'-2', ')')), colours = jet.colors(7), limits=c(0, bar_limit[ifig]), na.value="transparent", oob = scales::squish) + 
    geom_polygon(data = Map.Using, aes(x = long, y = lat, group = group), fill = NA, color = 'black', size = 1) +
    # change the background to black and white
    theme_bw() +
    # change the legend properties
    theme(legend.position = 'none') + 
    # theme(legend.justification = c(1, 0), legend.position = c(1, 0), legend.background = element_rect(fill = NA)) + 
    # change the size of colorbar
    guides(fill = guide_colorbar(barwidth = 1.5, barheight = 7)) + 
    theme(legend.text = element_text(size = 20), legend.title = element_text(size = 20)) +
    # add title
    labs(title = fig_name[ifig], x = '', y = '') + 
    # modify the position of title
    theme(plot.title = element_text(hjust = 0.5, size = 40)) + 
    # modify the font size
    theme(axis.title = element_text(size = 20)) + 
    # modify the margin
    theme(plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), 'inch')) +
    theme(axis.text=element_text(size = 30))
  
  eval(parse(text = paste('nn_p', ifig, ' = p', sep = '')))
}


## OB
CurrentData_Total = data.frame(GridInfo_OB[Valid_Grid_Loc_OB, 1:2], SOC_30cm_OB[Valid_Grid_Loc_OB]/1000, SOC_100cm_OB[Valid_Grid_Loc_OB]/1000, SOC_200cm_OB[Valid_Grid_Loc_OB]/1000)

colnames(CurrentData_Total) = c('Lon', 'Lat', 'Project_30cm', 'Project_100cm', 'Project_200cm')

fig_name = c('One Batch 0 - 30cm', 'One Batch 0 - 100cm', 'One Batch 0 - 200cm')
## Projected SOC 
for (ifig in 1:3){
  CurrentData = CurrentData_Total[ , c(1:2, 2+ifig)]
  colnames(CurrentData) = c('Lon', 'Lat', 'Project')
  p = ggplot(data = CurrentData, aes(x = Lon, y = Lat, fill = Project)) +
    geom_tile(na.rm = TRUE) + 
    scale_fill_gradientn(name = expression(paste('SOC \n(kg C m'^'-2', ')')), colours = jet.colors(7), limits=c(0, bar_limit[ifig]), na.value="transparent", oob = scales::squish) + 
    geom_polygon(data = Map.Using, aes(x = long, y = lat, group = group), fill = NA, color = 'black', size = 1) +
    # change the background to black and white
    theme_bw() +
    # change the legend properties
    theme(legend.position = 'none') + 
    # theme(legend.justification = c(1, 0), legend.position = c(1, 0), legend.background = element_rect(fill = NA)) + 
    # change the size of colorbar
    guides(fill = guide_colorbar(barwidth = 1.5, barheight = 6)) + 
    theme(legend.text = element_text(size = 20), legend.title = element_text(size = 20)) +
    # add title
    labs(title = fig_name[ifig], x = '', y = '') + 
    # modify the position of title
    theme(plot.title = element_text(hjust = 0.5, size = 40)) + 
    # modify the font size
    theme(axis.title = element_text(size = 20)) + 
    # modify the margin
    theme(plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), 'inch')) +
    theme(axis.text=element_text(size = 30))
  
  eval(parse(text = paste('ob_p', ifig, ' = p', sep = '')))
}

ob_p1 = ob_p1 +
  theme(legend.justification = c(0, 0), legend.position = c(0, 0), legend.background = element_rect(fill = NA))
label_names = c('a', 'b', 'c', 'd', 'e', 'f')
jpeg(paste(folder_dir, 'output_data/soc_map.jpeg', sep = ''), units = 'in', width = 25, height = 11, res = 300)
plot_grid(ob_p1, ob_p2, ob_p3, nn_p1, nn_p2, nn_p3, nrow = 2, labels = label_names, label_size = 45, label_fontface = 'plain', align = 'h', rel_widths = c(1, 1, 1))
dev.off()
