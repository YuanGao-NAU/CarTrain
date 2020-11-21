## Packages
library(R.matlab)
library(maps)
library(ggplot2)
library(interp)
library(gridExtra)
library(grid)
library(ModelMetrics)
# library(colorplaner)

rm(list=ls()) # clear memory
while (!is.null(dev.list()))  dev.off()

model_name = 'clm_cen'
# path for mac

args = commandArgs(TRUE)
DataPath = args[1]                         # Data folder

output_folder = args[2]

#################################################################################
# Load Projected para
#################################################################################
ParaNames = c('diffus', 'cryo', 'q10', 'efolding', 'tau4cwd', 'tau4l1', 'tau4l2l3', 'tau4s1', 'tau4s2', 'tau4s3', 'fl1s1', 'fl2s1', 'fl3s2', 'fs1s2', 'fs1s3', 'fs2s1', 'fs2s3', 'fs3s1', 'fcwdl2', 'ins', 'beta', 'p4ll', 'p4ml', 'p4cl', 'maxpsi')

## Neural NetWorking
Grid_Loc_NN = read.table(paste(output_folder, 'output_data/valid_grid_loc_clm_cen.csv', sep = ''))
Grid_Loc_NN = Grid_Loc_NN$V1

Pred_Para = read.csv(paste(output_folder, 'output_data/grid_para_result_clm_cen.csv', sep = ''))
colnames(Pred_Para) = ParaNames

GridInfo_NN = readMat(paste(DataPath, 'input_data/GlobalGrid4NN_SoilGrids.mat', sep = ''))
GridInfo_NN = GridInfo_NN$GlobalGrid
GridInfo_NN = GridInfo_NN[Grid_Loc_NN, 1:2]

colnames(GridInfo_NN) = c('Lon', 'Lat')

##############################################################################
# Global Grid info (different layer)
##############################################################################
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


#############################################################################
# Spatial distribution of para
#############################################################################
## Jet colorbar function
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
## NN
CurrentData_Total = data.frame(GridInfo_NN[Valid_Grid_Loc_NN, 1:2], Pred_Para[Valid_Grid_Loc_NN, ])

colnames(CurrentData_Total) = c('Lon', 'Lat', ParaNames)

## Projected para 
for (ipara in 1:length(ParaNames)){
  CurrentData = CurrentData_Total[ , c(1:2, 2+ipara)]
  colnames(CurrentData) = c('Lon', 'Lat', 'Project')
  p = ggplot(data = CurrentData, aes(x = Lon, y = Lat, fill = Project)) +
    geom_tile(na.rm = TRUE) + 
    scale_fill_gradientn(name = '', colours = jet.colors(7), limits=c(0, 1), na.value="transparent", oob = scales::squish) + 
    geom_polygon(data = Map.Using, aes(x = long, y = lat, group = group), fill = NA, color = 'black', size = 0.05) +
    # change the background to black and white
    theme_bw() +
    # change the legend properties
    theme(legend.position = 'none') + 
    # theme(legend.justification = c(1, 0), legend.position = c(1, 0), legend.background = element_rect(fill = NA)) + 
    # change the size of colorbar
    guides(fill = guide_colorbar(barwidth = 0.3, barheight = 1)) + 
    theme(legend.text = element_text(size = 3), legend.title = element_text(size = 5)) +
    # add title
    labs(title = ParaNames[ipara], x = '', y = '') + 
    # modify the position of title
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    # modify the font size
    theme(axis.title = element_text(size = 7)) + 
    # modify the margin
    theme(plot.margin = unit(c(0.0, 0.04, 0.02, 0.02), 'inch')) +
    theme(axis.text=element_text(size = 5))
  
  eval(parse(text = paste('p', ipara, ' = p', sep = '')))
}

p1 = p1 + 
  theme(legend.justification = c(1, 0), legend.position = c(1, 0), legend.background = element_rect(fill = NA)) + 
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 1.5)) + 
  theme(legend.text = element_text(size = 5), legend.title = element_text(size = 3)) 

jpeg(paste(output_folder, 'output_data/map_para_us.jpeg', sep = ''), units = 'in', width = 10, height = 7, res = 300)

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, nrow = 5, left = textGrob('', gp = gpar(fontsize=0), rot = 90), bottom = textGrob('', gp = gpar(fontsize=0)))
dev.off()





