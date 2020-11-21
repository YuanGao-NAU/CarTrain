library(R.matlab)
library(ncdf4) 
library(ggplot2)
library(pracma)

rm(list=ls()) # clear memory
while (!is.null(dev.list()))  dev.off()

##

args = commandArgs(TRUE)

is_nn = args[1]
is_nn = as.integer(is_nn)

# is_nn = 0 # 1 for NN, 0 for one batch

data_path = args[2]
output_folder = args[3]

source(paste("./Source_code/proda/src_clm_cen/", 'matrix_fun.R', sep = ''))

model_name = 'clm_cen'

ParaNames = c('diffus', 'cryo', 'q10', 'efolding', 'tau4cwd', 'tau4l1', 'tau4l2l3', 'tau4s1', 'tau4s2', 'tau4s3', 'fl1s1', 'fl2s1', 'fl3s2', 'fs1s2', 'fs1s3', 'fs2s1', 'fs2s3', 'fs3s1', 'fcwdl2', 'ins', 'beta', 'p4ll', 'p4ml', 'p4cl', 'maxpsi')
nPara = length(ParaNames)

EnvInfo = readMat(paste(data_path, 'input_data/EnvInfo.mat', sep = ''))
EnvInfo = EnvInfo$EnvInfo

US_Loc = readMat(paste(data_path, 'input_data/US_Loc.mat', sep = ''))
US_Loc = US_Loc$US.Loc
##
fileindata = readMat(paste(data_path, 'input_data/fileindata.mat', sep = '')) # data used in MCMC
GlobalNPP = fileindata$GlobalNPP
GlobalObsC = fileindata$GlobalObsC
GlobalOrigModC = fileindata$GlobalOrigModC
GlobalProfile = fileindata$GlobalProfile

altmax_in = fileindata$altmax.in
altmax_lastyear_in = fileindata$altmax.lastyear.in
cellsand_in = fileindata$cellsand.in
fpi_vr_in = fileindata$fpi.vr.in
mlat_in = fileindata$mlat.in
mlon_in = fileindata$mlon.in
oscalar_in = fileindata$oscalar.in
soilpsi_in = fileindata$soilpsi.in
tsoil_in= fileindata$tsoil.in

soildepth = readMat(paste(data_path, 'input_data/soildepth.mat', sep = ''))  #soil depth information
zisoi = soildepth$zisoi
zsoi = soildepth$zsoi
dz = soildepth$dz
dz_node = soildepth$dz.node

Global_NPP_Half_Deg = readMat(paste(data_path, 'input_data/Global_NPP_Half_Deg.mat', sep = ''))
NPP_Max = Global_NPP_Half_Deg$NPP.Max
NPP_Mean = Global_NPP_Half_Deg$NPP.Mean
NPP_Min = Global_NPP_Half_Deg$NPP.Min
NPP_Std = Global_NPP_Half_Deg$NPP.Std

GlobalGrid4NN_SoilGrids = readMat(paste(data_path, 'input_data/GlobalGrid4NN_SoilGrids.mat', sep = ''))
GlobalGrid = GlobalGrid4NN_SoilGrids$GlobalGrid


grid_para_result = read.csv(paste(output_folder, 'output_data/grid_para_result_', model_name, '.csv', sep = ''), header = FALSE)

valid_grid_loc = read.csv(paste(output_folder, 'output_data/valid_grid_loc_', model_name, '.csv', sep = ''), header = FALSE)
valid_grid_loc = valid_grid_loc$V1

batch_final_post = nc_open(paste(output_folder, 'output_data/para_keep2_log_cost_cov_01.nc', sep = ''))
batch_final_post = ncvar_get(batch_final_post, 'para_keep2')

batch_final_post = t(batch_final_post)
batch_final_post = batch_final_post[which(is.na(batch_final_post[ , 1]) == 0), ]
batch_final_post = batch_final_post[c(round(nrow(batch_final_post)/2):nrow(batch_final_post)), ]


homo_para = colMeans(batch_final_post)

## Global SOC and Turnover Time
use_vertsoilc = 1 
nspools = 7  # number of pools if no vertical
nspools_vr = 70 # number of pools if vertical
nSoilLayer = 10  # number of soil layers

# global mean temp in the past 120 months
tscalar_in = apply(tsoil_in, c(1, 2, 3), mean, na.rm = TRUE)
# global mean soil water potential
wscalar_in = apply(soilpsi_in, c(1, 2, 3), mean, na.rm = TRUE)
# global mean oxygen scalar
oscalar_in = apply(oscalar_in, c(1, 2, 3), mean, na.rm = TRUE) 
# for sand content percentage
# cellsand_in = mean(cellsand_in, 4, 'omitnan')
# nitrogen fpi
fpi_vr_in = apply(fpi_vr_in, c(1, 2, 3), mean, na.rm = TRUE) 
# alt of current year and last year
altmax_Current = apply(altmax_in, c(1, 2), mean, na.rm = TRUE) 
altmax_Lastyear = apply(altmax_lastyear_in, c(1, 2), mean, na.rm = TRUE) 

kelvin_to_celsius=273.15
normalization_tref=11.75+(29.7/pi)*atan(pi*0.031*(15-15.4))

# seconds per day
secspday = 24*60*60

# grid info
GlobalGrid = GlobalGrid[valid_grid_loc, ]
GridInfo = GlobalGrid[, c(1, 2, 6, 7, 8)]
ValidGrid = length(GridInfo[ , 1])

GlobalResults_Mean = array(NA, c(ValidGrid, nSoilLayer))

Old_LonGrid = c(seq(from = 0+2.5/2, to = 180-2.5/2, by = 2.5),  seq(from = -180 + 2.5/2, to = 0 - 2.5/2, by = 2.5))

Old_LatGrid = seq(from = -90 + 1.875/2, to = 90 - 1.875/2, by = 1.875)

iGrid = 1
for (iGrid in 1:nrow(GridInfo)) {
  print(paste('Processing Grid: ', iGrid, sep = ''))
  Grid_Lon = GridInfo[iGrid,1]
  Grid_Lat = GridInfo[iGrid,2]
  # find location in original resolution
  lon = which(abs(Old_LonGrid - Grid_Lon) == min(abs(Old_LonGrid - Grid_Lon)))
  if (length(lon) > 1) {
    lon = lon(1)
  }
  
  lat = which(abs(Old_LatGrid - Grid_Lat) == min(abs(Old_LatGrid - Grid_Lat)))
  if (length(lat) > 1) {
    lat = lat[1]
  }
  
  # alt current and last year
  altmax_Current_Indi = altmax_Current[lon, lat]
  altmax_Lastyear_Indi = altmax_Lastyear[lon, lat]
  # input for water and temperature scalar
  wscalar_indi = wscalar_in[lon, lat, ]
  tscalar_indi = tscalar_in[lon, lat, ] 
  # cell content
  # cellsand_indi = reshape(cellsand_in(lon, lat, :), [size(cellsand_in, 3), 1])
  # oxygen scalar
  xio = oscalar_in[lon, lat, ]
  # disable scalar of Oxygen
  # xio = ones(length(xio), 1)
  # nitrogen fpi_vr_in
  xin = fpi_vr_in[lon, lat, ]
  # NPP info of studied profile from MODIS NPP mean (year 2001-2016)
  NPP_mean=GridInfo[iGrid,3]
  NPP_min=GridInfo[iGrid,5]
  NPP_max=GridInfo[iGrid,4]
  
  if (is_nn == 0) {
    Para = homo_para
  } else {
    Para = as.numeric(grid_para_result[iGrid, ])
  }
  
  if (is.na(Para[1]) == 1 || is.na(NPP_mean) == 1 || is.na(altmax_Current_Indi) == 1) {
    next
  }
  
  Mod = matrix_fun(Para, NPP_min, NPP_max, NPP_mean, altmax_Current_Indi, altmax_Lastyear_Indi, tscalar_indi, wscalar_indi, xio, xin)
  
  GlobalResults_Mean[iGrid, ] = Mod$LayerC[ , 4]
  
  
}

# delete(gcp('nocreate'))
##
GlobalGridResults = list(GridInfo = GridInfo, GlobalResults_Mean = GlobalResults_Mean)

if (is_nn == 1) {
  writeMat(paste(output_folder, 'output_data/GlobalGridResults_NN_', model_name, '.mat', sep = ''), GlobalGridResults = GlobalGridResults)
} else {
  writeMat(paste(output_folder, 'output_data/GlobalGridResults_OB_', model_name, '.mat', sep = ''), GlobalGridResults = GlobalGridResults)
}

disp('Global Projection Finished')
