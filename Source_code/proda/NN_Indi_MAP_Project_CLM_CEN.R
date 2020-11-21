library(R.matlab)
library(ncdf4) 
library(ggplot2)
library(pracma)

rm(list=ls()) # clear memory
while (!is.null(dev.list()))  dev.off()

##

args = commandArgs(TRUE)

ifnn = args[1]
ifnn = as.integer(ifnn)
# ifnn = 2 # 0 for site by site, 1 for NN, 2 for one batch


model_name = 'clm_cen'

# path for mac
DataPath = args[2]

output_folder = args[3]

source(paste("./Source_code/proda/src_clm_cen/", 'matrix_fun.R', sep = ''))


# set vertical soil pools
use_vertsoilc = 1 
nspools = 7  # number of pools if no vertical
nspools_vr = 70 # number of pools if vertical
nSoilLayer = 10  # number of soil layers
# load original data
soildepth = readMat(paste(DataPath, 'input_data/soildepth.mat', sep = ''))  #soil depth information
zisoi = soildepth$zisoi
zsoi = soildepth$zsoi
dz = soildepth$dz
dz_node = soildepth$dz.node

fileindata = readMat(paste(DataPath, 'input_data/fileindata.mat', sep = '')) # data used in MCMC
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



ParaMean = readMat(paste(DataPath, 'input_data/ParaMean_V8.4.mat', sep = ''))
ParaMean = ParaMean$ParaMean

Para_Sumary = ParaMean[ , 3:ncol(ParaMean)]


if (ifnn == 1) {
  nn_predict = read.csv(paste(output_folder, 'output_data/nn_para_result_', model_name, '.csv', sep = ''), header = FALSE)
  nn_site_loc = read.csv(paste(output_folder, 'output_data/nn_site_loc_', model_name, '.csv', sep = ''), header = FALSE)
  nn_site_loc = nn_site_loc$V1
  para_for_us = Para_Sumary[nn_site_loc, ]
}

if (ifnn == 2) {
  batch_final_post = nc_open(paste(output_folder, 'output_data/para_keep2_log_cost_cov_01.nc', sep = ''))
  batch_final_post = ncvar_get(batch_final_post, 'para_keep2')
  
  batch_final_post = t(batch_final_post)
  batch_final_post = batch_final_post[which(is.na(batch_final_post[ , 1]) == 0), ]
  batch_final_post = batch_final_post[c(round(nrow(batch_final_post)/2):nrow(batch_final_post)), ]
  
  homo_para = colMeans(batch_final_post, na.rm = TRUE)
}

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
dt =secspday*30 # suppose it is monthly simulation
# nstep = length(tscalar_in(1,1,1,:))
lon_in = length(tscalar_in[ ,1,1])
lat_in = length(tscalar_in[1, ,1])

##
US_Loc = readMat(paste(DataPath, 'input_data/US_Loc.mat', sep = ''))
US_Loc = US_Loc$US.Loc


if (ifnn == 1) {
  US_Loc = nn_site_loc
}

indi_map_project_mod = array(NA, dim = c(length(US_Loc), 100))
indi_map_project_obs = array(NA, dim = c(length(US_Loc), 100))

site_by_site_mod = array(NA, dim = c(length(US_Loc), 10))

iprofile = 1
for (iprofile in 1:length(US_Loc)) {
  
  print(paste('Processing Profile', iprofile))
  profiles = US_Loc[iprofile]
  # find the lon and lat info of soil profile
  lon = GlobalProfile[profiles,4]
  lat = GlobalProfile[profiles,5]
  profile_id = GlobalProfile[profiles,1]
  # find currently using profile
  LocProfile = which(GlobalObsC[ ,1] == profile_id)
  # write all the info of observed carbon, including the info of carbon in different depth
  current_obsC = array(NA, dim = c(length(LocProfile), 5))
  # info of the depth of profile (depth of the node)
  current_obsC[ ,1] = GlobalObsC[LocProfile,4]
  # observed C info (g/m3)
  current_obsC[ ,2] = GlobalObsC[LocProfile,5]
  # specify the number of layers in studied profiles
  LayersProfile = length(current_obsC[ ,1])
  # weighting at different layers of soil profile
  weight = exp(-current_obsC[,1])
  weight[c(1, length(weight))] = 10
  
  if (max(current_obsC[ , 1]) < zsoi[10] && min(current_obsC[ , 1]) > zsoi[1]) {
    current_obsC[, 3] = interp1(zsoi[1:10], GlobalOrigModC[profiles, 2:11], current_obsC[ , 1], 'spline')
  } else {
    valid_loc = which(current_obsC[ , 1] < zsoi[10] & current_obsC[ , 1] > zsoi[1])
    current_obsC[valid_loc, 3] = interp1(zsoi[1:10], GlobalOrigModC[profiles, 2:11], current_obsC[valid_loc, 1], 'spline')
    current_obsC[which(current_obsC[ , 1] >= zsoi[10]), 3] = GlobalOrigModC[profiles, 11]
    current_obsC[which(current_obsC[ , 1] <= zsoi[1]), 3] = GlobalOrigModC[profiles, 2]
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
  # disable scalar of Oxygen
  # xin = ones(length(xin), 1)
  # NPP info of studied profile from MODIS NPP mean (year 2001-2016)
  NPP_mean=GlobalNPP[profiles,2]
  # NPP_sd(1,1)=GlobalNPP(profiles,3)
  if (GlobalNPP[profiles,4]==GlobalNPP[profiles,5]) {
    # if MODIS NPP min = MODIS NPP max
    # then min = mean - 0.5mean
    NPP_min=GlobalNPP[profiles,2] - 0.5*GlobalNPP[profiles,2]
    # max = mean + 0.5mean
    NPP_max=GlobalNPP[profiles,2] + 0.5*GlobalNPP[profiles,2]
  } else {
    # if NPP min ~= NPPmax
    NPP_min=GlobalNPP[profiles,4]
    NPP_max=GlobalNPP[profiles,5]
  }
  # Parameter names and their initial values in MCMC
  ParaName = c('diffus', 'cryo', 'q10', 'efolding', 'tau4cwd', 'tau4l1', 'tau4l2l3', 'tau4s1', 'tau4s2', 'tau4s3', 'fl1s1', 'fl2s1', 'fl3s2', 'fs1s2', 'fs1s3', 'fs2s1', 'fs2s3', 'fs3s1', 'fcwdl2', 'ins', 'beta', 'p4ll', 'p4ml', 'p4cl', 'minpsi')
  # number of paras
  nPara = length(ParaName)
  Min = array(0, dim = c(nPara, 1))
  Max = array(1, dim = c(nPara, 1))
  # set initial values of parameters
  if (ifnn == 1) {
    para_map = as.numeric(nn_predict[iprofile, ])
  } else if (ifnn == 2) {
    para_map = as.numeric(homo_para)
  } else {
    para_map = as.numeric(Para_Sumary[profiles, ])
  }
  
  if (is.na(para_map[1]) ==1) {
    next
  }
  # original modelled data
  Mod = matrix_fun(para_map, NPP_min, NPP_max, NPP_mean, altmax_Current_Indi, altmax_Lastyear_Indi, tscalar_indi, wscalar_indi, xio, xin)
  Mod = Mod$LayerC[ , 4]
  # profiles
  if (length(current_obsC[ ,1]) > 1 && is.na(Mod[1]) == 0) {
    
    if (max(current_obsC[ , 1]) < zsoi[10] && min(current_obsC[ , 1]) > zsoi[1]) {
      current_obsC[ , 3]=interp1(zsoi[1:nSoilLayer, 1], Mod, current_obsC[ ,1], 'spline')
    } else {
      valid_loc = which(current_obsC[ , 1] < zsoi[10] & current_obsC[ , 1] > zsoi[1])
      current_obsC[valid_loc, 3]=interp1(zsoi[1:nSoilLayer, 1], Mod, current_obsC[valid_loc,1], 'spline')
      current_obsC[which(current_obsC[ , 1] >= zsoi[10]), 3] = Mod[10]
      current_obsC[which(current_obsC[ , 1] <= zsoi[1]), 3] = Mod[1]
    }
    
    # scatter(current_obsC(:,1), current_obsC(:,2))
    site_by_site_mod[iprofile, ] = Mod
    indi_map_project_mod[iprofile, 1:length(current_obsC[ ,3])] = current_obsC[ ,3]
    indi_map_project_obs[iprofile, 1:length(current_obsC[ ,2])] = current_obsC[ ,2]
  }
  
}


indi_map_project_mod_1d = as.vector(indi_map_project_mod)
indi_map_project_obs_1d = as.vector(indi_map_project_obs)

corr_info = cor(indi_map_project_mod_1d[which(is.na(indi_map_project_obs_1d) == 0)], indi_map_project_obs_1d[which(is.na(indi_map_project_obs_1d) == 0)])

lm_info = lm(indi_map_project_mod_1d[which(is.na(indi_map_project_obs_1d) == 0)] ~ indi_map_project_obs_1d[which(is.na(indi_map_project_obs_1d) == 0)])


corr_info
summary_lm = summary(lm_info)

obs_vs_mod =
  ggplot() +
  geom_point(aes(x = indi_map_project_obs_1d[which(is.na(indi_map_project_obs_1d) == 0)]/1000, y = indi_map_project_mod_1d[which(is.na(indi_map_project_obs_1d) == 0)]/1000), size = 0.1, shape = 16) +
  geom_abline(slope = 1, intercept = 0) +
  annotate(geom = 'text', x = 0.1, y = 80, label = paste('R-Squared = ', round(summary_lm$r.squared, digits = 2), '***', '\nCorr = ', round(corr_info, digits = 2)), hjust = 0, size = 10) + 
  scale_x_log10(limits = c(min(indi_map_project_obs_1d)/1000, max(indi_map_project_obs_1d)/1000)) +
  scale_y_log10(limits = c(min(indi_map_project_obs_1d)/1000, max(indi_map_project_obs_1d)/1000)) +
  # change the background to black and white
  theme_bw() +
  # change the legend properties
  # theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background = element_rect(fill = NA)) +
  labs(x = expression(paste('Observation (log kg C m'^'-3', ')')) , y = expression(paste('Modelled (log kg C m'^'-3', ')'))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text=element_text(size=15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

nn_soc_validation = list(indi_map_project_mod = indi_map_project_mod, indi_map_project_obs = indi_map_project_obs)

site_by_site_mod = cbind(GlobalProfile[US_Loc, c(2:3)], site_by_site_mod)


if (ifnn == 0) {
  writeMat(paste(output_folder, 'output_data/SS_Simulation_World_', model_name, '.mat', sep = ''), site_by_site_mod = site_by_site_mod)
  
  jpeg(paste(output_folder, 'output_data/ss_obs_vs_mod.jpeg', sep = ''), units = 'in', width = 7, height = 7, res = 300)
  print(obs_vs_mod)
  dev.off()
}

if (ifnn == 1) {
  writeMat(paste(output_folder, 'output_data/nn_soc_validation_scale_', model_name, '.mat', sep = ''), nn_soc_validation = nn_soc_validation)
  
  jpeg(paste(output_folder, 'output_data/nn_obs_vs_mod.jpeg', sep = ''), units = 'in', width = 7, height = 7, res = 300)
  print(obs_vs_mod)
  dev.off()
}

if (ifnn == 2) {
  writeMat(paste(output_folder, 'output_data/ob_soc_validation_scale_', model_name, '.mat', sep = ''), nn_soc_validation = nn_soc_validation)

  jpeg(paste(output_folder, 'output_data/ob_obs_vs_mod.jpeg', sep = ''), units = 'in', width = 7, height = 7, res = 300)
  print(obs_vs_mod)
  dev.off()
}
  

