library(R.matlab)
library(ncdf4) 
library(ggplot2)
library(pracma)
library(MASS)
library(grid)
library(gridExtra)

rm(list=ls()) # clear memory
while (!is.null(dev.list()))  dev.off()

args = commandArgs(TRUE)

data_path = args[1]
output_folder = args[2]

source(paste("./Source_code/proda/src_clm_cen/", 'matrix_fun.R', sep = ''))

sample_num = 10

# set vertical soil pools
use_vertsoilc = 1 
nspools = 7  # number of pools if no vertical
nspools_vr = 70 # number of pools if vertical
nSoilLayer = 10  # number of soil layers


# load original data
soildepth = readMat(paste(data_path, 'input_data/soildepth.mat', sep = ''))  #soil depth information
zisoi = soildepth$zisoi
zsoi = soildepth$zsoi
dz = soildepth$dz
dz_node = soildepth$dz.node

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

##
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

## Start of MCMC
US_Loc = readMat(paste(data_path, 'input_data/US_Loc.mat', sep = ''))
US_Loc = US_Loc$US.Loc

selected_profile = sample(US_Loc, sample_num, replace = FALSE)

# ggplot() +
#   geom_point(aes(y = GlobalProfile[selected_profile, 3], x = GlobalProfile[selected_profile, 2]))

# find the lon and lat info of soil profile
lon = GlobalProfile[selected_profile,4]
lat = GlobalProfile[selected_profile,5]
profile_id = GlobalProfile[selected_profile,1]

LayersProfile = array(NA, dim = c(sample_num, 1))
for (iprofile in 1:length(selected_profile)) {
  profiles = selected_profile[iprofile]
  # specify the number of layers in studied profiles
  LayersProfile[iprofile] = length(which(GlobalObsC[ ,1] == profile_id[iprofile]))
}

max_layer_num = max(LayersProfile, na.rm = TRUE)

# write all the info of observed carbon, including the info of carbon in different depth
LocProfile = array(NA, dim = c(sample_num, max_layer_num))
current_obsC = array(NA, dim = c(sample_num, max_layer_num, 5))
weight = array(NA, dim = c(sample_num, max_layer_num))

altmax_Current_Indi = array(NA, dim = c(sample_num, 1))
altmax_Lastyear_Indi = array(NA, dim = c(sample_num, 1))
wscalar_indi = array(NA, dim = c(sample_num, 15))
tscalar_indi = array(NA, dim = c(sample_num, 15))
xio = array(NA, dim = c(sample_num, 15))
xin = array(NA, dim = c(sample_num, 15))

NPP_mean = array(NA, dim = c(sample_num, 1))
NPP_min = array(NA, dim = c(sample_num, 1))
NPP_max = array(NA, dim = c(sample_num, 1))

iprofile = 1
for (iprofile in 1:length(selected_profile)) {
  profiles = selected_profile[iprofile]
  indi_layer_num = length(which(GlobalObsC[ ,1] == profile_id[iprofile]))
  # find currently using profile
  LocProfile[iprofile, 1:indi_layer_num] = which(GlobalObsC[ ,1] == profile_id[iprofile])
  
  # info of the depth of profile (depth of the node)
  current_obsC[iprofile, 1:indi_layer_num, 1] = GlobalObsC[LocProfile[iprofile, 1:indi_layer_num],4]
  # observed C info (g/m3)
  current_obsC[iprofile, 1:indi_layer_num, 2]=GlobalObsC[LocProfile[iprofile, 1:indi_layer_num],5]
  # weighting at different layers of soil profile
  indi_weight = exp(-current_obsC[iprofile, 1:indi_layer_num, 1])
  # indi_weight[c(1, length(indi_weight))] = 10
  weight[iprofile, 1:indi_layer_num] = indi_weight
  
  
  if (max(current_obsC[iprofile, 1:indi_layer_num, 1]) < zsoi[10] && min(current_obsC[iprofile, 1:indi_layer_num, 1]) > zsoi[1]) {
    current_obsC[iprofile, 1:indi_layer_num, 3] = interp1(zsoi[1:10], GlobalOrigModC[profiles, 2:11], current_obsC[iprofile, 1:indi_layer_num, 1], 'spline')
  } else {
    valid_loc = which(current_obsC[iprofile, , 1] < zsoi[10] & current_obsC[iprofile, , 1] > zsoi[1])
    current_obsC[iprofile, valid_loc, 3] = interp1(zsoi[1:10], GlobalOrigModC[profiles, 2:11], current_obsC[iprofile, valid_loc, 1], 'spline')
    current_obsC[iprofile, which(current_obsC[iprofile, , 1] >= zsoi[10]), 3] = GlobalOrigModC[profiles, 11]
    current_obsC[iprofile, which(current_obsC[iprofile, , 1] <= zsoi[1]), 3] = GlobalOrigModC[profiles, 2]
  }
  
  
  # alt current and last year
  altmax_Current_Indi[iprofile] = altmax_Current[lon[iprofile], lat[iprofile]]
  altmax_Lastyear_Indi[iprofile]  = altmax_Lastyear[lon[iprofile], lat[iprofile]]
  # input for water and temperature scalar
  wscalar_indi[iprofile, ] = wscalar_in[lon[iprofile], lat[iprofile], ]
  tscalar_indi[iprofile, ] = tscalar_in[lon[iprofile], lat[iprofile], ]
  # cell content
  # cellsand_indi = reshape(cellsand_in(lon, lat, :), [size(cellsand_in, 3), 1])
  # oxygen scalar
  xio[iprofile, ] = oscalar_in[lon[iprofile], lat[iprofile], ]
  # disable scalar of Oxygen
  # xio = ones(length(xio), 1)
  # nitrogen fpi_vr_in
  xin[iprofile, ] = fpi_vr_in[lon[iprofile], lat[iprofile], ]
  # disable scalar of Oxygen
  # xin = ones(length(xin), 1)
  # NPP info of studied profile from MODIS NPP mean (year 2001-2016)
  NPP_mean[iprofile] = GlobalNPP[profiles,2]
  # NPP_sd(1,1)=GlobalNPP(profiles,3)
  if (GlobalNPP[profiles,4]==GlobalNPP[profiles,5]) {
    # if MODIS NPP min = MODIS NPP max
    # then min = mean - 0.5mean
    NPP_min[iprofile]=GlobalNPP[profiles,2] - 0.5*GlobalNPP[profiles,2]
    # max = mean + 0.5mean
    NPP_max[iprofile]=GlobalNPP[profiles,2] + 0.5*GlobalNPP[profiles,2]
  } else {
    # if NPP min ~= NPPmax
    NPP_min[iprofile]=GlobalNPP[profiles,4]
    NPP_max[iprofile]=GlobalNPP[profiles,5]
  }
  
}


# Parameter names and their initial values in MCMC
ParaName = c('diffus', 'cryo', 'q10', 'efolding', 'tau4cwd', 'tau4l1', 'tau4l2l3', 'tau4s1', 'tau4s2', 'tau4s3', 'fl1s1', 'fl2s1', 'fl3s2', 'fs1s2', 'fs1s3', 'fs2s1', 'fs2s3', 'fs3s1', 'fcwdl2', 'ins', 'beta', 'p4ll', 'p4ml', 'p4cl', 'minpsi')
# number of paras
nPara = length(ParaName)
Min = array(0, dim = c(nPara, 1))
Max = array(1, dim = c(nPara, 1))
# set initial values of parameters
par0 = 0.5*array(1, dim = c(nPara, 1))

for (iprofile in 1:length(selected_profile)) {
  indi_layer_num = LayersProfile[iprofile]
  # original modelled data
  Mod = matrix_fun(par0, NPP_min[iprofile], NPP_max[iprofile], NPP_mean[iprofile], altmax_Current_Indi[iprofile], altmax_Lastyear_Indi[iprofile], tscalar_indi[iprofile, ], wscalar_indi[iprofile, ], xio[iprofile, ], xin[iprofile, ])
  
  Mod = Mod$LayerC[ , 4]
  # profiles
  if (max(current_obsC[iprofile, 1:indi_layer_num, 1]) < zsoi[10] && min(current_obsC[iprofile, 1:indi_layer_num, 1]) > zsoi[1]) {
    current_obsC[iprofile, 1:indi_layer_num, 3] = interp1(zsoi[1:10], Mod, current_obsC[iprofile, 1:indi_layer_num, 1], 'spline')
  } else {
    valid_loc = which(current_obsC[iprofile, , 1] < zsoi[10] & current_obsC[iprofile, , 1] > zsoi[1])
    current_obsC[iprofile, valid_loc, 3] = interp1(zsoi[1:10], Mod, current_obsC[iprofile, valid_loc, 1], 'spline')
    current_obsC[iprofile, which(current_obsC[iprofile, , 1] >= zsoi[10]), 3] = Mod[10]
    current_obsC[iprofile, which(current_obsC[iprofile, , 1] <= zsoi[1]), 3] = Mod[1]
  }
  
}

# ggplot() + 
#   geom_point(aes(x = current_obsC[ ,1], y = current_obsC[ ,2]), color = 'red') +
#   geom_point(aes(x = current_obsC[ ,1], y = current_obsC[ ,3]), color = 'blue')

## MCMC core part

# try
isSecondTime = 0
# Predefine the size of some coefficients
#nsimu1 = 20000
nsimu1 = 20000
#nsimu2 = 30000
nsimu2 = 30000
nParallel = 1

Parameters_rec1 = array(NA, dim = c(nParallel, nPara, nsimu1))
Parameters_rec2 = array(NA, dim = c(nParallel, nPara, nsimu2))
Parameters_keep1 = array(NA, dim = c(nParallel, nPara, nsimu1))
Parameters_keep2 = array(NA, dim = c(nParallel, nPara, nsimu2))

J_rec1 = array(NA, dim = c(nsimu1, nParallel))
J_rec2 = array(NA, dim = c(nsimu2, nParallel))
J_keep1 = array(NA, dim = c(nsimu1, nParallel))
J_keep2 = array(NA, dim = c(nsimu2, nParallel))

upgrade1 = array(NA, dim = c(nParallel, 1))
upgrade2 = array(NA, dim = c(nParallel, 1))

#calculating prior cost function value
obs_1d = as.vector(current_obsC[ , , 2])
weight_1d = as.vector(weight)

mod_1d = as.vector(current_obsC[ , , 3])

J_old1 = sum(weight*(((mod_1d-obs_1d)**2)/(2*((0.6*obs_1d)**2))), na.rm = TRUE)
## Part1: MCMC run to calculate parameter covariances
iParallel = 1
Count2 = 0
while (iParallel <= nParallel) {
  Parameters_rec1[iParallel, , ] = NA
  Parameters_rec2[iParallel, , ] = NA
  Parameters_keep1[iParallel, , ] = NA
  Parameters_keep2[iParallel, , ] = NA
  
  J_rec1[ , iParallel] = NA
  J_rec2[ , iParallel] = NA
  J_keep1[ , iParallel] = NA
  J_keep2[ , iParallel] = NA
  

  J_old = J_old1
  J_Control = 1
  allow = 5*array(1, dim = c(nPara, 1))
  # allow(strcmp(ParaName, 'tau4p')) = 90
  # give original parameter values to par_old
  par_old=par0
  # interval width
  diff = Max - Min
  upgrade1[iParallel, 1] = 0
  simu = 1
  Count1 = 0
  ifbreak1 = 0
  while (simu <= nsimu1) {
    while (TRUE) {
      # generate new parameter values in the interval of
      # [-0.5, 0.5]*diff/allow + par_old
      par_new = as.numeric(par_old+(runif(nPara, min = 0, max = 1)-0.5)*diff/allow)
      
      # all the new valued parameters should be in the preassumed
      # intervals
      if (par_new[1]>Min[1] && par_new[1]<Max[1]
          && par_new[2]>Min[2] && par_new[2]<Max[2]
          && par_new[3]>Min[3] && par_new[3]<Max[3]
          && par_new[4]>Min[4] && par_new[4]<Max[4]
          && par_new[5]>Min[5] && par_new[5]<Max[5] 
          && par_new[6]>Min[6] && par_new[6]<Max[6] 
          && par_new[7]>Min[7] && par_new[7]<Max[7]
          && par_new[8]>Min[8] && par_new[8]<Max[8]
          && par_new[9]>Min[9] && par_new[9]<Max[9]
          && par_new[10]>Min[10] && par_new[10]<Max[10]
          && par_new[11]>Min[11] && par_new[11]<Max[11]
          && par_new[12]>Min[12] && par_new[12]<Max[12]
          && par_new[13]>Min[13] && par_new[13]<Max[13]
          && par_new[14]>Min[14] && par_new[14]<Max[14]
          && par_new[15]>Min[15] && par_new[15]<Max[15]
          && par_new[16]>Min[16] && par_new[16]<Max[16]
          && par_new[17]>Min[17] && par_new[17]<Max[17]
          && par_new[18]>Min[18] && par_new[18]<Max[18]
          && par_new[19]>Min[19] && par_new[19]<Max[19]
          && par_new[20]>Min[20] && par_new[20]<Max[20]
          && par_new[21]>Min[21] && par_new[21]<Max[21] 
          && par_new[22]>Min[22] && par_new[22]<Max[22]
          && par_new[23]>Min[23] && par_new[23]<Max[23]
          && par_new[24]>Min[24] && par_new[24]<Max[24]
          && par_new[25]>Min[25] && par_new[25]<Max[25]
          && par_new[22]*(0.25 - 0) + 0 + par_new[23]*(0.7 - 0.05) + 0.05 + par_new[24]*(0.5 - 0.01) + 0.01 < 1) {
        break
      }
    }
    
    
    # Model simulation
    for (iprofile in 1:length(selected_profile)) {
      indi_layer_num = LayersProfile[iprofile]
      # original modelled data
      Mod = matrix_fun(par_new, NPP_min[iprofile], NPP_max[iprofile], NPP_mean[iprofile], altmax_Current_Indi[iprofile], altmax_Lastyear_Indi[iprofile], tscalar_indi[iprofile, ], wscalar_indi[iprofile, ], xio[iprofile, ], xin[iprofile, ])
      
      Mod = Mod$LayerC[ , 4]
      # profiles
      if (max(current_obsC[iprofile, 1:indi_layer_num, 1]) < zsoi[10] && min(current_obsC[iprofile, 1:indi_layer_num, 1]) > zsoi[1]) {
        current_obsC[iprofile, 1:indi_layer_num, 4] = interp1(zsoi[1:10], Mod, current_obsC[iprofile, 1:indi_layer_num, 1], 'spline')
      } else {
        valid_loc = which(current_obsC[iprofile, , 1] < zsoi[10] & current_obsC[iprofile, , 1] > zsoi[1])
        current_obsC[iprofile, valid_loc, 4] = interp1(zsoi[1:10], Mod, current_obsC[iprofile, valid_loc, 1], 'spline')
        current_obsC[iprofile, which(current_obsC[iprofile, , 1] >= zsoi[10]), 4] = Mod[10]
        current_obsC[iprofile, which(current_obsC[iprofile, , 1] <= zsoi[1]), 4] = Mod[1]
      }
      
    }
    
    mod_1d = as.vector(current_obsC[ , , 4])
    J_new = sum(weight*(((mod_1d-obs_1d)**2)/(2*((0.6*obs_1d)**2))), na.rm = TRUE)
    
    if (J_new == 0 ) {
      J_new = J_old + 100
    }    
    
    delta_J = J_new-J_old
    # to decide whether to accept new parameter values
    if (min(1, exp(-delta_J*log(simu + 2))) > runif(1)) {
      # update the number of simulation
      upgrade1[iParallel, 1] = upgrade1[iParallel, 1] + 1
      disp(paste('upgrade1_parallel_', num2str(iParallel, fmt = 0), ' : ', num2str(upgrade1[iParallel, 1], fmt = 0), ' / ', num2str(simu, fmt = 0), sep = ''))
      # record accepted parameter values
      Parameters_keep1[iParallel, , upgrade1[iParallel, 1]] = par_new
      # record accepted values of cost function
      J_keep1[upgrade1[iParallel, 1], iParallel]=J_new
      # update the value of parameter values
      par_old = par_new
      # update old cost function value
      J_old = J_new
    }
    # give parameter values to Parameters_rec for covarance matrix,
    # rec: record
    Parameters_rec1[iParallel, , simu]=par_old
    J_rec1[simu, iParallel]=J_old
    simu = simu + 1
    
    if (simu == nsimu1 && isSecondTime == 0) {
      if (upgrade1[iParallel, 1] < 50) {
        Count1 = Count1 + 1
        simu = 1
        upgrade1[iParallel, 1] = 0
        Parameters_keep1[iParallel, ,  ] = NA
        Parameters_rec1[iParallel,  , ] = NA
        J_rec1[ , iParallel] = NA
        J_keep1[ , iParallel] = NA
        allow = allow*1.1
      }
    }
    if (Count1 == 3) {
      ifbreak1 = 1
      isSecondTime = 1
      break
    }
    
  }
  if (ifbreak1 == 1) {
    continue
  }
  
  ## Part 2: MCMC run with updating covariances
  sd=2.4/nPara
  sacling = 0.0
  # sd=sqrt(2.4^2/nPara)
  covars=sd*cov(t(Parameters_rec1[iParallel, , 1:nsimu1])) + sd*sacling*diag(ones(nPara, 1))
  # update the value of upgrade and simu for new task
  upgrade2[iParallel, 1] = 0
  # the very first cost functino value in simulation
  J_old = J_old1
  simu = 1
  ifbreak2 = 0
  ifbreak3 = 0
  while (simu <= nsimu2) {
    MonitorTime2 = Sys.time()
    while (TRUE) {
      # mointor the consumed time at the initial stage (if too long i.e. 10min, then back to the first proposal step)
      if (upgrade2[iParallel, 1] == 0) {
        ConsumeTime2 = as.numeric(Sys.time() - MonitorTime2)
        
        if (ConsumeTime2 > 1*60) {
          ifbreak2 = 1
        }
      }
      
      if (ifbreak2 == 1) {
        break
      }
      
      tryCatch(
        expr = {
          par_new = as.numeric(mvrnorm(n = 1, mu = par_old, Sigma = covars))
        },
        error = function(e) {
          ifbreak3 = 1
          break
        }
      )
      
      if (par_new[1]>Min[1] && par_new[1]<Max[1]
          && par_new[2]>Min[2] && par_new[2]<Max[2]
          && par_new[3]>Min[3] && par_new[3]<Max[3]
          && par_new[4]>Min[4] && par_new[4]<Max[4]
          && par_new[5]>Min[5] && par_new[5]<Max[5] 
          && par_new[6]>Min[6] && par_new[6]<Max[6] 
          && par_new[7]>Min[7] && par_new[7]<Max[7]
          && par_new[8]>Min[8] && par_new[8]<Max[8]
          && par_new[9]>Min[9] && par_new[9]<Max[9]
          && par_new[10]>Min[10] && par_new[10]<Max[10]
          && par_new[11]>Min[11] && par_new[11]<Max[11]
          && par_new[12]>Min[12] && par_new[12]<Max[12]
          && par_new[13]>Min[13] && par_new[13]<Max[13]
          && par_new[14]>Min[14] && par_new[14]<Max[14]
          && par_new[15]>Min[15] && par_new[15]<Max[15]
          && par_new[16]>Min[16] && par_new[16]<Max[16]
          && par_new[17]>Min[17] && par_new[17]<Max[17]
          && par_new[18]>Min[18] && par_new[18]<Max[18]
          && par_new[19]>Min[19] && par_new[19]<Max[19]
          && par_new[20]>Min[20] && par_new[20]<Max[20]
          && par_new[21]>Min[21] && par_new[21]<Max[21] 
          && par_new[22]>Min[22] && par_new[22]<Max[22]
          && par_new[23]>Min[23] && par_new[23]<Max[23]
          && par_new[24]>Min[24] && par_new[24]<Max[24]
          && par_new[25]>Min[25] && par_new[25]<Max[25]
          && par_new[22]*(0.25 - 0) + 0 + par_new[23]*(0.7 - 0.05) + 0.05 + par_new[24]*(0.5 - 0.01) + 0.01 < 1) {
        break
      }
    }
    
    if (ifbreak2 == 1 || ifbreak3 == 1) {
      break
    }
    
    # Model simulation
    # Model simulation
    for (iprofile in 1:length(selected_profile)) {
      indi_layer_num = LayersProfile[iprofile]
      # original modelled data
      Mod = matrix_fun(par_new, NPP_min[iprofile], NPP_max[iprofile], NPP_mean[iprofile], altmax_Current_Indi[iprofile], altmax_Lastyear_Indi[iprofile], tscalar_indi[iprofile, ], wscalar_indi[iprofile, ], xio[iprofile, ], xin[iprofile, ])
      
      Mod = Mod$LayerC[ , 4]
      # profiles
      if (max(current_obsC[iprofile, 1:indi_layer_num, 1]) < zsoi[10] && min(current_obsC[iprofile, 1:indi_layer_num, 1]) > zsoi[1]) {
        current_obsC[iprofile, 1:indi_layer_num, 4] = interp1(zsoi[1:10], Mod, current_obsC[iprofile, 1:indi_layer_num, 1], 'spline')
      } else {
        valid_loc = which(current_obsC[iprofile, , 1] < zsoi[10] & current_obsC[iprofile, , 1] > zsoi[1])
        current_obsC[iprofile, valid_loc, 4] = interp1(zsoi[1:10], Mod, current_obsC[iprofile, valid_loc, 1], 'spline')
        current_obsC[iprofile, which(current_obsC[iprofile, , 1] >= zsoi[10]), 4] = Mod[10]
        current_obsC[iprofile, which(current_obsC[iprofile, , 1] <= zsoi[1]), 4] = Mod[1]
      }
      
    }
    
    mod_1d = as.vector(current_obsC[ , , 4])
    J_new = sum(weight*(((mod_1d-obs_1d)**2)/(2*((0.6*obs_1d)**2))), na.rm = TRUE)
    
    if (J_new == 0 ) {
      J_new = J_old + 100
    }    
  
    delta_J = J_new-J_old
    
    # to determie whether to accept the newly modelled data
    if (min(1, exp(-delta_J*log(simu + 2))) > runif(1)) {
      upgrade2[iParallel, 1] = upgrade2[iParallel, 1] + 1
      
      disp(paste('upgrade2_parallel_', num2str(iParallel, fmt = 0), ' : ', num2str(upgrade2[iParallel, 1], fmt = 0), ' / ', num2str(simu, fmt = 0), sep = ''))
      
      # record the accepted parameter value
      Parameters_keep2[iParallel, , upgrade2[iParallel, 1]] = par_new
      
      J_keep2[upgrade2[iParallel, 1], iParallel] = J_new
      par_old = par_new
      J_old = J_new
      coef = upgrade2[iParallel, 1]/simu
    }
    
    Parameters_rec2[iParallel, , simu] = par_old
    J_rec2[simu, iParallel] = J_old
    if (simu > 4000) {
      covars=sd*(cov(t(Parameters_rec2[iParallel, , 1:simu])) + sacling*diag(as.numeric(array(1, dim = c(nPara, 1)))))
    }
    simu = simu + 1
    # to test if the acceptance rate is in a resonable level
    if (simu == nsimu2) {
      if (coef < 0.1 || coef > 0.5) {
        ifbreak2 = 1
        Count2 = Count2 + 1
        break
      }
    }
    
  }
  
  if (ifbreak2 == 0 && ifbreak3 == 0) {
    iParallel = iParallel + 1
  }
  if (Count2 > 5) {
    erf()
  }
} 

disp('MCMC has been finished')

para_keep2 = Parameters_keep2[1, , ]



# path and file name, set dname
ncpath <- output_folder
ncname_data <- "output_data/para_keep2_log_cost_cov_01"  
ncfname_data <- paste(ncpath, ncname_data, ".nc", sep = "")

# create and write the netCDF file -- ncdf4 version
# define dimensions
iteratoin_dim <- ncdim_def("row", units = 'None', vals = 1:nrow(para_keep2))
para_dim <- ncdim_def("col", units = 'None', vals = 1:ncol(para_keep2))

# define variables
fillvalue <- NA

dlname <- "para_keep2"
para_deep2_def <- ncvar_def(name = "para_keep2", units = "None", dim = list(iteratoin_dim, para_dim), missval = fillvalue, longname = dlname, prec = "double")

# create netCDF file and put arrays
# ncout <- nc_create(filename = ncfname, vars = soc_content_def) 
ncout_data <- nc_create(filename = ncfname_data, vars = para_deep2_def)

# put variables
ncvar_put(ncout_data, para_deep2_def, as.matrix(para_keep2))

# Get a summary of the created file:
ncout_data

# close the file, writing data to disk
nc_close(ncout_data)

# batch_final_post = nc_open(paste(data_path, '/para_keep2_log_cost_cov_01.nc', sep = ''))
# batch_final_post = ncvar_get(batch_final_post, 'para_keep2')


#########################################################
# plot figures soc vs depth
#########################################################
current_data = c()
for (iprofile in 1:sample_num){
  current_data = rbind(current_data, cbind(current_obsC[iprofile, , 1:2], iprofile))
}

current_data = data.frame(current_data)
colnames(current_data) = c('depth', 'soc', 'profile')
current_data$profile = as.factor(current_data$profile)

jpeg(paste(output_folder, 'output_data/one_batch_depth_vs_soc.jpeg', sep = ''), units = 'in', width = 7, height = 7, res = 300)
ggplot() +
  geom_line(data = current_data, aes(x = depth, y = soc/1000, color = profile), size = 1) +
  # change the background to black and white
  theme_bw() +
  # change the legend properties
  theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.background = element_rect(fill = NA)) +
  labs(x = 'Depth (m)' , y = expression(paste('Obs. SOC (kg C m'^'-3', ')'))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text=element_text(size=15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()

#########################################################
# plot figures sample spatial dsitribution
#########################################################
Map.Using = map_data('state')

current_data = cbind(GlobalProfile[selected_profile, 2], GlobalProfile[selected_profile, 3], c(1:sample_num))
current_data = data.frame(current_data)
colnames(current_data) = c('lon', 'lat', 'profile')
current_data$profile = as.factor(current_data$profile)

jpeg(paste(output_folder, 'output_data/one_batch_profile_dist.jpeg', sep = ''), units = 'in', width = 10, height = 7, res = 300)
ggplot(data = current_data, aes(x = lon, y = lat, color = profile)) +
  geom_polygon(data = Map.Using, aes(x = long, y = lat, group = group), fill = NA, color = 'black', size = 1) +
  geom_point(size = 5, shape = 16) +
  # change the background to black and white
  theme_bw() +
  # change the legend properties
  theme(legend.position = 'none') + 
  # theme(legend.justification = c(1, 0), legend.position = c(1, 0), legend.background = element_rect(fill = NA)) + 
  # change the size of colorbar
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 7)) + 
  theme(legend.text = element_text(size = 20), legend.title = element_text(size = 20)) +
  # add title
  labs(title = '', x = '', y = '') + 
  # modify the position of title
  theme(plot.title = element_text(hjust = 0.5, size = 40)) + 
  # modify the font size
  theme(axis.title = element_text(size = 20)) + 
  # modify the margin
  theme(plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), 'inch')) +
  theme(axis.text=element_text(size = 30))
dev.off()
#########################################################
# plot figures posterior distribution
#########################################################

batch_final_post_small = nc_open(paste(output_folder, 'output_data/para_keep2_log_cost_cov_01.nc', sep = ''))
batch_final_post_small = ncvar_get(batch_final_post_small, 'para_keep2')

batch_final_post_small = t(batch_final_post_small)
batch_final_post_small = batch_final_post_small[which(is.na(batch_final_post_small[ , 1]) == 0), ]
batch_final_post_small = batch_final_post_small[c(round(nrow(batch_final_post_small)/2):nrow(batch_final_post_small)), ]

for (iPara in 1:length(ParaName))
{
  CurrentData = data.frame(batch_final_post_small[ , iPara])
  colnames(CurrentData) = 'Data'
  # find optimal value
  DensityInfo = density(CurrentData$Data)
  
  # find peak of other parameters
  Peak = DensityInfo$x[which.max(DensityInfo$y)]
  p = ggplot(data = CurrentData) +
    geom_histogram(aes(x = Data, y=..density..), bins = 90) +
    geom_density(aes(x = Data, y=..density..), col = 'red', linetype = 'longdash', alpha = 1, size = 1) +
    geom_hline(yintercept=0, colour = "white", size = 1.2) +
    # geom_vline(xintercept = Peak, color = 'red', linetype = 'longdash', size = 1, alpha = 1) +
    theme_bw() +
    ggtitle(ParaName[iPara]) +
    theme(plot.title = element_text(hjust = 0.5, size = 30)) +
    labs(x = '', y = '') + 
    theme(axis.text=element_text(size=25)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  eval(parse(text = paste('p', iPara, ' = p', sep = '')))
}

jpeg(paste(output_folder, 'output_data/one_batch_Histogram_Para.jpeg', sep = ''),  units = 'in', width = 25, height = 25, res = 300)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, nrow = 5, left = textGrob('Density', gp = gpar(fontsize=45), rot = 90), bottom = textGrob('Parameter Value', gp = gpar(fontsize=45)))
dev.off()

