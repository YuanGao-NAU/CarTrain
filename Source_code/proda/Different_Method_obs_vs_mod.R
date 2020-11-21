## Packages
library(R.matlab)
library(maps)
library(ggplot2)
library(interp)
library(gridExtra)
library(grid)
library(maps)
library(cowplot)
library(Metrics)

rm(list=ls()) # clear memory
while (!is.null(dev.list()))  dev.off()

args = commandArgs(TRUE)

folder_dir = args[1]
output_folder = args[2]

####################################################################################
# Load data NN
####################################################################################
nn_para_result = read.csv(paste(output_folder, 'output_data/nn_para_result_clm_cen.csv', sep = ''))
nn_para_result = data.matrix(nn_para_result)
nn_site_loc = read.csv(paste(output_folder, 'output_data/nn_site_loc_clm_cen.csv', sep = ''))
nn_site_loc = data.matrix(nn_site_loc)

soc_summary = readMat(paste(output_folder, 'output_data/nn_soc_validation_scale_clm_cen.mat', sep = ''))
soc_summary = soc_summary$nn.soc.validation
matrix_mod = soc_summary[[1]]
matrix_obs = soc_summary[[2]]
soc_mod_nn = as.vector(soc_summary[[1]])
soc_obs = as.vector(soc_summary[[2]])

# Load Environmental Factors
EnvInfo = readMat(paste(folder_dir, 'input_data/EnvInfo.mat', sep = ''))
EnvInfo.Variables = c('ProfileNum', 'ProfileID', 'LayerNum', 'Lon', 'Lat', 'LonGrid', 'LatGrid', 'IGBP', 'Climate', 'Soil_Type', 'NPPmean', 'NPPmax', 'NPPmin', 'MAT', 'Preci', 'R_Squared')
EnvInfo = data.frame(EnvInfo$EnvInfo)
# EnvInfo = EnvInfo[ , 1:12]
colnames(EnvInfo) = EnvInfo.Variables
EnvInfo = EnvInfo[nn_site_loc, ]


obs_depth_interval = readMat(paste(folder_dir, 'input_data/GlobalObsC.mat', sep = ''))
obs_depth_interval = obs_depth_interval$GlobalObsC[ , c(1, 2, 3)]
obs_depth_interval = data.frame(obs_depth_interval, as.numeric(NA))
colnames(obs_depth_interval) = c('id', 'upper', 'lower', 'depth')
obs_depth_interval$depth = (obs_depth_interval$lower - obs_depth_interval$upper)/100

obs_layer_depth = array(NA, c(length(EnvInfo$ProfileID), 35))
for (iprofile in 1:length(EnvInfo$ProfileID)) {
  loc_profile = which(obs_depth_interval$id == EnvInfo$ProfileID[iprofile])
  obs_layer_depth[iprofile, 1:length(loc_profile)] = obs_depth_interval$depth[loc_profile]
}

####################################################################################
# plot obs soc v.s. nn
####################################################################################
## Jet colorbar function
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

nn_data_lm = data.frame(cbind(soc_obs, soc_mod_nn))
colnames(nn_data_lm) = c('obs', 'mod')
nn_data_lm = nn_data_lm[is.na(nn_data_lm[ , 1]) == 0, ]/1000
nn_data_lm = nn_data_lm[nn_data_lm$mod > 0.01, ]

# nn_data_lm$mod = nn_data_lm$mod - mean(nn_data_lm$mod - nn_data_lm$obs)

cor.test(nn_data_lm$obs, nn_data_lm$mod, method = 'pearson')
rmse(nn_data_lm$obs, nn_data_lm$mod)

Linear.Regression = lm(mod ~ obs + 0, data = nn_data_lm)
summary_lm = summary(Linear.Regression)
summary_lm

Linear.Regression = lm(mod ~ obs, data = nn_data_lm)
summary_lm = summary(Linear.Regression)
summary_lm
# nn_data_lm$mod = nn_data_lm$mod/summary_lm$coefficients[1]
# Linear.Regression = lm(mod ~ obs + 0, data = nn_data_lm)
# summary_lm = summary(Linear.Regression)

nn_data_lm = cbind(nn_data_lm, Linear.Regression$fitted.values)
colnames(nn_data_lm) = c('obs', 'mod', 'lm')

nn_lm = ggplot(data = nn_data_lm) + 
  stat_bin_hex(aes(x = obs, y = mod), bins = 60) +
  scale_fill_gradientn(colours = jet.colors(7), limits = c(1, 200), oob = scales::squish) + 
  # geom_point(aes(x = obs, y = mod), shape = 20, size = 0.5, alpha = 0.8) +
  # geom_line(aes(x = obs, y = lm), colour = 'red', linetype = 'longdash', size = 1) + 
  geom_line(aes(x = obs, y =  obs), colour = 'black', linetype = 'solid', size = 1) + 
  # axis limit
  coord_cartesian(ylim = c(quantile(nn_data_lm$obs, 0), quantile(nn_data_lm$obs, 1)), xlim = c(quantile(nn_data_lm$obs, 0), quantile(nn_data_lm$obs, 1))) + 
  annotate(geom = 'text', x = 0.1, y = 100, label = paste('R-Squared = ', round(summary_lm$r.squared, digits = 2), '***', '\nRMSE = ', round(rmse(nn_data_lm$obs, nn_data_lm$mod), digits = 2)), hjust = 0, size = 10) + 
  scale_x_log10() +
  scale_y_log10() +
  # change the size of colorbar
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 15)) + 
  # change the background to black and white
  theme_bw() +
  # change the legend properties
  # theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background = element_rect(fill = NA)) +
  labs(x = expression(paste('Observation (log kg C m'^'-3', ')')) , y = expression(paste('Modelled (log kg C m'^'-3', ')'))) +
  theme(axis.title = element_text(size = 20)) + 
  theme(axis.text=element_text(size=15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# nn_lm

####################################################################################
# Load data OB
####################################################################################
soc_summary = readMat(paste(output_folder, 'output_data/ob_soc_validation_scale_clm_cen.mat', sep = ''))
soc_summary = soc_summary$nn.soc.validation
matrix_mod = soc_summary[[1]]
matrix_obs = soc_summary[[2]]
soc_mod_nn = as.vector(soc_summary[[1]])
soc_obs = as.vector(soc_summary[[2]])

# Load Environmental Factors
EnvInfo = readMat(paste(folder_dir, 'input_data/EnvInfo.mat', sep = ''))
EnvInfo.Variables = c('ProfileNum', 'ProfileID', 'LayerNum', 'Lon', 'Lat', 'LonGrid', 'LatGrid', 'IGBP', 'Climate', 'Soil_Type', 'NPPmean', 'NPPmax', 'NPPmin', 'MAT', 'Preci', 'R_Squared')
EnvInfo = data.frame(EnvInfo$EnvInfo)
# EnvInfo = EnvInfo[ , 1:12]
colnames(EnvInfo) = EnvInfo.Variables


obs_depth_interval = readMat(paste(folder_dir, 'input_data/GlobalObsC.mat', sep = ''))
obs_depth_interval = obs_depth_interval$GlobalObsC[ , c(1, 2, 3)]
obs_depth_interval = data.frame(obs_depth_interval, as.numeric(NA))
colnames(obs_depth_interval) = c('id', 'upper', 'lower', 'depth')
obs_depth_interval$depth = (obs_depth_interval$lower - obs_depth_interval$upper)/100

obs_layer_depth = array(NA, c(length(EnvInfo$ProfileID), 35))
for (iprofile in 1:length(EnvInfo$ProfileID)) {
  loc_profile = which(obs_depth_interval$id == EnvInfo$ProfileID[iprofile])
  obs_layer_depth[iprofile, 1:length(loc_profile)] = obs_depth_interval$depth[loc_profile]
}

####################################################################################
# plot obs soc v.s. nn
####################################################################################
## Jet colorbar function
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

ob_data_lm = data.frame(cbind(soc_obs, soc_mod_nn))
colnames(ob_data_lm) = c('obs', 'mod')
ob_data_lm = ob_data_lm[is.na(ob_data_lm[ , 1]) == 0, ]/1000
ob_data_lm = ob_data_lm[ob_data_lm$mod > 0.01, ]


cor.test(ob_data_lm$obs, ob_data_lm$mod, method = 'pearson')
rmse(ob_data_lm$obs, ob_data_lm$mod)

Linear.Regression = lm(mod ~ obs + 0, data = ob_data_lm)
summary_lm = summary(Linear.Regression)
summary_lm

Linear.Regression = lm(mod ~ obs, data = ob_data_lm)
summary_lm = summary(Linear.Regression)
summary_lm
# ob_data_lm$mod = ob_data_lm$mod/summary_lm$coefficients[1]
# Linear.Regression = lm(mod ~ obs + 0, data = ob_data_lm)
# summary_lm = summary(Linear.Regression)

ob_data_lm = cbind(ob_data_lm, Linear.Regression$fitted.values)
colnames(ob_data_lm) = c('obs', 'mod', 'lm')

ob_lm = ggplot(data = ob_data_lm) + 
  stat_bin_hex(aes(x = obs, y = mod), bins = 60) +
  scale_fill_gradientn(colours = jet.colors(7), limits = c(1, 200), oob = scales::squish) + 
  # geom_point(aes(x = obs, y = mod), shape = 20, size = 0.5, alpha = 0.8) +
  # geom_line(aes(x = obs, y = lm), colour = 'red', linetype = 'longdash', size = 1) + 
  geom_line(aes(x = obs, y =  obs), colour = 'black', linetype = 'solid', size = 1) + 
  # axis limit
  coord_cartesian(ylim = c(quantile(ob_data_lm$obs, 0), quantile(ob_data_lm$obs, 1)), xlim = c(quantile(ob_data_lm$obs, 0), quantile(ob_data_lm$obs, 1))) + 
  annotate(geom = 'text', x = 0.1, y = 100, label = paste('R-Squared = ', round(summary_lm$r.squared, digits = 2), '***', '\nRMSE = ', round(rmse(ob_data_lm$obs, ob_data_lm$mod), digits = 2)), hjust = 0, size = 10) + 
  scale_x_log10() +
  scale_y_log10() +
  # change the size of colorbar
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 15)) + 
  # change the background to black and white
  theme_bw() +
  # change the legend properties
  # theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background = element_rect(fill = NA)) +
  labs(x = expression(paste('Observation (log kg C m'^'-3', ')')) , y = expression(paste('Modelled (log kg C m'^'-3', ')'))) +
  theme(axis.title = element_text(size = 20)) + 
  theme(axis.text=element_text(size=15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# ob_lm

################################################################
# Post setting of figures (vertical)
################################################################
ob_lm = ob_lm +
  ggtitle('One Batch') +
  theme(plot.title = element_text(size = (40), hjust = 0.5)) +
  theme(legend.position = 'none') +
  labs(x = expression(paste('Observed SOC (log kg C m'^'-3', ')')) , y = expression(paste('Modelled SOC (log kg C m'^'-3', ')'))) +
  theme(axis.text=element_text(size=30), axis.title=element_text(size = 30))

nn_lm = nn_lm +
  ggtitle('Neural Networking') +
  theme(plot.title = element_text(size = (40), hjust = 0.5)) +
  theme(legend.position = 'none')  +
  labs(x = expression(paste('Observed SOC (log kg C m'^'-3', ')')) , y = expression(paste('Modelled SOC (log kg C m'^'-3', ')'))) +
  theme(axis.text=element_text(size=30), axis.title=element_text(size = 30))

jpeg(paste(output_folder, 'output_data/mod_vs_obs_ob.jpeg', sep = ''), units = 'in', width = 10, height = 10, res = 300)
ob_lm
dev.off()

jpeg(paste(output_folder, 'output_data/mod_vs_obs_nn.jpeg', sep = ''), units = 'in', width = 10, height = 10, res = 300)
nn_lm
dev.off()
