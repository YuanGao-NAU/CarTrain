from pandas import Series as se
from pandas import DataFrame as df
from scipy.io import loadmat
import scipy.stats
import pandas as pd
import numpy as np

import tensorflow as tf
from tensorflow.keras import backend as K
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Conv2D, Dropout, Flatten, MaxPooling2D
from tensorflow.keras.layers import Dropout
from tensorflow.keras.optimizers import SGD

from matplotlib import pyplot as plt

import random

import sys

########################################################
# Folder Path
########################################################
folder_dir = sys.argv[1]
output_folder = sys.argv[2]

########################################################
# Environment Setting
########################################################
#print('Deafult or self defined setting? 1 for default, 0 for self-defined')
user_input = sys.argv[3]
is_default_setting = int(user_input)

config = tf.ConfigProto() 
config.intra_op_parallelism_threads = 1 
config.inter_op_parallelism_threads= 1 
sess = tf.Session(config=config)

if (is_default_setting == 1): 
	nn_loss = 'joint_loss' # mse
	nn_optimizer = 'adadelta'
	nn_batch_size = 64
	nn_epochs = 1200
	nn_layer_num = [256, 512, 512, 256]
	nn_drop_ratio = [0.3, 0.5, 0.5, 0.3]
	
	use_custom_activation = 0
	nn_activation = [None]*len(nn_layer_num)

	if use_custom_activation == 1:
    	# define activation function
		def custom_activation(x):
			custom_activation = tf.keras.activations.relu(x, alpha = 0.1)
			return custom_activation
			
		for ilayer in range(len(nn_layer_num)):
			get_custom_objects().update({'custom_activation_'+str(ilayer): Activation(custom_activation)})
			nn_activation[ilayer] = 'custom_activation_'+str(ilayer)
	else:
		nn_activation = ['relu', 'relu', 'relu', 'relu']
else: 
	# nn_loss = 'joint_loss' # mse
	#print('Enter your loss function, possible options: joint_loss, mse')
	user_input = sys.argv[4]
	nn_loss = user_input
	
	# nn_optimizer = 'adadelta'
	#print('Enter your optimizer, possible options: adadelta, adam, RMSprop')
	user_input = sys.argv[5]
	nn_optimizer = user_input
	
	# nn_batch_size = 64
	#print('Enter your batch size, possible options: an interger, recomanding smaller than 100')
	user_input = sys.argv[6]
	nn_batch_size = int(user_input)
	
	# nn_epochs = 1200
	#print('Enter your epochs size, possible options: an interger, e.g. 1200')
	user_input = sys.argv[7]
	nn_epochs = int(user_input)
	
	# nn_layer_num = [256, 512, 512, 256]
	#print('Enter your layer numbers, e.g. 4 for four hidden layers')
	user_input = sys.argv[8]
	nn_layer = int(user_input)
	
	#print('Enter neurone numder for each layer, e.g. 256 for each hidden layers')
	user_input = sys.argv[9]
	nn_layer_num = [int(user_input)]*nn_layer
	
	# nn_drop_ratio = [0.3, 0.5, 0.5, 0.3]
	#print('Enter drop ratio for each layer, e.g. 0.3 for each hidden layers')
	user_input = sys.argv[10]
	nn_drop_ratio = [float(user_input)]*nn_layer
	
		
	use_custom_activation = 0
	
	if use_custom_activation == 1:
    	# define activation function
		def custom_activation(x):
			custom_activation = tf.keras.activations.relu(x, alpha = 0.1)
			return custom_activation
			
		for ilayer in range(len(nn_layer_num)):
			get_custom_objects().update({'custom_activation_'+str(ilayer): Activation(custom_activation)})
			nn_activation[ilayer] = 'custom_activation_'+str(ilayer)
	else:
		# nn_activation = ['relu', 'relu', 'relu', 'relu']
		#print('Enter your activation function, e.g. relu')
		user_input = sys.argv[11]
		nn_activation = [user_input]*nn_layer

########################################################
# Import para info after bayesian method
########################################################
# laod para info after MCMC
para_without_trans = loadmat(folder_dir + 'input_data/ParaMean_V8.4.mat')
para_without_trans = df(para_without_trans['ParaMean'])
para_without_trans = para_without_trans.iloc[:, 2:]
para_names = ['diffus', 'cryo', 'q10', 'efolding', 'tau4cwd', 'tau4l1', 'tau4l2l3', 'tau4s1', 'tau4s2', 'tau4s3', 'fl1s1', 'fl2s1', 'fl3s2', 'fs1s2', 'fs1s3', 'fs2s1', 'fs2s3', 'fs3s1', 'fcwdl2', 'ins', 'beta', 'p4ll', 'p4ml', 'p4cl', 'maxpsi']
para_without_trans.columns = para_names
# environmental info of soil profiles
env_info = loadmat(folder_dir + 'input_data/EnvInfo4NN_SoilGrids.mat')
env_info = df(env_info['EnvInfo'])
env_info_names = ['ProfileNum', 'ProfileID', 'MaxDepth', 'LayerNum', 'Lon', 'Lat', 'LonGrid', 'LatGrid', 'IGBP', 'Climate', 'Soil_Type', 'NPPmean', 'NPPmax', 'NPPmin', \
'Veg_Cover', \
'BIO1', 'BIO2', 'BIO3', 'BIO4', 'BIO5', 'BIO6', 'BIO7', 'BIO8', 'BIO9', 'BIO10', 'BIO11', 'BIO12', 'BIO13', 'BIO14', 'BIO15', 'BIO16', 'BIO17', 'BIO18', 'BIO19', \
'Abs_Depth_to_Bedrock', \
'Bulk_Density_0cm', 'Bulk_Density_30cm', 'Bulk_Density_100cm',\
'CEC_0cm', 'CEC_30cm', 'CEC_100cm', \
'Clay_Content_0cm', 'Clay_Content_30cm', 'Clay_Content_100cm', \
'Coarse_Fragments_v_0cm', 'Coarse_Fragments_v_30cm', 'Coarse_Fragments_v_100cm', \
'Depth_Bedrock_R', \
'Garde_Acid', \
'Occurrence_R_Horizon', \
'pH_Water_0cm', 'pH_Water_30cm', 'pH_Water_100cm', \
'Sand_Content_0cm', 'Sand_Content_30cm', 'Sand_Content_100cm', \
'Silt_Content_0cm', 'Silt_Content_30cm', 'Silt_Content_100cm', \
'SWC_v_Wilting_Point_0cm', 'SWC_v_Wilting_Point_30cm', 'SWC_v_Wilting_Point_100cm', \
'Texture_USDA_0cm', 'Texture_USDA_30cm', 'Texture_USDA_100cm', \
'USDA_Suborder', \
'WRB_Subgroup', \
'Drought', \
'R_Squared']

env_info.columns = env_info_names
env_info.BIO1 = env_info.BIO1 + 273.15
env_info.BIO5 = env_info.BIO5 + 273.15
env_info.BIO6 = env_info.BIO6 + 273.15
env_info.BIO8 = env_info.BIO8 + 273.15
env_info.BIO9 = env_info.BIO9 + 273.15
env_info.BIO10 = env_info.BIO10 + 273.15
env_info.BIO11 = env_info.BIO11 + 273.15

env_info.Drought = env_info.Drought + 1
# env_info.Drought = (env_info.Drought - (-5))/(5 - (-5))
# environmental info of global grids
grid_env_info = loadmat(folder_dir + 'input_data/GlobalGrid4NN_SoilGrids.mat')
grid_env_info = df(grid_env_info['GlobalGrid'])
grid_env_info_names = ['Lon', 'Lat', 'IGBP', 'Climate', 'Soil_Type', 'NPPmean', 'NPPmax', 'NPPmin', \
'Veg_Cover', \
'BIO1', 'BIO2', 'BIO3', 'BIO4', 'BIO5', 'BIO6', 'BIO7', 'BIO8', 'BIO9', 'BIO10', 'BIO11', 'BIO12', 'BIO13', 'BIO14', 'BIO15', 'BIO16', 'BIO17', 'BIO18', 'BIO19', \
'Abs_Depth_to_Bedrock', \
'Bulk_Density_0cm', 'Bulk_Density_30cm', 'Bulk_Density_100cm', \
'CEC_0cm', 'CEC_30cm', 'CEC_100cm', \
'Clay_Content_0cm', 'Clay_Content_30cm', 'Clay_Content_100cm', \
'Coarse_Fragments_v_0cm', 'Coarse_Fragments_v_30cm', 'Coarse_Fragments_v_100cm', \
'Depth_Bedrock_R', \
'Garde_Acid', \
'Occurrence_R_Horizon', \
'pH_Water_0cm', 'pH_Water_30cm', 'pH_Water_100cm', \
'Sand_Content_0cm', 'Sand_Content_30cm', 'Sand_Content_100cm', \
'Silt_Content_0cm', 'Silt_Content_30cm', 'Silt_Content_100cm', \
'SWC_v_Wilting_Point_0cm', 'SWC_v_Wilting_Point_30cm', 'SWC_v_Wilting_Point_100cm', \
'Texture_USDA_0cm', 'Texture_USDA_30cm', 'Texture_USDA_100cm', \
'USDA_Suborder', \
'WRB_Subgroup', \
'Drought']
grid_env_info.columns = grid_env_info_names
grid_env_info.BIO1 = grid_env_info.BIO1 + 273.15
grid_env_info.BIO5 = grid_env_info.BIO5 + 273.15
grid_env_info.BIO6 = grid_env_info.BIO6 + 273.15
grid_env_info.BIO8 = grid_env_info.BIO8 + 273.15
grid_env_info.BIO9 = grid_env_info.BIO9 + 273.15
grid_env_info.BIO10 = grid_env_info.BIO10 + 273.15
grid_env_info.BIO11 = grid_env_info.BIO11 + 273.15

grid_env_info.Drought = grid_env_info.Drought + 1
# grid_env_info.Drought = (grid_env_info.Drought - (-5))/(5 - (-5))
# variables used in training the NN
var4nn = ['IGBP', 'Climate', 'Soil_Type', 'NPPmean', 'NPPmax', 'NPPmin', \
'Veg_Cover', \
'BIO1', 'BIO2', 'BIO3', 'BIO4', 'BIO5', 'BIO6', 'BIO7', 'BIO8', 'BIO9', 'BIO10', 'BIO11', 'BIO12', 'BIO13', 'BIO14', 'BIO15', 'BIO16', 'BIO17', 'BIO18', 'BIO19', \
'Abs_Depth_to_Bedrock', \
'Bulk_Density_0cm', 'Bulk_Density_30cm', 'Bulk_Density_100cm',\
'CEC_0cm', 'CEC_30cm', 'CEC_100cm', \
'Clay_Content_0cm', 'Clay_Content_30cm', 'Clay_Content_100cm', \
'Coarse_Fragments_v_0cm', 'Coarse_Fragments_v_30cm', 'Coarse_Fragments_v_100cm', \
'Depth_Bedrock_R', \
'Garde_Acid', \
'Occurrence_R_Horizon', \
'pH_Water_0cm', 'pH_Water_30cm', 'pH_Water_100cm', \
'Sand_Content_0cm', 'Sand_Content_30cm', 'Sand_Content_100cm', \
'Silt_Content_0cm', 'Silt_Content_30cm', 'Silt_Content_100cm', \
'SWC_v_Wilting_Point_0cm', 'SWC_v_Wilting_Point_30cm', 'SWC_v_Wilting_Point_100cm', \
'Texture_USDA_0cm', 'Texture_USDA_30cm', 'Texture_USDA_100cm', \
'USDA_Suborder', \
'WRB_Subgroup', \
'Drought']
# para names in clm4.5
para4nn = ['q10', 'efolding', 'tau4cwd', 'tau4s1', 'tau4s2', 'tau4s3', 'fl1s1', 'fl2s1', 'fl3s2', 'fs1s2', 'fs1s3', 'fs2s1', 'fs2s3', 'fs3s1', 'fcwdl2', 'beta', 'p4ll', 'p4ml', 'p4cl', 'maxpsi']
# R2 of soil profiles in MCMC to eliminate some profiles
r_squared = loadmat(folder_dir + 'input_data/EnvInfo.mat')
r_squared = df(r_squared['EnvInfo'])
r_squared = r_squared.iloc[:, -1]

########################################################
# specify the profiles to US continent
########################################################
# profiles that in use
site_loc = loadmat(folder_dir + 'input_data/US_Loc.mat')
site_loc = np.ndarray.flatten(site_loc['US_Loc'])
# site_loc = np.array(range(1, len(para_without_trans)+1))
para_without_trans = para_without_trans.iloc[(site_loc - 1), :]
env_info = env_info.iloc[(site_loc - 1), :]
r_squared = r_squared[(site_loc - 1)]
# delete nan value and R2 < 0
nan_loc = np.ndarray.flatten(np.argwhere(np.isnan(env_info.R_Squared)))
# valid_loc = site_loc[(np.isnan(r_squared) == False) & (r_squared > 0)] # np.where find the location
valid_loc = site_loc[(np.isnan(r_squared) == False) & (r_squared > 0) & (env_info.MaxDepth > 50) & (env_info.LayerNum > 2)]
# currentdata_y = np.array(para_without_trans.loc[(np.isnan(r_squared) == False) & (r_squared > 0), :])
currentdata_y = np.array(para_without_trans.loc[(np.isnan(r_squared) == False) & (r_squared > 0) & (env_info.MaxDepth > 50) & (env_info.LayerNum > 2), :])
# currentdata_x = np.array(env_info.loc[(np.isnan(r_squared) == False) & (r_squared > 0), var4nn])
currentdata_x = np.array(env_info.loc[(np.isnan(r_squared) == False) & (r_squared > 0) & (env_info.MaxDepth > 50) & (env_info.LayerNum > 2), var4nn])


current_grid = np.array(grid_env_info.loc[:, var4nn])
valid_grid_loc = np.array(range(len(current_grid[:, 1]))) + 1
# delete veired data
for ivar in range(len(var4nn)):
	currentdata_y = currentdata_y[currentdata_x[:, ivar] >= 0, :]
	valid_loc = valid_loc[currentdata_x[:, ivar] >= 0]
	currentdata_x = currentdata_x[currentdata_x[:, ivar] >= 0, :]
	valid_grid_loc = valid_grid_loc[current_grid[:, ivar] >= 0]
	current_grid = current_grid[current_grid[:, ivar] >= 0, :]

# normalization for x
for ivar in range(len(var4nn)):
	currentdata_x[:, ivar] = (currentdata_x[:, ivar] - min(current_grid[:, ivar]))/(max(current_grid[:, ivar]) - min(current_grid[:, ivar]))
	current_grid[:, ivar] = (current_grid[:, ivar] - min(current_grid[:, ivar]))/(max(current_grid[:, ivar]) - min(current_grid[:, ivar]))

# seperate all the data into train, test and validation set with the propotion of 8:1:1
train_loc = np.asarray(sorted(random.sample(range(len(currentdata_x)), round(0.8*len(currentdata_x)))))
test_loc = np.setdiff1d(np.asarray(range(len(currentdata_x))), train_loc)
# validation_loc =dd test_loc[np.asarray(sorted(random.sample(range(len(test_loc)), round(0.5*len(test_loc)))))]
# test_loc = np.setdiff1d(test_loc, validation_loc)

########################################################
# Build NN
########################################################
# split into input and outputs
train_x = currentdata_x[train_loc, :]
train_y = currentdata_y[train_loc, :]
 
test_x = currentdata_x[test_loc, :]
test_y = currentdata_y[test_loc, :]

print(train_x.shape, train_y.shape, test_x.shape, test_y.shape)

# define a joint loss
para_mse = 1000*0.5
para_ratio = 10*0.5
def joint_loss (y_true, y_pred):
    # mse
	mse_loss = K.mean(K.square(y_true - y_pred))
	# mean absolute ratio  error
	ratio_loss = K.mean(K.abs((y_true - y_pred)/y_true))
	# return the joint loss
	return para_mse*mse_loss + para_ratio*ratio_loss

def ratio_loss (y_true, y_pred):
	# mean absolute ratio  error
	ratio_loss = K.mean(K.abs((y_true - y_pred)/y_true))
	return ratio_loss


# design network
model = Sequential()

# hidden layers
for ilayer in range(len(nn_layer_num)):
	if use_custom_activation == 1:
		if ilayer == 0:
			model.add(Dense(nn_layer_num[ilayer], input_dim = len(var4nn)))
			model.add(Activation(custom_activation, name = nn_activation[ilayer]))
			model.add(Dropout(nn_drop_ratio[ilayer]))
		else:
			model.add(Dense(nn_layer_num[ilayer]))
			model.add(Activation(custom_activation, name = nn_activation[ilayer]))
			model.add(Dropout(nn_drop_ratio[ilayer]))
	else:
		if ilayer == 0:
			model.add(Dense(nn_layer_num[ilayer], input_dim = len(var4nn), activation=nn_activation[ilayer]))
			model.add(Dropout(nn_drop_ratio[ilayer]))
		else:
			model.add(Dense(nn_layer_num[ilayer], activation=nn_activation[ilayer]))
			model.add(Dropout(nn_drop_ratio[ilayer]))

model.add(Dense(len(para_names)))

# define loss and optimizer
if nn_loss == 'joint_loss':
	model.compile(loss = joint_loss, optimizer = nn_optimizer, metrics = ['accuracy'])
else:
	model.compile(loss = nn_loss, optimizer = nn_optimizer, metrics = ['accuracy'])

model.summary()

# fit network
history = model.fit(x = train_x, y = train_y, epochs = nn_epochs, batch_size = nn_batch_size, validation_split = 0.2)
# save the trained model
model.save(output_folder + 'output_data/trained_nn_model.h5')

# predict para based on the trained NN
nn_predict = model.predict(test_x)
grid_predict = model.predict(current_grid)

########################################################
# Visualization of NN Results
########################################################
## loss function
plt.plot(history.history['loss'])    
plt.plot(history.history['val_loss'])
plt.yscale('log')
plt.xscale('log')
plt.legend(['train', 'validation'], loc = 'upper left')
plt.savefig(output_folder + 'output_data/loss_nn.png')
plt.close()

## predicted paras
corr_para = [None]*len(para_names)
for ipara in range(len(para_names)):
	corr_para[ipara] = np.corrcoef(test_y[:, ipara], nn_predict[:, ipara])[0, 1]
	plt.subplot(5, 5, ipara + 1)
	plt.plot(test_y[:, ipara], nn_predict[:, ipara], 'o', markersize = 0.1, markeredgecolor = 'red', markerfacecolor = 'red')
	plt.title(para_names[ipara])

plt.savefig(output_folder + 'output_data/para_nn.png')
plt.close()

########################################################
# Save the output
########################################################
nn_site_loc =  valid_loc[test_loc] 
np.savetxt(output_folder + 'output_data/nn_para_result_clm_cen.csv', nn_predict, delimiter = ',')
np.savetxt(output_folder + 'output_data/nn_site_loc_clm_cen.csv', nn_site_loc, delimiter = ',')
np.savetxt(output_folder + 'output_data/grid_para_result_clm_cen.csv', grid_predict, delimiter = ',')
np.savetxt(output_folder + 'output_data/valid_grid_loc_clm_cen.csv', valid_grid_loc, delimiter = ',')

