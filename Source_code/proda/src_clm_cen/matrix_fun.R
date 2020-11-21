matrix_fun = function (kp, NPP_min, NPP_max, NPP_mean, altmax_Current_Indi, altmax_Lastyear_Indi, tscalar_indi, wscalar_indi, xio, xin) {
  
# kp = c(0.0390086024999619,	0.495852589607239,	0.156862199306488,	0.708438277244568,	0.496389031410217,	0.490545690059662,	0.467732012271881,	0.595383465290070,	0.201273083686829,	0.164519175887108,	0.489019215106964,	0.478766977787018,	0.483924388885498,	0.427478283643723,	0.0530362874269486,	0.461646556854248,	0.0280526746064425,	0.490527480840683,	0.486660271883011,	0.500131487846375,	0.294538438320160,	0.466051816940308,	0.441801667213440,	0.442415505647659,	0.454672306776047)
# 
# NPP_min = 55.9
# NPP_max = 151.6
# NPP_mean = 92.2231
# altmax_Current_Indi = 0.2974
# altmax_Lastyear_Indi = 0.6198 
# tscalar_indi = c(262.914841588338, 262.912482579549, 262.883142598470, 262.857393519084, 262.834057108561, 262.828102620443, 262.821913019816, 262.815472284953, 262.827524566650, 262.852219136556, 262.867498270671, 262.874276733398, 262.860902150472, 262.811635080973, 262.763277435303)
# wscalar_indi = c(-11.5796332348449, -11.5960581871060, -11.0243588082337, -10.9651540086449, -10.8701369819231, -10.8359593710241, -11.3317970476424, -15, -15, -14.9922681570053, -15, -15, -15, -15, -15) 
# 
# xio = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
# xin = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  
  ###################################################################
  ## assign parameter values
  ###################################################################
  kelvin_to_celsius = 273.15
  use_vertsoilc = 1  #whether or not use vertical maxing part
  nspools = 7  #number of pools if no vertical
  nspools_vr = 70 #number of pools if vertical
  nSoilLayer = 10  #number of soil layers
  nlevdecomp = 10
  days_per_year = 365
  secspday = 24*60*60
  #dt = secspday*30
  #width between two interfaces
  dz = c(0.0175128180000000, 0.0275789690000000, 0.0454700330000000, 0.0749674110000000, 
         0.123600365000000, 0.203782551000000, 0.335980626000000, 0.553938405000000, 
         0.913290032000000, 1.50576070100000, 2.48257969700000, 4.09308195300000, 
         6.74835127800000, 11.1261502900000, 13.8511521400000)
  #depth between two node
  dz_node = c(0.00710063500000000, 0.0208243650000000, 0.0343335740000000, 0.0566064930000000,
              0.0933283290000000, 0.153872401000000, 0.253692701000000, 0.418268552000000,
              0.689608259000000, 1.13697180500000, 1.87454959800000, 3.09060979600000,
              5.09555411000000, 8.40114844600000, 13.8511521400000)
  #depth of the interface
  zisoi = c(0.0175128180000000, 0.0450917870000000, 0.0905618200000000, 0.165529231000000,
            0.289129597000000, 0.492912148000000, 0.828892774000000, 1.38283117900000,
            2.29612121100000, 3.80188191200000, 6.28446160900000, 10.3775435600000,
            17.1258948400000, 28.2520451300000, 42.1031972800000)
  #depth of the node
  zsoi = c(0.00710063500000000, 0.0279250000000000, 0.0622585740000000, 0.118865067000000,
           0.212193396000000, 0.366065797000000, 0.619758498000000, 1.0380270500000,
           1.72763530900000, 2.86460711300000, 4.73915671100000, 7.82976650700000,
           12.9253206200000, 21.3264690600000, 35.1776212100000)
  
  xin = array(1, dim = c(nSoilLayer, 1))
  xio = array(1, dim = c(nSoilLayer, 1))
  
  #define parameter names
  bio = kp[1]*(5*10^(-4) - 3*10^(-5)) + 3*10^(-5)                         #diffusion (bioturbation?) (m2/yr)
  cryo = kp[2]*(16*10^(-4) - 3*10^(-5)) + 3*10^(-5)                            #cryoturbation (m2/yr)
  q10 = kp[3]*(3 - 1.2) + 1.2                                              #Q10 (unitless)
  fq10 = q10                                                           #Q10 when forzen (unitless)
  efolding = kp[4]*(1 - 0) + 0                                       #parameters used in vertical discretization of carbon inputs (metre)
  tau4cwd = kp[5]*(10 - 1) + 1                                       #turnover time of CWD (yr)
  tau4l1 = kp[6]*(0.2 - 0) + 0                                        #tau for metabolic litter (yr)
  tau4l2 = kp[7]*(1 - 0.2) + 0.2                                       #tau for cellulose litter (yr)
  tau4l3 = tau4l2                                    #tau for lignin litter (yr)
  tau4s1 = kp[8]*(1 - 0) + 0                                        #tau for fast SOC (yr)
  tau4s2 = kp[9]*(50 - 1) + 1                                      #tau for slow SOC (yr)
  tau4s3 = kp[10]*(2000 - 200) + 200                                 #tau for passive SOC (yr)
  
  fl1s1 = kp[11]*(0.8 - 0.2) + 0.2 
  fl2s1 = kp[12]*(0.8 - 0.2) + 0.2 
  fl3s2 = kp[13]*(0.8 - 0.2) + 0.2 
  fs1s2 = kp[14]*(0.4 - 0) + 0 
  fs1s3 = kp[15]*(0.03 - 0) + 0 
  fs2s1 = kp[16]*(0.6 - 0.1) + 0.1 
  fs2s3 = kp[17]*(0.1 - 0) + 0 
  fs3s1 = kp[18]*(1 - 0) + 0 
  fcwdl2 = kp[19]*(1 - 0.5) + 0.5 
  
  
  ins = kp[20]*(NPP_max - NPP_min)/NPP_mean + NPP_min/NPP_mean  #fraction of NPP input (unitless) for NPP (gC/m2/yr)
  beta = kp[21]*(0.9999 - 0.5) + 0.5     #Vegetation root distribution parameter for CWD (beta function) (unitless)
  dmax = 3.8    #maximum depth of root (the maximum depth that the NPP and be distributed) (metre)
  p4ll = kp[22]*(0.25 - 0) + 0             #allocation ratio from NPP to CWD (unitless)
  p4ml = kp[23]*(0.7 - 0.05) + 0.05     #allocation ratio from NPP to metabolic litter (unitless)
  p4cl = kp[24]*(0.5 - 0.01) + 0.01    #allocation ratio from NPP to cellulose litter (unitless)
  minpsi= -25          #minimum water potential (MPa)
  maxpsi= kp[25]*(0 - (-0.015)) + (-0.015)              #minimum water potential (MPa)
  adv = 0                       #parameter for advection (m/yr)
  
  p4cwd = 1 - (p4ll + p4ml + p4cl)        #allocation ratio from NPP to lignin litter (unitless)
  
  ###################################################################
  ## environmental scalars
  ###################################################################
  ## Environmental Scalar (Xi)
  #temperature related function xit
  #calculate rate constant scalar for soil temperature
  #assuming that the base rate constants are assigned for non-moisture
  #limiting conditions at 25 C.
  xit = array(NA, dim = c(nSoilLayer, 1))
  for (iSoilLayers in 1 : nSoilLayer) {
    if (tscalar_indi[iSoilLayers] >= 0 + kelvin_to_celsius) {
      xit[iSoilLayers] = q10**((tscalar_indi[iSoilLayers] - (kelvin_to_celsius + 25))/10)
    } else {
      xit[iSoilLayers] = q10**((273.15 - 298.15)/10)*(fq10**((tscalar_indi[iSoilLayers] - (0 + kelvin_to_celsius))/10))
    }
  }
  
  ## water related function xiw
  #calculate the rate constant scalar for soil water content.
  #Uses the log relationship with water potential given in
  #Andren, O., and K. Paustian, 1987. Barley straw decomposition in the field:
  #a comparison of models. Ecology, 68(5):1190-1200.
  #and supported by data in
  #Orchard, V.A., and F.J. Cook, 1983. Relationship between soil respiration
  #and soil moisture. Soil Biol. Biochem., 15(4):447-453.
  xiw = array(NA, dim = c(nSoilLayer, 1))
  #maxpsi = -(9.8*10^(-5))*exp((1.54 - 0.0095*sand + 0.0063*(100 - sand - clay))*log(10))
  for (iSoilLayers in 1:nSoilLayer) {
    if (wscalar_indi[iSoilLayers] > maxpsi) {
      xiw[iSoilLayers] = 1
    } else if (wscalar_indi[iSoilLayers] < minpsi) {
      xiw[iSoilLayers] = 0.001
    } else {
      xiw[iSoilLayers] = (log10(minpsi/wscalar_indi[iSoilLayers]))/(log10(minpsi/maxpsi))
    }
    #adapted from matrix ORCHIDEE
    xiw[iSoilLayers] = max(0.25, xiw[iSoilLayers])
  }
  
  ###################################################################
  ## A matrix
  ###################################################################
  a_ma_vr = diag(c(-array(1, dim = c(nspools_vr, 1))))
  fcwdl3 = 1 - fcwdl2
  for (j in 1:nlevdecomp) {
    transfer_fraction = c(fl1s1, fl2s1, fl3s2, fs1s2, fs1s3, fs2s1, fs2s3, fs3s1, fcwdl2, fcwdl3)
    a_ma_vr[(3-1)*nlevdecomp+j,(1-1)*nlevdecomp+j] = transfer_fraction[9]
    a_ma_vr[(4-1)*nlevdecomp+j,(1-1)*nlevdecomp+j] = transfer_fraction[10]
    a_ma_vr[(5-1)*nlevdecomp+j,(2-1)*nlevdecomp+j] = transfer_fraction[1]
    a_ma_vr[(5-1)*nlevdecomp+j,(3-1)*nlevdecomp+j] = transfer_fraction[2]
    a_ma_vr[(5-1)*nlevdecomp+j,(6-1)*nlevdecomp+j] = transfer_fraction[6]
    a_ma_vr[(5-1)*nlevdecomp+j,(7-1)*nlevdecomp+j] = transfer_fraction[8]
    a_ma_vr[(6-1)*nlevdecomp+j,(4-1)*nlevdecomp+j] = transfer_fraction[3]
    a_ma_vr[(6-1)*nlevdecomp+j,(5-1)*nlevdecomp+j] = transfer_fraction[4]
    a_ma_vr[(7-1)*nlevdecomp+j,(5-1)*nlevdecomp+j] = transfer_fraction[5]
    a_ma_vr[(7-1)*nlevdecomp+j,(6-1)*nlevdecomp+j] = transfer_fraction[7]        
  }
  a_ma = a_ma_vr
  
  ###################################################################
  ## decomposition matrix
  ###################################################################
  n_scalar = xin[1:nSoilLayer, 1]  # nitrogen, fpi
  t_scalar = xiw  # temperature
  w_scalar = xit  # water
  o_scalar = xio[1:nSoilLayer, 1]  # oxgen
  
  kl1 = 1/(days_per_year * tau4l1)
  kl2 = 1/(days_per_year * tau4l2)
  kl3 = 1/(days_per_year * tau4l3)
  ks1 = 1/(days_per_year * tau4s1)
  ks2 = 1/(days_per_year * tau4s2)
  ks3 = 1/(days_per_year * tau4s3)
  kcwd = 1/(days_per_year * tau4cwd)
  
  decomp_depth_efolding = efolding
  
  depth_scalar = exp(-zsoi/decomp_depth_efolding)
  xi_tw = t_scalar*w_scalar*o_scalar
  xi_tw = xi_tw[1:nSoilLayer, 1]
  
  kk_ma_vr = array(0, dim = c(nspools_vr, nspools_vr))
  
  for (j in 1:nSoilLayer) {
    # CWD exists only on the surface of land
    kk_ma_vr[j,j] = kcwd * xi_tw[j] * depth_scalar[j] 
    # other litters and SOC and be decomposed at each layer of the soil
    kk_ma_vr[1*nSoilLayer+j, 1*nSoilLayer+j] = kl1 * xi_tw[j] * depth_scalar[j]* n_scalar[j] 
    kk_ma_vr[2*nSoilLayer+j, 2*nSoilLayer+j] = kl2 * xi_tw[j] * depth_scalar[j]* n_scalar[j]
    kk_ma_vr[3*nSoilLayer+j, 3*nSoilLayer+j] = kl3 * xi_tw[j] * depth_scalar[j]* n_scalar[j] 
    kk_ma_vr[4*nSoilLayer+j, 4*nSoilLayer+j] = ks1 * xi_tw[j] * depth_scalar[j]
    kk_ma_vr[5*nSoilLayer+j, 5*nSoilLayer+j] = ks2 * xi_tw[j] * depth_scalar[j]
    kk_ma_vr[6*nSoilLayer+j, 6*nSoilLayer+j] = ks3 * xi_tw[j] * depth_scalar[j]
  }
  
  kk_ma = kk_ma_vr
  
  
  ###################################################################
  ## tri matrix
  ###################################################################
  som_adv_flux = adv
  
  max_altdepth_cryoturbation = 2.0  # not used only for clm version
  eps_def = 1e-30
  nbedrock=10
  
  # maximum depth of biotrubation and cryoturbation
  bioturbation_depth = 5 #2.
  max_cryoturb_alt = 5 #3.
  # minimum depth of cryoturbation
  min_cryoturb_alt = 0.01
  
  cryoturbation_method = 4
  
  # change the unit from m2/yr to m2/s
  diff_k_const     = cryo/(days_per_year)
  bio_diff_k_const = bio/(days_per_year)
  
  # define the depth of cryoturbation
  cryoturbation_depth = altmax_Lastyear_Indi
  
  # if 0.01m < alt < 3m, the cryoturbation is the dominate
  cryoturb_location =  ( altmax_Lastyear_Indi <  max_cryoturb_alt ) & ( altmax_Lastyear_Indi > min_cryoturb_alt )
  # if alt > 3m, the bioturbation is the dominate
  bioturb_location =  ( altmax_Lastyear_Indi > max_cryoturb_alt ) 
  # when the cyroturbation is dominate
  if ((altmax_Lastyear_Indi <  max_cryoturb_alt ) & ( altmax_Lastyear_Indi > min_cryoturb_alt )) {
    
    som_diffus_coef = array(NA, dim = c(nSoilLayer, 1))
    for (il in 1:nSoilLayer) { # linear dropoff to zero between alt and 3*alt
      if ( zsoi[il] < cryoturbation_depth ) {
        som_diffus_coef[il] = diff_k_const
      } else {
        som_diffus_coef[il] = diff_k_const*(1.0-max(min((zsoi[il]-cryoturbation_depth) / (2.*cryoturbation_depth),1.0),0.0));
      }
      #  max_cryoturb_alt = 3m
      if ( zsoi[il] > max_cryoturb_alt ) {
        som_diffus_coef[il] = 0.0;
      }
    } 
    
  } else if ( altmax_Lastyear_Indi > max_cryoturb_alt ) {
    # when the bioturbation is dominate
    som_diffus_coef = array(NA, dim = c(nSoilLayer, 1))
    for (il in 1:nSoilLayer) {
      # the maximum depth of bioturbation is 2.0m (bioturbation_depth = 2)
      if (zsoi[il] < bioturbation_depth) {
        som_diffus_coef[il] = bio_diff_k_const
      } else {
        som_diffus_coef[il] = 0
      }
    }
  } else {
    som_diffus_coef = array(NA, dim = c(nSoilLayer, 1))
    for (il in 1:nSoilLayer) {
      som_diffus_coef[il] = 0
    }
  }
  
  
  
  som_adv_coef = array(som_adv_flux, dim = c(nSoilLayer, 1))
  #==============================================================================
  dz_node = array(NA, dim = c(nSoilLayer, 1))
  dz_node[1] = zsoi[1]
  
  for (j in 2:nSoilLayer) {
    dz_node[j]= zsoi[j] - zsoi[j-1]
  }
  
  # then minimal advective rate and diffusion rate is eps_def = 1e-30
  adv_flux = array(NA, dim = c(nSoilLayer, 1))
  diffus = array(NA, dim = c(nSoilLayer, 1))
  
  for (j in 1:nSoilLayer) {
    if ( abs(som_adv_coef[j])  < eps_def) {
      adv_flux[j] = eps_def
    } else {
      adv_flux[j] = som_adv_coef[j]
    }
    
    if ( abs(som_diffus_coef[j])  < eps_def ) {
      diffus[j] = eps_def
    } else {
      diffus[j] = som_diffus_coef[j]
    }
  }
  
  
  d_m1_zm1 = array(NA, dim = c(nSoilLayer, 1))
  d_p1_zp1 = array(NA, dim = c(nSoilLayer, 1))
  f_m1 = array(NA, dim = c(nSoilLayer, 1))
  f_p1 = array(NA, dim = c(nSoilLayer, 1))
  pe_m1 = array(NA, dim = c(nSoilLayer, 1))
  pe_p1 = array(NA, dim = c(nSoilLayer, 1))
  
  for (j in 1:nSoilLayer) {
    # Calculate the D and F terms in the Patankar algorithm
    if (j == 1) {
      d_m1_zm1[j] = 0.
      w_p1 = (zsoi[j+1] - zisoi[j]) / dz_node[j+1]
      if ( diffus[j+1] > 0. && diffus[j] > 0. ) {
        d_p1 = 1.  / ((1.  - w_p1) / diffus[j] + w_p1 / diffus[j+1]) # Harmonic mean of diffus
      } else {
        d_p1 = 0.
      }
      d_p1_zp1[j] = d_p1 / dz_node[j+1]
      f_m1[j] = adv_flux[j]  # Include infiltration here
      f_p1[j] = adv_flux[j+1]
      pe_m1[j] = 0.
      pe_p1[j] = f_p1[j] / d_p1_zp1[j] # Peclet #
      
    } else if (j >= nbedrock) {
      # At the bottom, assume no gradient in d_z (i.e., they're the same)
      w_m1 = (zisoi[j-1] - zsoi[j-1]) / dz_node[j]
      if ( diffus[j] > 0. && diffus[j-1] > 0. ) {
        d_m1 = 1.  / ((1.  - w_m1) / diffus[j] + w_m1 / diffus[j-1]) # Harmonic mean of diffus
      } else {
        d_m1 = 0.
      }
      d_m1_zm1[j] = d_m1 / dz_node[j]
      d_p1_zp1[j] = d_m1_zm1[j] # Set to be the same
      f_m1[j] = adv_flux[j]
      #f_p1(j) = adv_flux(j+1)
      f_p1[j] = 0.
      pe_m1[j] = f_m1[j] / d_m1_zm1[j] # Peclet #
      pe_p1[j] = f_p1[j] / d_p1_zp1[j] # Peclet #
    } else {
      # Use distance from j-1 node to interface with j divided by distance between nodes
      w_m1 = (zisoi[j-1] - zsoi[j-1]) / dz_node[j]
      if ( diffus[j-1] > 0. && diffus[j] > 0. ) {
        d_m1 = 1.  / ((1.  - w_m1) / diffus[j] + w_m1 / diffus[j-1]) # Harmonic mean of diffus
      } else {
        d_m1 = 0.
      }
      w_p1 = (zsoi[j+1] - zisoi[j]) / dz_node[j+1]
      if ( diffus[j+1] > 0.  && diffus[j] > 0. ) {
        d_p1 = 1.  / ((1.  - w_p1) / diffus[j] + w_p1 / diffus[j+1]) # Harmonic mean of diffus
      } else {
        d_p1 = (1.  - w_m1) * diffus[j] + w_p1 * diffus[j+1] # Arithmetic mean of diffus
      }
      d_m1_zm1[j] = d_m1 / dz_node[j]
      d_p1_zp1[j] = d_p1 / dz_node[j+1]
      f_m1[j] = adv_flux[j]
      f_p1[j] = adv_flux[j+1]
      pe_m1[j] = f_m1[j] / d_m1_zm1[j] # Peclet #
      pe_p1[j] = f_p1[j] / d_p1_zp1[j] # Peclet #
    }
  }
  

  # Calculate the tridiagonal coefficients
  a_tri_e = array(NA, dim = c(nSoilLayer, 1))
  c_tri_e = array(NA, dim = c(nSoilLayer, 1))
  b_tri_e = array(NA, dim = c(nSoilLayer, 1))
  
  a_tri_dz = array(NA, dim = c(nSoilLayer, 1))
  c_tri_dz = array(NA, dim = c(nSoilLayer, 1))
  b_tri_dz = array(NA, dim = c(nSoilLayer, 1))

    for (j in 0:nSoilLayer + 1) {
    if (j == 0)  {# top layer (atmosphere)
      #a_tri(j) = 0.
      #b_tri(j) = 1.
      #c_tri(j) = -1.
      #b_tri_e(j) = b_tri(j)
    } else if (j == 1) {
      aaa_m1 = max (0. , (1.  - 0.1  * abs(pe_m1[j]))**5)  # A function from Patankar, Table 5.2, pg 95
      aaa_p1 = max (0. , (1.  - 0.1 * abs(pe_p1[j]))**5)  # A function from Patankar, Table 5.2, pg 95
      a_tri_e[j] = -(d_m1_zm1[j] * aaa_m1 + max( f_m1[j], 0. )) # Eqn 5.47 Patankar
      c_tri_e[j] = -(d_p1_zp1[j] * aaa_p1 + max(-f_p1[j], 0. ))
      b_tri_e[j] = -a_tri_e[j] - c_tri_e[j]
    } else if (j < nSoilLayer+1) {
      aaa_m1 = max (0. , (1.  - 0.1  * abs(pe_m1[j]))**5)  # A function from Patankar, Table 5.2, pg 95
      aaa_p1 = max (0. , (1.  - 0.1 * abs(pe_p1[j]))**5)  # A function from Patankar, Table 5.2, pg 95
      a_tri_e[j] = -(d_m1_zm1[j] * aaa_m1 + max( f_m1[j], 0. )) # Eqn 5.47 Patankar
      c_tri_e[j] = -(d_p1_zp1[j] * aaa_p1 + max(-f_p1[j], 0. ))
      b_tri_e[j] = -a_tri_e[j] - c_tri_e[j]
      
    } else { # j==nSoilLayer+1 0 concentration gradient at bottom
      #a_tri(j) = -1.
      #b_tri(j) = 1.
      #c_tri(j) = 0.
    }
    
  }
  
  
  for (il in 1:nSoilLayer) {
    a_tri_dz[il] = a_tri_e[il] / dz[il]
    b_tri_dz[il] = b_tri_e[il] / dz[il]
    c_tri_dz[il] = c_tri_e[il] / dz[il]
  }
  
  tri_matrix_vr = array(0, dim = c(nspools_vr, nspools_vr))
  zerodiffus = 10
  
  for (i  in 1:6){
    for (il in 1:nSoilLayer) {
      tri_matrix_vr[il+(i-1)*nSoilLayer+zerodiffus,il+(i-1)*nSoilLayer+zerodiffus] = b_tri_dz[il]
      
      if (il == 1) {   # upper boundary
        tri_matrix_vr[1+(i-1)*nSoilLayer+zerodiffus,1+(i-1)*nSoilLayer+zerodiffus] = -c_tri_dz[1]
      }
      if (il == nSoilLayer) {  # bottom boundary
        tri_matrix_vr[nSoilLayer+(i-1)*nSoilLayer+zerodiffus,nSoilLayer+(i-1)*nSoilLayer+zerodiffus] = -a_tri_dz[nSoilLayer]
      }
      
      if (il < nSoilLayer) {    # avoid tranfer from for example, litr3_10th layer to soil1_1st layer
        tri_matrix_vr[il+(i-1)*nSoilLayer+zerodiffus,il+1+(i-1)*nSoilLayer+zerodiffus] = c_tri_dz[il]
      }
      
      if (il > 1) {  # avoid tranfer from for example, soil1_1st layer to litr3_10th layer
        tri_matrix_vr[il+(i-1)*nSoilLayer+zerodiffus,il-1+(i-1)*nSoilLayer+zerodiffus] = a_tri_dz[il]
      }
    }
  } #end of tridiagonal matrix
  
  tri_ma = tri_matrix_vr
  
  
  ###################################################################
  ## B Matrix
  ###################################################################
  vertical_input = array(NA, dim = c(nSoilLayer, 1))
  if (altmax_Lastyear_Indi > 0) {
    for (j in 1:nSoilLayer) {
      if (zisoi[j] < dmax && (zisoi[j]-dz[j]) < dmax) {
        vertical_input[j, 1] = (beta**((zisoi[j]-dz[j])*100) - beta**(zisoi[j]*100))/dz[j] #integral(rooting_fun_l3, zisoi(j)-dz(j), zisoi(j))
      } else if (zisoi[j] > dmax && (zisoi[j]-dz[j]) > dmax) {
        vertical_input[j, 1] = 0
      } else {
        vertical_input[j, 1] = (beta**((zisoi[j]-dz[j])*100) - beta**(dmax*100))/dz[j] #integral(rooting_fun_l3, zisoi(j)-dz(j), dmax)
      }
    }
  } else {
    vertical_input[1] = 1/dz[1]
    vertical_input[2:length(vertical_input)] = 0
  }
  
    
    vertical_input = dz[1:nSoilLayer]*vertical_input/sum(vertical_input*dz[1:nSoilLayer])
    
    
    ###################################################################
    ## Steady State Calculation
    ###################################################################
    
    matrix_in = array(NA, dim = c(70,1))
    #unit of NPP: gC/m2/yr
    matrix_in[1:10,1] = p4cwd*ins*NPP_mean*vertical_input/(dz[1:nSoilLayer]*days_per_year) #litter input gc/m3/day
    matrix_in[11:20,1] = p4ml*ins*NPP_mean*vertical_input/(dz[1:nSoilLayer]*days_per_year)
    matrix_in[21:30,1] = p4cl*ins*NPP_mean*vertical_input/(dz[1:nSoilLayer]*days_per_year)
    matrix_in[31:40,1] = p4ll*ins*NPP_mean*vertical_input/(dz[1:nSoilLayer]*days_per_year)
    matrix_in[41:70,1] = 0
    
    #if cond(a_ma*kk_ma-tri_ma) < 1e14
    #unit of output: gC/m3
    PoolC = solve((a_ma%*%kk_ma-tri_ma), (-matrix_in))
    
    TotSOC = sum((PoolC[41:50]+PoolC[51:60]+PoolC[61:70])*dz[1:nSoilLayer])
    LayerC = cbind(PoolC[41:50], PoolC[51:60], PoolC[61:70])
    LayerC = cbind(LayerC, rowSums(LayerC[ , c(1:3)]))
    
    matrix_fun_out = list(TotSOC = TotSOC, LayerC = LayerC, PoolC = PoolC)
    
    return(matrix_fun_out)
}


