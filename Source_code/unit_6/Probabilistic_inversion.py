"""
Author: Xin Huang
Time: May 28,2020
This program employs Metropolis-Hastings algorithm for data assimilation on TECO model with ambient and elevated CO2 treatments.
Please refer to Xu et al., (2006) for detailed information
"""
import numpy as np
import sys

def temp_mois():
   """Generate environmental factors based on temperature and moisture
   Global args: temp, mois, Nt"""
   tau = []
   mscut = 0.2
   for i in range(Nt + 1):
      tmp = temp[i]
      if (i > 9):
         tmp=sum(temp[range(i-9,i+1)])/10
      tmp = 0.65 * 2.2 ** ((tmp - 10) / 10)
      moisture = 1
      if (mois[i] < mscut):
         moisture = 1.0 - 5.0 * (mscut - mois[i])
      tau.append(tmp * moisture)
   return tau


def GenerateParamValues(c_op):
   """Generate new parameter values based on eigvalue and eigvectors
   Global args: eigD, eigV, cmin, cmax, paramNum"""
   flag = True
   while (flag):
      randVector = np.random.randn(paramNum) # Normally distributed pseudorandom numbers
      cT = randVector * np.sqrt(eigD)
      cNew = np.dot(eigV, (np.dot(eigV.T, c_op) + cT))
      if (isQualified(cNew)):
         flag = False
   return cNew

def isQualified(c):
   """Decide whether the new parameter values exist in [cmin, cmax] value interval
   Global args: paramNum, cmin, cmax"""
   flag = True
   for i in range(paramNum):
      if(c[i] > cmax[i] or c[i] < cmin[i]):
         flag = False
         break
   return flag

## Step 3: TECO model
def run_model(c):
   """Forward run model.
   Global args: tau, b, u, x0, Nt, cbnScale"""
   ## x stores 7 simulated carbon pools simulated in 5 years (daily scale)
   x=np.zeros([7,Nt+1], dtype=float)
   ## multiple matrix A and diagnol matrix  C
   AC = np.matrix([[-c[0], 0, 0, 0, 0, 0, 0],
         [0, -c[1], 0, 0, 0, 0, 0],
         [0.7123 * c[0], 0, -c[2], 0, 0, 0, 0],
         [0.2877 * c[0], c[1], 0, -c[3], 0, 0, 0],
         [0, 0, 0.45 * c[2], 0.275 * c[3], -c[4], 0.42 * c[5], 0.45 * c[6]],
         [0, 0, 0, 0.275 * c[3], 0.296 * c[4], -c[5], 0],
         [0, 0, 0, 0, 0.004 * c[4], 0.03 * c[5], -c[6]]])
   x[:,0] = x0
   ## simulate for 5 years
   for i in range(1,Nt+1):
      ## Eq. 1 in Xu et al., (2006)
      x[:,i] = np.dot((np.eye(7) + AC * tau[i-1]), x[:,i-1]) + np.asarray(b) * u[i-1] * cbnScale
      
   ## simulated carbon pools with 7 rows and Nt columns
   xsimu=x[:,range(1,Nt+1)]
   ## use mapping function to covert 7 carbon pools to 6 simulated data sets
   ## get yearly simulated litterfall
   litterDaily=tau[:-1] * np.dot(phi_litterfall, xsimu)
   litterYr = [sum(litterDaily[range((i-1)*365, i*365)]) for i in range(1,6)]
   ## get simulated soilResp
   soilTime=soilResp[0,:].astype(int)-1 # convert float to int. In python, index starts from 0.
   soilResp_simu=np.asarray(tau)[soilTime] * np.dot(phi_slResp, xsimu[:,(soilTime)])+0.25 * (1 - b[0] - b[1]) * u[soilTime]
   ## get simulated woody biomass
   woodyTime=woody[0,:].astype(int)-1
   woody_simu = np.dot(phi_woodBiom, xsimu[:, woodyTime])
   ## get simulated foliage biomass
   foliageTime=foliage[0,:].astype(int)-1
   foliage_simu=np.dot(phi_foilageBiom, xsimu[:, foliageTime])
   ## get simulated Forestfloor biomass
   forestFloorTime = forestFloor[0, :].astype(int) - 1
   forestFloor_simu = np.dot(phi_cForestFloor, xsimu[:, forestFloorTime])
   ## get simulated ForestMinearal biomass
   cMineralTime = forestMineral[0, :].astype(int) - 1
   forestMineral_simu = np.dot(phi_cMineral, xsimu[:, cMineralTime])
   return [woody_simu, foliage_simu, litterYr, forestFloor_simu, forestMineral_simu, soilResp_simu]

def getBest():
   """Get the best simulation after MH sampling.
   Global args: c_record, J_record, record, tau, b, u, x0, Nt, cbnScale"""
   bestId = np.where(J_record == np.min(J_record[0:record]))
   bestC = c_record[:, bestId[0][0]]
   bestSimu=run_model(bestC)
   return [bestC, bestSimu]

def write_io_file(outDir):
   """write outputs to files.
   Global args: c_record, J_record, record, bestC, bestSimu"""
   np.savetxt(outDir+'/mismatch_accepted.txt', J_record[1:record])
   np.savetxt(outDir+'/param_accepted.txt', c_record[:, 1:record])
   np.savetxt(outDir+'/accepted_num.txt', [record])
   np.savetxt(outDir+'/bestParam.txt', bestC)
   np.savetxt(outDir+'/Woody_bestSimu.txt',bestSimu[0])
   np.savetxt(outDir+'/Foliage_bestSimu.txt',bestSimu[1])
   np.savetxt(outDir+'/Litterfall_bestSimu.txt',bestSimu[2])
   np.savetxt(outDir+'/Forestfloor_bestSimu.txt',bestSimu[3])
   np.savetxt(outDir+'/ForestMineral_bestSimu.txt',bestSimu[4])
   np.savetxt(outDir+'/SoilResp_bestSimu.txt',bestSimu[5])

if __name__ == '__main__':
   """startpoint of program. This is the global namespace. All variables defined here are accessable in functions defined above.
   Notation: If you want to change these global variables, using 'global' keyword to clarify their global attribute firstly."""
   ## namelist.txt indicates CO2 treatment and parameter range file
   #f=open('./Source_code/unit_6/namelist.txt','r')
   #line1=f.readline() ## get the 1st line in namelist.txt file
   ## 1 -- ambient inversion, 2 -- elevated inversion
   #ninput=int(line1.split('!')[0].split('=')[1]) ## Firstly, split the string by '!' and choose the frist part, \
      # then split string by '=', select the second part, . Afterward, convert to int type
   #line2=f.readline() ## get the 2nd line in namelist.txt file
   ## 'ParamRange.txt' in the first practice, 'newParamRange.txt' in the second practice
   #paramRangeFile=line2.split('!')[0].split('=')[1][:-1] ## [:-1] is to remove the '/n' at the end of the string
   paramRangeFile="./Source_code/unit_6/input/" + sys.argv[2]
   #line3=f.readline()
   #outDir=line3.split('!')[0].split('=')[1][:-1] ## get the 3rd line in namelist.txt file
   ninput = int(sys.argv[1])
   outDir = sys.argv[3]

   Nt=1825 # simulation step: daily step in 5 years
   ## initialize Cpool-related variables
   cbnScale = 2 if ninput ==1 else 1.5 # affect C input flux
   x0=[469, 4100, 64, 694, 123, 1385, 923] # initial value of states
   b=[0.25, 0.30, 0, 0, 0, 0, 0] # C input
   u=np.loadtxt('./Source_code/unit_6/input/CarbonInput.txt')[ninput,]  #1st row is time, 2nd row and 3nd row are observations

   ## initialize parameter-related varibles
   param=np.loadtxt(paramRangeFile) ## read Parameter Range from file
   cmin=param[0,]
   cmax=param[1,]
   c=param[1+ninput,]
   cRange  = (cmax-cmin)
   paramNum=np.shape(param)[1]              #number of parameters
   ## Prior estimate of covariance matrix of parameters (from previous uniform run)
   cov_c=np.loadtxt('./Source_code/unit_6/input/Cov_C_Init.txt')[range(7*(ninput-1),7*ninput),] # the first 7 rows for ninput=1, second 7 rows for ninput=2
   ## eigD is eigenvalue, eigV is eigenvectors
   if ninput==1:
      eigD=np.loadtxt('./Source_code/unit_6/input/eigD_amb.txt')
      eigV=np.loadtxt('./Source_code/unit_6/input/eigV_amb.txt')
   else:
      eigD = np.loadtxt('./Source_code/unit_6/input/eigD_ele.txt')
      eigV = np.loadtxt('./Source_code/unit_6/input/eigV_ele.txt')
   ## initialize environmental factor
   temp = np.loadtxt('./Source_code/unit_6/input/temperature.txt')[1,] # dimention is (2,(Nt+1)), 1st row is time, 2nd row is observation
   mois = np.loadtxt('./Source_code/unit_6/input/moisture.txt')[1,] # dimention is (2,Nt), 1st row is time, 2nd row is observation
   tau = temp_mois()

   ## Step 2: 6 Data Sets and their variances
   ## initialize observation-related varibles
   soilResp=np.loadtxt('./Source_code/unit_6/input/SoilRespiration.txt')[[0,ninput],] #1st row is time, 2nd row and 3nd row are observations
   woody=np.loadtxt('./Source_code/unit_6/input/Woody.txt')[[0,ninput],] #1st row is time, 2nd row and 3nd row are observations
   foliage=np.loadtxt('./Source_code/unit_6/input/Foliage.txt')[[0,ninput],] #1st row is time, 2nd row and 3nd row are observations
   litterfall=np.loadtxt('./Source_code/unit_6/input/Litterfall.txt')[[0,ninput],] #1st row is time, 2nd row and 3nd row are observations, yearly
   forestFloor=np.loadtxt('./Source_code/unit_6/input/ForestFloor.txt')[[0,ninput],] #1st row is time, 2nd row and 3nd row are observations
   forestMineral=np.loadtxt('./Source_code/unit_6/input/ForestMineral.txt')[[0,ninput],] #1st row is time, 2nd row and 3nd row are observations
   ## collect the 6 data sets into an observation list
   obsList=[woody[1,],foliage[1,],litterfall[1,],forestFloor[1,],forestMineral[1,],soilResp[1,]]
   ## variance of 6 observations
   varList=[np.var(obs, ddof=1) for obs in obsList] # ddof=1 non-bias estimator for sample variance

   ## The mappings
   phi_slResp          = [0.25*c[0], 0.25*c[1],  0.55*c[2],   0.45*c[3],   0.7*c[4],    0.55*c[5],   0.55*c[6]]
   phi_woodBiom        = [0,         1,         0,         0,         0,        0,         0]
   phi_foilageBiom     = [0.75,       0,         0,         0,         0,        0,         0]
   phi_litterfall      = [0.75*c[0], 0.75*c[1], 0,         0,         0,        0,         0]
   phi_cMineral        = [0,         0,         0,         0,         1,        1,         1]
   phi_cForestFloor    = [0,         0,         0.75,      0.75,      0,        0,         0]

   ## initialize MCMC-related variables
   nsimu    = 20000 # the number of iterations
   record=0 #the number of accepted parameter values in the sampling series
   c_record = np.zeros((paramNum, nsimu + 1), dtype=float) # save accepted parameter values
   c_record[:, 0] = c
   J_record = np.zeros(nsimu + 1, dtype=float) # save corresponding simulation error
   J_record[0] = 300000

   ## Step 5: Metropolis-Hasting sampling algorithm
   ## the start of Step 5
   for simu in range(nsimu):
      ## Proposing step: generate a new set of parameter values based on current accepted parameter values
      c_new = GenerateParamValues(c_record[:, record])

      ## only update 2 mapping when new parameter values generate
      phi_slResp = [0.25 * c_new[0],  0.25 * c_new[1],  0.55 * c_new[2],   0.45 * c_new[3],   0.7 * c_new[4],    0.55 * c_new[5], 0.55 * c_new[6]]
      phi_litterfall = [0.75 * c_new[0], 0.75 * c_new[1], 0,         0,         0,        0,         0]
      ## Step 3: TECO model
      ## running model, return [woody_simu, foliage_simu, litterYr, forestFloor_simu, forestMineral_simu, soilResp_simu]
      simuList = run_model(c_new)

      ## Step 4: Cost Function
      J_new=sum([sum((simuList[i] - obsList[i])**2) / (2 * varList[i]) for i in range(6)])
      delta_J = J_record[record] - J_new
      ## Moving step: to decide whether the new set of parameter values will be accepted or not
      randNum = np.random.uniform(0, 1, 1)
      if (min(1.0, np.exp(delta_J)) > randNum):
         ## accept the new set of parameter values and update relevant variables
         record += 1
         c_record[:, record] = c_new
         J_record[record] = J_new
         ## print out the acceptance rate
         print('simu=' + str(simu) + ' accepted=' + str(record))
   ## the end of Step5: Metropolish-Hasting sampling algorith
   ## Step 6: Estimated Parameters and Step7: Prediction
   ## get the optimal parameter values bestC and the corresponding best model outputs using these parameter values
   [bestC, bestSimu] = getBest()
   ## write relevant variables (i.e., record, J_record, c_record, bestC, bestSimu) into files
   write_io_file(outDir)
