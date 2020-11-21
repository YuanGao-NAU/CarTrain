from GeneralModel import GeneralModel
import numpy as np
import matplotlib.pyplot as plt

if __name__ == "__main__":

    B = np.array([0.4, 0.35, 0.25, 0, 0, 0, 0]).reshape([7,1])   # allocation 

    f41 = 0.7; f51 = 0.25; f42 = 0.65; f52 = 0.25; f43 = 0.15; f53 = 0.75;
    f64 = 0.3; f65 = 0.05; f75 = 0.04;

    A = np.array([-1, 0, 0, 0, 0, 0, 0,
                0, -1, 0, 0, 0, 0, 0,
                0, 0, -1, 0, 0, 0, 0,
                f41, f42, f43, -1, 0, 0, 0,
                f51, f52, f53, 0, -1, 0, 0,
                0, 0, 0, f64, f65, -1, 0,
                0, 0, 0, 0, f75, 0, -1]).reshape([7,7])   # tranfer 

    #turnover rate per day of pools:
    #leaf,root,wood, metabolic litter, structural litter,
    #fast SOM, passive SOM      
    temp = [0.0017, 0.002, 0.0001, 0.01, 0.001, 0.0001, 0.000001] 
                                                                                            
                                                                                        
    K = np.zeros(49).reshape([7, 7])

    for i in range(0, 7):
        K[i][i] = temp[i]

    #Unit of turnover rate from day^-1 to second^-1  
    #1 day = 86400 seconds
    K = np.multiply(K, 1/86400)  

    # Cinput_const, assume to be constant 
    input_fluxes = 0.00002245 #    

    nyear = 10000

    times = np.linspace(0, nyear*365*86400, num = nyear)

    iv_list = [0,0,0,0,0,0,0]

    mod = GeneralModel(times, B, A, K, iv_list, input_fluxes)

    res = mod.get_x()

    fig = plt.figure(6*2, figsize=(14, 7.68))
    plt.subplots_adjust(left  = 0.1, right = 0.95, bottom = 0.10, top = 0.9, wspace =0.2, hspace =0)
    x = list(range(1,nyear+1, 1))

    pool_names = ["foliage", "wood", "metabolic litter", "structural litter", "soil microbial", "slow soil", "passive soil"]

    for i in range(1, 4):
        for j in range(1, 4):
            if ((i-1) * 3 + j) > 7 :
                break
            ax = plt.subplot(3, 3, (i-1) * 3 + j)
            ax.plot(x, res[(i-1) * 3 + j - 1,:])
            plt.xlabel("year", fontsize = 12)
            plt.ylabel(pool_names[(i-1) * 3 + (j-1)] + " pool ($g C m^{-2}$)", fontsize = 12)
    plt.savefig(output_folder + "/result" + ".png", dpi = 500)
    plt.show()

    print(res[:,nyear-1])

    #mod.write_output("./output.csv")