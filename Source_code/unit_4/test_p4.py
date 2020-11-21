from GeneralModel import GeneralModel
import numpy as np
import matplotlib.pyplot as plt
import sys

if __name__ == '__main__':

    output_folder = sys.argv[1]
    
    B = np.array([0.45, 0.55, 0, 0, 0, 0, 0]).reshape([7,1])   # allocation 

    f31 = 0.72; f41 = 0.28; f42 = 1; f53 = 0.45; f54 = 0.275; f64 = 0.275;
    f65 = 0.296; f75 = 0.004; f56 = 0.42; f76 = 0.03; f57 = 0.45;

    A = np.array([-1, 0, 0, 0, 0, 0, 0,
                0, -1, 0, 0, 0, 0, 0,
                f31, 0, -1, 0, 0, 0, 0,
                f41, f42, 0, -1, 0, 0, 0,
                0, 0, f53, f54, -1, f56, f57,
                0, 0, 0, f64, f65, -1, 0,
                0, 0, 0, 0, f75, f76, -1]).reshape([7,7])   # tranfer 


    #turnover rate per day of pools: foliage, wood, metabolic litter, structural
    #litter, soil microbial,slow soil, passive soil      
    temp = [0.00176, 0.000100104, 0.021468, 0.000845, 0.008534, 8.976e-005, 0.00000154782]

    K = np.zeros(49).reshape([7, 7])

    for i in range(0, 7):
        K[i][i] = temp[i]
        
    #Unit of turnover rate from day^-1 to second^-1  
    #1 day = 86400 seconds
    K = np.multiply(K, 1/86400)  

    # Cinput_const
    input_fluxes = 0.00002245 #    

    nyear = 10000   # number of simulation years 

    times = np.linspace(0, nyear*365*86400, num = nyear)

    iv_list = [0,0,0,0,0,0,0]

    mod = GeneralModel(times, B, A, K, iv_list, input_fluxes)

    res = mod.get_x()

    fig = plt.figure(4*3, figsize=(14, 7.68))
    plt.subplots_adjust(left  = 0.1, right = 0.95, bottom = 0.10, top = 0.9, wspace =0.3, hspace =0.4)
    x = list(range(1,nyear + 1, 1))

    df = mod.get_x_df()

    pool_names = ["foliage", "wood", "metabolic litter", "structural litter", "soil microbial", "slow soil", "passive soil"]

    for i in range(1, 5):
        for j in range(1, 4):
            if ((i-1) * 3 + j) > 7 :
                break
            ax = plt.subplot(4, 3, (i-1) * 3 + j)
            ax.plot(x, res[(i-1) * 3 + j - 1,:])
            plt.xlabel("year", fontsize = 8)
            plt.ylabel(pool_names[(i-1) * 3 + (j-1)] + " pool ($g C m^{-2}$)", fontsize = 8)
    ax = plt.subplot(4, 3, 8)
    ax.plot(x, df["C_input"], label = "carbon input")
    plt.xlabel("year", fontsize = 8)
    plt.ylabel("C input g C $s^{-1}$", fontsize = 8)
    
    ax = plt.subplot(4, 3, 9)
    ax.plot(x, df["Tres"]/86400/365, label = "residence time")
    plt.xlabel("year", fontsize = 8)
    plt.ylabel("year", fontsize = 8)

    ax = plt.subplot(4, 3, 10)
    ax.plot(x, df["X"], color = "blue", label = "carbon storage")
    ax.plot(x, df["Xc"], color = "red", label = "carbon storage capacity")
    plt.xlabel("year", fontsize = 8)
    plt.ylabel("g C $m^{-2}$", fontsize = 8)
    plt.legend()
    plt.savefig(output_folder + "/result" + ".png", dpi = 500)
    #plt.show()

    print(res[:,nyear-1])   # print result of the last year

    mod.write_output(output_folder + "/output.csv")